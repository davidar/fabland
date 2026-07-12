! fl_input — evdev pointer devices for the drm backend. Raw reads of
! struct input_event from /dev/input/event*, classified with EVIOCGBIT
! ioctls into mice (EV_REL) and touchpads (EV_ABS + BTN_TOUCH), and
! translated into the same tev events the terminal backend produces.
! No libinput: a mouse is a stream of 24-byte records if you ask nicely.
module fl_input
  use iso_c_binding
  use fl_libc
  use fl_term, only: tev, TK_MOTION, TK_PRESS, TK_RELEASE, TK_WHEEL
  implicit none
  private
  public :: inp_open_all, inp_count, inp_fd, inp_read, inp_tick, inp_describe

  integer, parameter :: MAXDEV = 8

  ! ioctl request codes (x86_64, computed from _IOC in linux/input.h)
  integer(c_long), parameter :: IOC_GBIT_EV  = int(z'80084520', c_long) ! EVIOCGBIT(0, 8)
  integer(c_long), parameter :: IOC_GBIT_KEY = int(z'80604521', c_long) ! EVIOCGBIT(EV_KEY, 96)
  integer(c_long), parameter :: IOC_GBIT_REL = int(z'80084522', c_long) ! EVIOCGBIT(EV_REL, 8)
  integer(c_long), parameter :: IOC_GBIT_ABS = int(z'80084523', c_long) ! EVIOCGBIT(EV_ABS, 8)
  integer(c_long), parameter :: IOC_GABS_X   = int(z'80184540', c_long) ! EVIOCGABS(ABS_X)
  integer(c_long), parameter :: IOC_GABS_Y   = int(z'80184541', c_long)
  integer(c_long), parameter :: IOC_GNAME    = int(z'80404506', c_long) ! EVIOCGNAME(64)

  integer, parameter :: EV_KEY = 1, EV_REL = 2, EV_ABS = 3
  integer, parameter :: REL_X = 0, REL_Y = 1, REL_WHEEL = 8
  integer, parameter :: ABS_X = 0, ABS_Y = 1
  integer, parameter :: BTN_LEFT = 272, BTN_RIGHT = 273, BTN_MIDDLE = 274
  integer, parameter :: BTN_TOUCH = 330

  type, bind(c) :: absinfo_t
    integer(c_int32_t) :: value = 0, minimum = 0, maximum = 0
    integer(c_int32_t) :: fuzz = 0, flat = 0, resolution = 0
  end type

  interface
    integer(c_int) function c_ioctl_p(fd, req, arg) bind(c, name='ioctl')
      import :: c_int, c_long, c_ptr
      integer(c_int), value :: fd
      integer(c_long), value :: req
      type(c_ptr), value :: arg
    end function
  end interface

  integer :: ndev = 0
  integer(c_int) :: fds(MAXDEV) = -1
  logical :: is_pad(MAXDEV) = .false.
  integer :: rngx(MAXDEV) = 1, rngy(MAXDEV) = 1     ! touchpad abs ranges
  character(80) :: names(MAXDEV) = ' '
  ! touchpad state: contact tracking + tap-to-click
  logical :: touching(MAXDEV) = .false., have_last(MAXDEV) = .false.
  integer :: lax(MAXDEV) = 0, lay(MAXDEV) = 0
  integer(8) :: tdown(MAXDEV) = 0
  real :: tmoved(MAXDEV) = 0.0
  real :: frx(MAXDEV) = 0.0, fry(MAXDEV) = 0.0      ! sub-pixel remainders
  ! tap-and-drag: a tap emits press only; the release is deferred so a
  ! quick second touch can continue it as a drag (like libinput)
  logical :: tap_held(MAXDEV) = .false.    ! deferred release pending
  logical :: tap_drag(MAXDEV) = .false.    ! second touch is dragging
  integer(8) :: tap_at(MAXDEV) = 0         ! wall-clock ms of the tap

contains

  integer function inp_count()
    inp_count = ndev
  end function

  integer(c_int) function inp_fd(i)
    integer, intent(in) :: i
    inp_fd = fds(i)
  end function

  function inp_describe(i) result(s)
    integer, intent(in) :: i
    character(96) :: s
    s = trim(names(i))
    if (is_pad(i)) then
      s = trim(s)//' (touchpad)'
    else
      s = trim(s)//' (mouse)'
    end if
  end function

  logical function tbit(buf, n)
    integer(c_int8_t), intent(in) :: buf(:)
    integer, intent(in) :: n
    tbit = .false.
    if (n / 8 + 1 <= size(buf)) &
      tbit = btest(iand(int(buf(n / 8 + 1)), 255), mod(n, 8))
  end function

  ! Probe FABLAND_INPUT_DEV if set (no ioctls — lets a FIFO stand in for
  ! hardware; a ':pad' suffix makes it a touchpad with abs range 0..1000),
  ! else scan /dev/input/event0..31.
  subroutine inp_open_all(explicit)
    character(*), intent(in) :: explicit
    character(32) :: path
    integer :: n
    n = len_trim(explicit)
    if (n > 4) then
      if (explicit(n-3:n) == ':pad') then
        call try_dev(explicit(1:n-4), .true.)
        if (ndev > 0) then
          is_pad(ndev) = .true.
          rngx(ndev) = 1000; rngy(ndev) = 1000
        end if
        return
      end if
    end if
    if (n > 0) then
      call try_dev(trim(explicit), .true.)
      return
    end if
    do n = 0, 31
      if (ndev >= MAXDEV) return
      write(path, '(a,i0)') '/dev/input/event', n
      call try_dev(trim(path), .false.)
    end do
  end subroutine

  subroutine try_dev(path, force_mouse)
    character(*), intent(in) :: path
    logical, intent(in) :: force_mouse
    character(kind=c_char), target :: cpath(len_trim(path)+1)
    integer(c_int8_t), target :: evb(8), keyb(96), relb(8), absb(8)
    character(kind=c_char), target :: nameb(80)
    type(absinfo_t), target :: ax, ay
    integer(c_int) :: fd
    integer :: i, st
    logical :: mouse, pad
    do i = 1, len_trim(path)
      cpath(i) = path(i:i)
    end do
    cpath(len_trim(path)+1) = c_null_char
    ! O_RDWR | O_NONBLOCK | O_CLOEXEC (rdwr keeps a test FIFO from EOFing)
    fd = c_open(cpath, 526338_c_int, 0_c_int)
    if (fd < 0) return
    mouse = force_mouse
    pad = .false.
    if (.not. force_mouse) then
      evb = 0_1; keyb = 0_1; relb = 0_1; absb = 0_1
      if (c_ioctl_p(fd, IOC_GBIT_EV, c_loc(evb)) < 0) then
        st = c_close(fd); return
      end if
      if (tbit(evb, EV_KEY)) st = c_ioctl_p(fd, IOC_GBIT_KEY, c_loc(keyb))
      if (tbit(evb, EV_REL)) st = c_ioctl_p(fd, IOC_GBIT_REL, c_loc(relb))
      if (tbit(evb, EV_ABS)) st = c_ioctl_p(fd, IOC_GBIT_ABS, c_loc(absb))
      mouse = tbit(relb, REL_X) .and. tbit(relb, REL_Y) .and. tbit(keyb, BTN_LEFT)
      pad = (.not. mouse) .and. tbit(absb, ABS_X) .and. tbit(absb, ABS_Y) &
            .and. tbit(keyb, BTN_TOUCH)
      if (.not. (mouse .or. pad)) then
        st = c_close(fd); return
      end if
    end if
    ndev = ndev + 1
    fds(ndev) = fd
    is_pad(ndev) = pad
    names(ndev) = path
    if (.not. force_mouse) then
      nameb = c_null_char
      if (c_ioctl_p(fd, IOC_GNAME, c_loc(nameb)) > 0) then
        names(ndev) = ' '
        do i = 1, 79
          if (nameb(i) == c_null_char) exit
          names(ndev)(i:i) = nameb(i)
        end do
      end if
    end if
    if (pad) then
      ax = absinfo_t(); ay = absinfo_t()
      st = c_ioctl_p(fd, IOC_GABS_X, c_loc(ax))
      st = c_ioctl_p(fd, IOC_GABS_Y, c_loc(ay))
      rngx(ndev) = max(1, ax%maximum - ax%minimum)
      rngy(ndev) = max(1, ay%maximum - ay%minimum)
    end if
  end subroutine

  ! Flush deferred tap releases whose drag window has expired.
  subroutine inp_tick(evq, nev, now)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer(8), intent(in) :: now
    integer :: di
    do di = 1, ndev
      if (tap_held(di) .and. .not. tap_drag(di) .and. now - tap_at(di) > 280_8) then
        call put(evq, nev, TK_RELEASE, 0, 0, 0)
        tap_held(di) = .false.
      end if
    end do
  end subroutine

  ! Drain device di, moving the pointer (px, py) and appending tev events.
  ! Motion is flushed before any button so clicks land where they happened.
  subroutine inp_read(di, px, py, ow, oh, evq, nev, now)
    integer, intent(in) :: di, ow, oh
    integer(8), intent(in) :: now
    integer, intent(inout) :: px, py, nev
    type(tev), intent(inout) :: evq(:)
    integer(c_int8_t), target :: buf(24 * 64)
    integer(c_long) :: n
    integer :: nrec, i, o, etype, ecode, eval, d
    integer(8) :: tms
    logical :: moved
    real :: r
    moved = .false.
    do
      n = c_read(fds(di), c_loc(buf), int(size(buf), c_size_t))
      if (n < 24) exit
      nrec = int(n) / 24
      do i = 0, nrec - 1
        o = i * 24
        etype = iand(int(transfer(buf(o+17:o+18), 0_c_int16_t)), 65535)
        ecode = iand(int(transfer(buf(o+19:o+20), 0_c_int16_t)), 65535)
        eval = transfer(buf(o+21:o+24), 0_c_int32_t)
        select case (etype)
        case (EV_REL)
          select case (ecode)
          case (REL_X)
            px = max(1, min(ow, px + eval)); moved = .true.
          case (REL_Y)
            py = max(1, min(oh, py + eval)); moved = .true.
          case (REL_WHEEL)
            call flush_motion(px, py, moved, evq, nev)
            call put(evq, nev, TK_WHEEL, -eval, 0, 0)
          end select
        case (EV_ABS)
          if (.not. (is_pad(di) .and. touching(di))) cycle
          if (ecode == ABS_X) then
            if (have_last(di)) then
              r = real(eval - lax(di)) * 1.3 * real(ow) / real(rngx(di)) + frx(di)
              d = int(r); frx(di) = r - real(d)
              px = max(1, min(ow, px + d)); moved = .true.
              tmoved(di) = tmoved(di) + abs(r)
            end if
            lax(di) = eval
          else if (ecode == ABS_Y) then
            if (have_last(di)) then
              r = real(eval - lay(di)) * 1.3 * real(oh) / real(rngy(di)) + fry(di)
              d = int(r); fry(di) = r - real(d)
              py = max(1, min(oh, py + d)); moved = .true.
              tmoved(di) = tmoved(di) + abs(r)
            end if
            lay(di) = eval
            have_last(di) = .true.   ! Y closes the first sample pair
          end if
        case (EV_KEY)
          tms = transfer(buf(o+1:o+8), 0_c_int64_t) * 1000 + &
                transfer(buf(o+9:o+16), 0_c_int64_t) / 1000
          select case (ecode)
          case (BTN_LEFT, BTN_RIGHT, BTN_MIDDLE)
            call flush_motion(px, py, moved, evq, nev)
            if (tap_held(di)) then      ! physical click cancels a pending tap
              call put(evq, nev, TK_RELEASE, 0, px, py)
              tap_held(di) = .false.; tap_drag(di) = .false.
            end if
            d = 0                                   ! tev button: 0 L, 1 M, 2 R
            if (ecode == BTN_MIDDLE) d = 1
            if (ecode == BTN_RIGHT) d = 2
            if (eval /= 0) then
              call put(evq, nev, TK_PRESS, d, px, py)
            else
              call put(evq, nev, TK_RELEASE, d, px, py)
            end if
          case (BTN_TOUCH)
            if (.not. is_pad(di)) cycle
            if (eval /= 0) then
              touching(di) = .true.
              have_last(di) = .false.
              tdown(di) = tms
              tmoved(di) = 0.0
              frx(di) = 0.0; fry(di) = 0.0
              if (tap_held(di) .and. now - tap_at(di) <= 280_8) then
                tap_drag(di) = .true.   ! tap-and-drag: keep the button held
              else if (tap_held(di)) then
                call put(evq, nev, TK_RELEASE, 0, px, py)   ! stale tap: click ends
                tap_held(di) = .false.
              end if
            else
              touching(di) = .false.
              if (tap_drag(di)) then
                ! drag finger lifted: release. If this contact was itself a
                ! tap, that's a double-tap: release + a second full click.
                call flush_motion(px, py, moved, evq, nev)
                call put(evq, nev, TK_RELEASE, 0, px, py)
                tap_held(di) = .false.; tap_drag(di) = .false.
                if (tms - tdown(di) < 220_8 .and. tmoved(di) < 8.0) then
                  call put(evq, nev, TK_PRESS, 0, px, py)
                  call put(evq, nev, TK_RELEASE, 0, px, py)
                end if
              else if (tms - tdown(di) < 220_8 .and. tmoved(di) < 8.0) then
                ! tap: press now, release deferred (tap-and-drag window)
                call flush_motion(px, py, moved, evq, nev)
                call put(evq, nev, TK_PRESS, 0, px, py)
                tap_held(di) = .true.
                tap_at(di) = now
              end if
            end if
          end select
        end select
      end do
    end do
    call flush_motion(px, py, moved, evq, nev)
  end subroutine

  subroutine flush_motion(px, py, moved, evq, nev)
    integer, intent(in) :: px, py
    logical, intent(inout) :: moved
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    if (.not. moved) return
    call put(evq, nev, TK_MOTION, px, py, 0)
    moved = .false.
  end subroutine

  subroutine put(evq, nev, k, a, b, c)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer, intent(in) :: k, a, b, c
    if (nev >= size(evq)) return
    nev = nev + 1
    evq(nev)%k = k
    evq(nev)%a = a
    evq(nev)%b = b
    evq(nev)%c = c
  end subroutine

end module fl_input

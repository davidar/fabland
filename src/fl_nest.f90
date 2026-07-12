! fl_nest — nested backend: fabland running as a Wayland *client*.
! The composited desktop is presented as a toplevel window on a host
! compositor (GNOME, weston, or another fabland), and the host's
! keyboard/pointer events are fed back into fabland's own seat.
! Client-side wire protocol, shm-over-memfd, xdg-shell handshake —
! still all Fortran.
module fl_nest
  use iso_c_binding
  use fl_libc
  use fl_term    ! for tev / TK_* event vocabulary
  implicit none
  private
  public :: nest_connect, nest_pump, nest_present, nest_fd, &
            nest_ready, nest_dead, nest_can_present

  integer, parameter :: NW = 1024, NH = 640   ! must match fl_core OUTW/OUTH

  integer(c_int) :: ufd = -1
  logical :: nest_dead = .false.
  logical :: nest_ready = .false.

  ! upstream object ids (we are the client, we allocate)
  integer, parameter :: ID_DISPLAY = 1, ID_REGISTRY = 2
  integer :: next_id = 3
  integer :: id_sync = 0, id_compositor = 0, id_shm = 0, id_seat = 0
  integer :: id_wm_base = 0, id_surface = 0, id_xdg = 0, id_top = 0
  integer :: id_pool = 0, id_buf(2) = 0, id_kbd = 0, id_ptr = 0
  integer :: id_frame_cb = 0
  logical :: buf_busy(2) = .false.
  logical :: frame_pending = .false.
  logical :: configured = .false.
  integer :: seat_caps = 0

  ! shm pool (double buffered)
  type(c_ptr) :: pool_mem = c_null_ptr
  integer(c_int) :: pool_fd = -1

  ! read buffer + fd queue for upstream events
  integer(c_int8_t) :: rb(65536) = 0_1
  integer :: rblen = 0
  integer(c_int) :: rfdq(16) = -1
  integer :: nrfdq = 0

  ! outgoing scratch
  integer(4), target :: uw(64)
  integer :: un = 0

contains

  function nest_fd() result(fd)
    integer(c_int) :: fd
    fd = ufd
  end function

  function nest_can_present() result(ok)
    logical :: ok
    ok = nest_ready .and. .not. frame_pending .and. &
         (.not. buf_busy(1) .or. .not. buf_busy(2))
  end function

  function new_id() result(id)
    integer :: id
    id = next_id
    next_id = next_id + 1
  end function

  ! ── outgoing requests ─────────────────────────────────────────────────

  subroutine ureset()
    un = 0
  end subroutine

  subroutine uput(v)
    integer, intent(in) :: v
    un = un + 1
    uw(2 + un) = v
  end subroutine

  subroutine uput_s(s)
    character(*), intent(in) :: s
    integer :: l, nwords, i
    integer(1) :: tmp(256)
    l = len_trim(s) + 1
    call uput(l)
    nwords = (l + 3) / 4
    tmp = 0_1
    do i = 1, l - 1
      tmp(i) = int(iachar(s(i:i)), 1)
    end do
    uw(2+un+1 : 2+un+nwords) = transfer(tmp(1:nwords*4), 0_4, nwords)
    un = un + nwords
  end subroutine

  subroutine usend(id, op)
    integer, intent(in) :: id, op
    integer :: nbytes
    nbytes = (2 + un) * 4
    uw(1) = id
    uw(2) = ior(shiftl(nbytes, 16), op)
    if (.not. send_all(ufd, c_loc(uw), nbytes)) nest_dead = .true.
  end subroutine

  subroutine usend_fd(id, op, passfd)
    integer, intent(in) :: id, op
    integer(c_int), intent(in) :: passfd
    integer :: nbytes
    nbytes = (2 + un) * 4
    uw(1) = id
    uw(2) = ior(shiftl(nbytes, 16), op)
    if (.not. send_with_fd(ufd, c_loc(uw), nbytes, passfd)) nest_dead = .true.
  end subroutine

  ! ── incoming event helpers ────────────────────────────────────────────

  function eu32(pos) result(v)
    integer, intent(in) :: pos
    integer(4) :: v
    v = transfer(rb(pos:pos+3), 0_4)
  end function

  subroutine estr(pos, s)
    integer, intent(inout) :: pos
    character(*), intent(out) :: s
    integer :: l, i, n
    l = eu32(pos)
    pos = pos + 4
    s = ' '
    n = min(l - 1, len(s))
    do i = 1, n
      s(i:i) = achar(iand(int(rb(pos+i-1)), 255))
    end do
    pos = pos + shiftl(shiftr(l + 3, 2), 2)
  end subroutine

  ! ── connection + handshake ────────────────────────────────────────────

  function nest_connect(rtdir, host_display, title) result(ok)
    character(*), intent(in) :: rtdir, host_display, title
    logical :: ok
    type(tev) :: dummy(8)
    integer :: ndummy, spins

    ok = .false.
    ufd = connect_socket(trim(rtdir)//'/'//trim(host_display))
    if (ufd < 0) return

    ! wl_display.get_registry(2), then a sync to learn all globals
    call ureset(); call uput(ID_REGISTRY); call usend(ID_DISPLAY, 1)
    id_sync = new_id()
    call ureset(); call uput(id_sync); call usend(ID_DISPLAY, 0)

    ! pump until the sync comes back (globals bound as they arrive)
    spins = 0
    do while (id_sync /= 0 .and. .not. nest_dead .and. spins < 400)
      ndummy = 0
      call pump_block(dummy, ndummy, 10)
      spins = spins + 1
    end do
    if (nest_dead) return
    if (id_compositor == 0 .or. id_shm == 0 .or. id_wm_base == 0) then
      nest_dead = .true.
      return
    end if

    ! surface + xdg toplevel
    id_surface = new_id()
    call ureset(); call uput(id_surface); call usend(id_compositor, 0)
    id_xdg = new_id()
    call ureset(); call uput(id_xdg); call uput(id_surface); call usend(id_wm_base, 2)
    id_top = new_id()
    call ureset(); call uput(id_top); call usend(id_xdg, 1)
    call ureset(); call uput_s(trim(title)); call usend(id_top, 2)          ! set_title
    call ureset(); call uput_s('cc.vidr.fabland'); call usend(id_top, 3)    ! set_app_id
    call usend0(id_surface, 6)                                              ! commit

    ! shm pool: memfd with two framebuffers
    pool_fd = memfd_sized('fabland-fb', NW * NH * 4 * 2)
    if (pool_fd < 0) then
      nest_dead = .true.
      return
    end if
    pool_mem = c_mmap(c_null_ptr, int(NW*NH*4*2, c_size_t), 3, MAP_SHARED, pool_fd, 0_c_long)
    id_pool = new_id()
    call ureset(); call uput(id_pool); call uput(NW * NH * 4 * 2)
    call usend_fd(id_shm, 0, pool_fd)
    id_buf(1) = new_id()
    call ureset(); call uput(id_buf(1)); call uput(0)
    call uput(NW); call uput(NH); call uput(NW*4); call uput(1)   ! xrgb8888
    call usend(id_pool, 0)
    id_buf(2) = new_id()
    call ureset(); call uput(id_buf(2)); call uput(NW * NH * 4)
    call uput(NW); call uput(NH); call uput(NW*4); call uput(1)
    call usend(id_pool, 0)

    ! wait for the initial configure
    spins = 0
    do while (.not. configured .and. .not. nest_dead .and. spins < 400)
      ndummy = 0
      call pump_block(dummy, ndummy, 10)
      spins = spins + 1
    end do
    if (nest_dead .or. .not. configured) then
      nest_dead = .true.
      return
    end if
    nest_ready = .true.
    ok = .true.
  end function

  subroutine usend0(id, op)
    integer, intent(in) :: id, op
    call ureset()
    call usend(id, op)
  end subroutine

  ! poll the upstream fd briefly, then pump
  subroutine pump_block(evq, nev, tmo)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer, intent(in) :: tmo
    type(pollfd_t) :: p(1)
    integer :: st
    p(1)%fd = ufd
    p(1)%events = POLLIN
    p(1)%revents = 0
    st = c_poll(p, 1_c_long, tmo)
    if (iand(int(p(1)%revents), int(POLLIN)) /= 0) call nest_pump(evq, nev)
  end subroutine

  ! ── event pump: read + dispatch everything available ─────────────────

  subroutine nest_pump(evq, nev)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer(8) :: n
    integer :: off, id, word, msize, opc, i, st

    n = recv_with_fds(ufd, rb, rblen, size(rb), rfdq, nrfdq)
    if (n <= 0) then
      nest_dead = .true.
      return
    end if
    rblen = rblen + int(n)

    off = 0
    do
      if (rblen - off < 8) exit
      id = eu32(off + 1)
      word = eu32(off + 5)
      msize = iand(shiftr(word, 16), 65535)
      opc = iand(word, 65535)
      if (msize < 8) then
        nest_dead = .true.
        exit
      end if
      if (rblen - off < msize) exit
      call handle_event(id, opc, off + 9, evq, nev)
      off = off + msize
    end do
    if (off > 0) then
      if (rblen > off) rb(1:rblen-off) = rb(off+1:rblen)
      rblen = rblen - off
    end if

    ! we never keep host fds (e.g. keymaps)
    do i = 1, nrfdq
      st = c_close(rfdq(i))
    end do
    nrfdq = 0
  end subroutine

  subroutine push(evq, nev, k, a, b, c)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer, intent(in) :: k, a, b, c
    if (nev < size(evq)) then
      nev = nev + 1
      evq(nev) = tev(k, a, b, c)
    end if
  end subroutine

  subroutine handle_event(id, opc, ap, evq, nev)
    integer, intent(in) :: id, opc, ap
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    character(64) :: iface
    integer :: p, gname, gver, v, bindver

    if (id == ID_DISPLAY) then
      if (opc == 0) then          ! error(object, code, message)
        nest_dead = .true.
      end if                      ! delete_id: ignored

    else if (id == ID_REGISTRY) then
      if (opc == 0) then          ! global(name, interface, version)
        gname = eu32(ap)
        p = ap + 4
        call estr(p, iface)
        gver = eu32(p)
        select case (trim(iface))
        case ('wl_compositor')
          id_compositor = new_id()
          call bind_global(gname, iface, min(gver, 4), id_compositor)
        case ('wl_shm')
          id_shm = new_id()
          call bind_global(gname, iface, 1, id_shm)
        case ('wl_seat')
          id_seat = new_id()
          call bind_global(gname, iface, min(gver, 5), id_seat)
        case ('xdg_wm_base')
          id_wm_base = new_id()
          call bind_global(gname, iface, 1, id_wm_base)
        end select
      end if

    else if (id == id_sync .and. id /= 0) then
      id_sync = 0                 ! roundtrip complete

    else if (id == id_wm_base .and. id /= 0) then
      if (opc == 0) then          ! ping -> pong
        v = eu32(ap)
        call ureset(); call uput(v); call usend(id_wm_base, 3)
      end if

    else if (id == id_xdg .and. id /= 0) then
      if (opc == 0) then          ! configure(serial) -> ack
        v = eu32(ap)
        call ureset(); call uput(v); call usend(id_xdg, 4)
        configured = .true.
      end if

    else if (id == id_top .and. id /= 0) then
      if (opc == 1) call push(evq, nev, TK_QUIT, 0, 0, 0)   ! close
      ! configure/bounds: we keep our fixed size

    else if (id == id_seat .and. id /= 0) then
      if (opc == 0) then          ! capabilities
        seat_caps = eu32(ap)
        if (iand(seat_caps, 1) /= 0 .and. id_ptr == 0) then
          id_ptr = new_id()
          call ureset(); call uput(id_ptr); call usend(id_seat, 0)
        end if
        if (iand(seat_caps, 2) /= 0 .and. id_kbd == 0) then
          id_kbd = new_id()
          call ureset(); call uput(id_kbd); call usend(id_seat, 1)
        end if
      end if

    else if (id == id_kbd .and. id /= 0) then
      select case (opc)
      case (3)                    ! key(serial, time, key, state)
        call push(evq, nev, TK_RAWKEY, eu32(ap + 8), eu32(ap + 12), 0)
      case (4)                    ! modifiers(serial, dep, lat, lock, group)
        call push(evq, nev, TK_RAWMODS, eu32(ap + 4), eu32(ap + 8), eu32(ap + 12))
      end select                  ! keymap fd already closed by pump

    else if (id == id_ptr .and. id /= 0) then
      select case (opc)
      case (0)                    ! enter(serial, surface, sx, sy)
        call push(evq, nev, TK_MOTION, eu32(ap + 8) / 256, eu32(ap + 12) / 256, 0)
      case (2)                    ! motion(time, sx, sy)
        call push(evq, nev, TK_MOTION, eu32(ap + 4) / 256, eu32(ap + 8) / 256, 0)
      case (3)                    ! button(serial, time, button, state)
        v = eu32(ap + 8)
        select case (v)
        case (272); v = 0
        case (274); v = 1
        case default; v = 2
        end select
        if (eu32(ap + 12) == 1) then
          call push(evq, nev, TK_PRESS, v, -1, -1)    ! -1: keep current position
        else
          call push(evq, nev, TK_RELEASE, v, -1, -1)
        end if
      case (4)                    ! axis(time, axis, value)
        if (eu32(ap + 4) == 0) then
          v = eu32(ap + 8)
          call push(evq, nev, TK_WHEEL, merge(1, -1, v > 0), 0, 0)
        end if
      end select

    else if (id == id_buf(1)) then
      if (opc == 0) buf_busy(1) = .false.
    else if (id == id_buf(2)) then
      if (opc == 0) buf_busy(2) = .false.
    else if (id == id_frame_cb .and. id /= 0) then
      frame_pending = .false.
      id_frame_cb = 0
    end if
  end subroutine

  subroutine bind_global(gname, iface, ver, idout)
    integer, intent(in) :: gname, ver, idout
    character(*), intent(in) :: iface
    call ureset()
    call uput(gname)
    call uput_s(trim(iface))
    call uput(ver)
    call uput(idout)
    call usend(ID_REGISTRY, 0)
  end subroutine

  ! ── presentation ──────────────────────────────────────────────────────

  subroutine nest_present(canvas)
    integer(4), intent(in) :: canvas(NW, NH)
    integer(4), pointer :: fb(:)
    integer :: b, x, y, base
    if (.not. nest_can_present()) return
    b = merge(1, 2, .not. buf_busy(1))
    call c_f_pointer(pool_mem, fb, [NW * NH * 2])
    base = (b - 1) * NW * NH
    do y = 1, NH
      do x = 1, NW
        fb(base + (y-1)*NW + x) = canvas(x, y)
      end do
    end do
    call ureset(); call uput(id_buf(b)); call uput(0); call uput(0)
    call usend(id_surface, 1)                                   ! attach
    call ureset(); call uput(0); call uput(0); call uput(NW); call uput(NH)
    call usend(id_surface, 2)                                   ! damage
    id_frame_cb = new_id()
    call ureset(); call uput(id_frame_cb); call usend(id_surface, 3)  ! frame
    call usend0(id_surface, 6)                                  ! commit
    buf_busy(b) = .true.
    frame_pending = .true.
  end subroutine

end module fl_nest

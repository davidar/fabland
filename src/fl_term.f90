! fl_term — terminal backend: your terminal emulator becomes the monitor,
! keyboard and mouse. Output is truecolor half-block cells; input is raw
! bytes + xterm SGR mouse reports parsed into events.
module fl_term
  use iso_c_binding
  use fl_libc
  implicit none
  private
  public :: tev, term_init, term_shutdown, term_probe, term_render, &
            term_read_input, term_tick, &
            TK_CHAR, TK_SPECIAL, TK_MOTION, TK_PRESS, TK_RELEASE, TK_WHEEL

  integer, parameter :: TK_CHAR = 1     ! a=byte, b=1 if alt held
  integer, parameter :: TK_SPECIAL = 2  ! a=special id (see SP_*)
  integer, parameter :: TK_MOTION = 3   ! a,b = canvas x,y
  integer, parameter :: TK_PRESS = 4    ! a=button(0 left,1 middle,2 right), b,c = canvas x,y
  integer, parameter :: TK_RELEASE = 5
  integer, parameter :: TK_WHEEL = 6    ! a = +1 down / -1 up

  type :: tev
    integer :: k = 0
    integer :: a = 0, b = 0, c = 0
  end type

  integer :: cols = 80, rows = 24
  real :: tscale = 1.0
  integer :: tox = 0, toy = 0        ! letterbox offset in terminal pixels
  integer :: cvw = 0, cvh = 0        ! canvas dims (from last probe)

  ! pending input bytes (partial escape sequences)
  integer :: pbuf(512), plen = 0
  integer(8) :: ptime = 0

  character(1), allocatable :: fb(:)
  integer :: fblen = 0

contains

  subroutine tput(s)
    character(*), intent(in) :: s
    integer(c_int8_t), allocatable, target :: raw(:)
    integer :: i
    integer(8) :: w
    allocate(raw(len(s)))
    do i = 1, len(s)
      raw(i) = int(iachar(s(i:i)), 1)
    end do
    w = c_write(1_c_int, c_loc(raw(1)), int(len(s), c_size_t))
  end subroutine

  subroutine term_init()
    call execute_command_line('stty raw -echo 2>/dev/null')
    call tput(achar(27)//'[?1049h'//achar(27)//'[?25l'// &
              achar(27)//'[?1003h'//achar(27)//'[?1006h'//achar(27)//'[2J')
  end subroutine

  subroutine term_shutdown()
    call tput(achar(27)//'[?1003l'//achar(27)//'[?1006l'// &
              achar(27)//'[?25h'//achar(27)//'[0m'//achar(27)//'[?1049l')
    call execute_command_line('stty sane 2>/dev/null')
  end subroutine

  subroutine term_probe(w, h)
    integer, intent(in) :: w, h      ! canvas size
    type(winsize_t) :: ws
    integer :: st, pw, ph
    st = c_ioctl_ws(1_c_int, 21523_c_long, ws)   ! TIOCGWINSZ
    if (st == 0 .and. ws%col > 0) then
      cols = min(int(ws%col), 500)
      rows = min(int(ws%row), 250)
    end if
    cvw = w; cvh = h
    pw = cols
    ph = (rows - 1) * 2
    tscale = min(real(pw) / real(w), real(ph) / real(h))
    tox = (pw - int(w * tscale)) / 2
    toy = (ph - int(h * tscale)) / 2
    if (allocated(fb)) deallocate(fb)
    allocate(fb(rows * cols * 48 + 65536))
  end subroutine

  subroutine app(s)
    character(*), intent(in) :: s
    integer :: i
    do i = 1, len(s)
      fb(fblen + i) = s(i:i)
    end do
    fblen = fblen + len(s)
  end subroutine

  subroutine app_int(v)
    integer, intent(in) :: v
    character(12) :: t
    write(t, '(i0)') v
    call app(trim(t))
  end subroutine

  function sample(canvas, tx, ty) result(col)
    integer(4), intent(in) :: canvas(cvw, cvh)
    integer, intent(in) :: tx, ty     ! terminal pixel coords, 0-based
    integer(4) :: col
    integer :: cx, cy, k, r, g, b
    real :: fx, fy
    integer, parameter :: LETTERBOX = 921620   ! #0e1014
    if (tx < tox .or. ty < toy) then
      col = LETTERBOX; return
    end if
    r = 0; g = 0; b = 0
    do k = 0, 3
      fx = (real(tx - tox) + 0.25 + 0.5 * mod(k, 2)) / tscale
      fy = (real(ty - toy) + 0.25 + 0.5 * (k / 2)) / tscale
      cx = int(fx) + 1
      cy = int(fy) + 1
      if (cx < 1 .or. cx > cvw .or. cy < 1 .or. cy > cvh) then
        col = LETTERBOX; return
      end if
      r = r + iand(shiftr(canvas(cx, cy), 16), 255)
      g = g + iand(shiftr(canvas(cx, cy), 8), 255)
      b = b + iand(canvas(cx, cy), 255)
    end do
    ! quantize slightly so identical-color runs compress well
    r = iand(r / 4, 252); g = iand(g / 4, 252); b = iand(b / 4, 252)
    col = ior(shiftl(r, 16), ior(shiftl(g, 8), b))
  end function

  subroutine term_render(canvas, status)
    integer(4), intent(in) :: canvas(cvw, cvh)
    character(*), intent(in) :: status
    integer :: r, c, ty
    integer(4) :: top, bot, lf, lb
    integer(8) :: w
    integer(c_int8_t), allocatable, target :: raw(:)
    integer :: i, n

    fblen = 0
    call app(achar(27)//'[H')
    do r = 1, rows - 1
      lf = -1; lb = -1
      ty = (r - 1) * 2
      do c = 1, cols
        top = sample(canvas, c - 1, ty)
        bot = sample(canvas, c - 1, ty + 1)
        if (top /= lf .or. bot /= lb) then
          call app(achar(27)//'[38;2;')
          call app_int(iand(shiftr(top, 16), 255)); call app(';')
          call app_int(iand(shiftr(top, 8), 255));  call app(';')
          call app_int(iand(top, 255))
          call app(';48;2;')
          call app_int(iand(shiftr(bot, 16), 255)); call app(';')
          call app_int(iand(shiftr(bot, 8), 255));  call app(';')
          call app_int(iand(bot, 255)); call app('m')
          lf = top; lb = bot
        end if
        call app(achar(226)//achar(150)//achar(128))   ! ▀
      end do
      call app(achar(27)//'[0m'//achar(13)//achar(10))
    end do
    ! status bar
    call app(achar(27)//'[7m')
    if (len_trim(status) >= cols) then
      call app(status(1:cols))
    else
      call app(trim(status)//repeat(' ', cols - len_trim(status)))
    end if
    call app(achar(27)//'[0m')

    n = fblen
    allocate(raw(n))
    do i = 1, n
      raw(i) = int(iachar(fb(i)), 1)
    end do
    w = c_write(1_c_int, c_loc(raw(1)), int(n, c_size_t))
  end subroutine

  ! map a terminal cell (1-based) to canvas coordinates
  subroutine cell_to_canvas(mx, my, px, py)
    integer, intent(in) :: mx, my
    integer, intent(out) :: px, py
    px = int((real(mx - 1 - tox) + 0.5) / tscale)
    py = int((real((my - 1) * 2 - toy) + 1.0) / tscale)
    px = max(0, min(cvw - 1, px))
    py = max(0, min(cvh - 1, py))
  end subroutine

  ! read available stdin bytes and parse into events
  subroutine term_read_input(evq, nev, now)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer(8), intent(in) :: now
    integer(c_int8_t), target :: buf(256)
    integer(8) :: n
    integer :: i
    n = c_read(0_c_int, c_loc(buf), 256_c_size_t)
    do i = 1, int(n)
      if (plen < size(pbuf)) then
        plen = plen + 1
        pbuf(plen) = iand(int(buf(i)), 255)
        if (plen == 1) ptime = now
      end if
    end do
    call parse_pending(evq, nev, now, .false.)
  end subroutine

  ! flush a lone ESC / stale partial sequence after a timeout
  subroutine term_tick(evq, nev, now)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer(8), intent(in) :: now
    if (plen > 0 .and. now - ptime > 60_8) call parse_pending(evq, nev, now, .true.)
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

  subroutine parse_pending(evq, nev, now, flush)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer(8), intent(in) :: now
    logical, intent(in) :: flush
    integer :: pos, b, fin, i, np, prm(4), px, py, sp
    logical :: incomplete

    pos = 1
    do while (pos <= plen)
      b = pbuf(pos)
      if (b /= 27) then
        call push(evq, nev, TK_CHAR, b, 0, 0)
        pos = pos + 1
        cycle
      end if
      ! ESC ...
      if (pos == plen) then
        if (flush) then
          call push(evq, nev, TK_CHAR, 27, 0, 0)
          pos = pos + 1
        end if
        exit
      end if
      b = pbuf(pos + 1)
      if (b == iachar('[')) then
        ! CSI: find final byte
        fin = 0
        incomplete = .true.
        do i = pos + 2, plen
          if (pbuf(i) >= 64 .and. pbuf(i) <= 126) then
            fin = i
            incomplete = .false.
            exit
          end if
        end do
        if (incomplete) then
          if (flush) pos = plen + 1   ! drop garbage
          exit
        end if
        call parse_csi(evq, nev, pos + 2, fin)
        pos = fin + 1
      else if (b == iachar('O')) then
        if (pos + 2 > plen) then
          if (flush) pos = plen + 1
          exit
        end if
        sp = 0
        select case (pbuf(pos + 2))
        case (iachar('P')); sp = 11
        case (iachar('Q')); sp = 12
        case (iachar('R')); sp = 13
        case (iachar('S')); sp = 14
        case (iachar('H')); sp = 5
        case (iachar('F')); sp = 6
        end select
        if (sp > 0) call push(evq, nev, TK_SPECIAL, sp, 0, 0)
        pos = pos + 3
      else
        ! Alt+key
        call push(evq, nev, TK_CHAR, b, 1, 0)
        pos = pos + 2
      end if
    end do
    ! keep unconsumed tail
    if (pos > 1) then
      np = 0
      do i = pos, plen
        np = np + 1
        pbuf(np) = pbuf(i)
      end do
      plen = np
      ptime = now
    end if
  end subroutine

  subroutine parse_csi(evq, nev, p0, fin)
    type(tev), intent(inout) :: evq(:)
    integer, intent(inout) :: nev
    integer, intent(in) :: p0, fin
    integer :: prm(4), np, i, b, v, sp, btn, mx, my, px, py
    logical :: have, mouse

    mouse = (p0 <= fin - 1 .and. pbuf(p0) == iachar('<'))
    prm = 0; np = 0; v = 0; have = .false.
    do i = merge(p0 + 1, p0, mouse), fin - 1
      b = pbuf(i)
      if (b >= iachar('0') .and. b <= iachar('9')) then
        v = v * 10 + (b - iachar('0'))
        have = .true.
      else if (b == iachar(';')) then
        if (np < 4) then
          np = np + 1
          prm(np) = v
        end if
        v = 0; have = .false.
      end if
    end do
    if (have .and. np < 4) then
      np = np + 1
      prm(np) = v
    end if

    if (mouse) then
      if (np < 3) return
      btn = prm(1); mx = prm(2); my = prm(3)
      call cell_to_canvas(mx, my, px, py)
      if (btn >= 64 .and. btn <= 65) then
        call push(evq, nev, TK_MOTION, px, py, 0)
        call push(evq, nev, TK_WHEEL, merge(-1, 1, btn == 64), 0, 0)
      else if (iand(btn, 32) /= 0) then
        call push(evq, nev, TK_MOTION, px, py, 0)
      else if (pbuf(fin) == iachar('M')) then
        call push(evq, nev, TK_MOTION, px, py, 0)
        call push(evq, nev, TK_PRESS, iand(btn, 3), px, py)
      else if (pbuf(fin) == iachar('m')) then
        call push(evq, nev, TK_RELEASE, iand(btn, 3), px, py)
      end if
      return
    end if

    sp = 0
    select case (pbuf(fin))
    case (iachar('A')); sp = 1
    case (iachar('B')); sp = 2
    case (iachar('C')); sp = 3
    case (iachar('D')); sp = 4
    case (iachar('H')); sp = 5
    case (iachar('F')); sp = 6
    case (iachar('~'))
      select case (prm(1))
      case (2);  sp = 9    ! insert
      case (3);  sp = 10   ! delete
      case (5);  sp = 7    ! pgup
      case (6);  sp = 8    ! pgdn
      case (11); sp = 11   ! F1
      case (12); sp = 12
      case (13); sp = 13
      case (14); sp = 14
      case (15); sp = 15   ! F5
      case (17); sp = 16
      case (18); sp = 17
      case (19); sp = 18
      case (20); sp = 19
      case (21); sp = 20   ! F10
      case (24); sp = 22   ! F12
      end select
    end select
    if (sp > 0) call push(evq, nev, TK_SPECIAL, sp, 0, 0)
  end subroutine

end module fl_term

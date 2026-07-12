! ═══════════════════════════════════════════════════════════════════════
!  fabland — a Wayland compositor written in Fortran.
!
!  Speaks the Wayland wire protocol directly over a unix socket (no
!  libwayland, no wlroots): wl_display, wl_registry, wl_compositor,
!  wl_shm (with SCM_RIGHTS fd passing + mmap), wl_seat with keyboard
!  and pointer (xkb keymap served over a memfd), wl_output,
!  wl_subcompositor, wl_data_device_manager and xdg-shell — enough for
!  real, unmodified Wayland clients (including toolkit apps like
!  weston-terminal) to connect, map toplevels, and receive input.
!
!  Two backends:
!    term     — your terminal is the monitor (truecolor half-blocks),
!               your keyboard/mouse are the seat (raw stdin + SGR mouse)
!    headless — PNG frames only
!  Both write PNG screenshots via a from-scratch encoder.
! ═══════════════════════════════════════════════════════════════════════
module fl_core
  use iso_c_binding
  use fl_libc
  use fl_png
  use fl_xkb
  use fl_term
  implicit none

  integer, parameter :: OUTW = 1024, OUTH = 640

  ! interface ids
  integer, parameter :: IF_DISPLAY = 1, IF_REGISTRY = 2, IF_CALLBACK = 3, &
    IF_COMPOSITOR = 4, IF_SHM = 5, IF_SHM_POOL = 6, IF_BUFFER = 7, &
    IF_SURFACE = 8, IF_REGION = 9, IF_OUTPUT = 10, IF_SEAT = 11, &
    IF_WM_BASE = 12, IF_POSITIONER = 13, IF_XDG_SURFACE = 14, &
    IF_TOPLEVEL = 15, IF_POPUP = 16, IF_DUMMY = 17, &
    IF_POINTER = 18, IF_KEYBOARD = 19, IF_TOUCH = 20, &
    IF_SUBCOMP = 21, IF_SUBSURF = 22, &
    IF_DDM = 23, IF_DATA_DEV = 24, IF_DATA_SRC = 25

  ! registry global names
  integer, parameter :: G_COMPOSITOR = 1, G_SHM = 2, G_SEAT = 3, &
    G_OUTPUT = 4, G_WM_BASE = 5, G_SUBCOMP = 6, G_DDM = 7

  integer(4), parameter :: AMASK = -16777216      ! 0xFF000000
  integer(4), parameter :: RGBMASK = 16777215     ! 0x00FFFFFF

  ! linux button codes
  integer, parameter :: BTN_LEFT = 272, BTN_RIGHT = 273, BTN_MIDDLE = 274

  type :: obj_t
    integer :: id = 0
    integer :: iface = 0
    integer :: ver = 1
    integer :: datai = 0
  end type

  type :: client_t
    logical :: used = .false.
    integer(c_int) :: fd = -1
    type(obj_t) :: objs(512)
    integer :: nobjs = 0
    integer(c_int) :: fdq(32) = -1
    integer :: nfdq = 0
    integer(c_int8_t) :: rbuf(65536) = 0_1
    integer :: rlen = 0
    logical :: dead = .false.
    integer :: kbd_id = 0, kbd_ver = 1
    integer :: ptr_id = 0, ptr_ver = 1
  end type

  type :: surf_t
    logical :: used = .false.
    integer :: ci = 0
    integer :: sid = 0
    integer :: xdg_id = 0, top_id = 0
    logical :: configured = .false., acked = .false., mapped = .false.
    integer :: pend_buf = -1        ! -1 nothing, 0 null attach, >0 buffer id
    integer :: w = 0, h = 0, x = 64, y = 64
    integer(4), allocatable :: tex(:,:)
    integer :: cbs(16) = 0
    integer :: ncbs = 0
    character(128) :: title = ' '
    integer :: parent_si = 0        ! nonzero: this is a subsurface
    integer :: subx = 0, suby = 0
    logical :: has_csd = .false.    ! client draws own decorations
  end type

  type :: pool_t
    logical :: used = .false.
    integer :: ci = 0
    integer(c_int) :: fd = -1
    integer(c_size_t) :: sz = 0
    type(c_ptr) :: mem = c_null_ptr
  end type

  type :: buf_t
    logical :: used = .false.
    integer :: ci = 0
    integer :: pooli = 0
    integer :: off = 0, w = 0, h = 0, stride = 0, fmt = 0
  end type

  type(client_t), target :: clients(8)
  type(surf_t),  target :: surfs(64)
  type(pool_t),  target :: pools(64)
  type(buf_t),   target :: bufs(256)

  integer :: serial = 0
  integer :: nwindows = 0
  logical :: needs_paint = .true.
  logical :: debug = .false.
  logical :: term_mode = .false.
  integer :: logu = 6
  integer(4), allocatable :: canvas(:,:)

  ! stacking order (bottom -> top), surface indices
  integer :: zlist(64) = 0, nz = 0

  ! input state
  integer :: kfocus = 0            ! keyboard-focused surface index
  integer :: pfocus = 0            ! pointer-focused surface index
  integer :: drag_si = 0, drag_dx = 0, drag_dy = 0
  integer :: ptr_x = OUTW/2, ptr_y = OUTH/2
  integer(c_int) :: keymap_fd = -1
  integer :: keymap_sz = 0
  logical :: want_shot = .false.
  logical :: want_quit = .false.

  ! outgoing message scratch
  integer(4), target :: mw(1024)
  integer :: mn = 0

contains

  subroutine logmsg(s)
    character(*), intent(in) :: s
    write(logu, '(a)') '[fabland] '//trim(s)
    flush(logu)
  end subroutine

  function itoa(v) result(s)
    integer, intent(in) :: v
    character(16) :: s
    write(s, '(i0)') v
  end function

  function next_serial() result(s)
    integer :: s
    serial = serial + 1
    s = serial
  end function

  function tms32() result(t)
    integer :: t
    t = int(mod(now_ms(), 2000000000_8))
  end function

  ! ── outgoing wire messages ────────────────────────────────────────────

  subroutine mreset()
    mn = 0
  end subroutine

  subroutine mput_u(v)
    integer, intent(in) :: v
    mn = mn + 1
    mw(2 + mn) = v
  end subroutine

  subroutine mput_s(s)
    character(*), intent(in) :: s
    integer :: l, nw, i
    integer(1) :: tmp(512)
    l = len_trim(s) + 1
    call mput_u(l)
    nw = (l + 3) / 4
    tmp = 0_1
    do i = 1, l - 1
      tmp(i) = int(iachar(s(i:i)), 1)
    end do
    mw(2+mn+1 : 2+mn+nw) = transfer(tmp(1:nw*4), 0_4, nw)
    mn = mn + nw
  end subroutine

  subroutine msend(ci, id, op)
    integer, intent(in) :: ci, id, op
    integer :: nbytes
    logical :: ok
    nbytes = (2 + mn) * 4
    mw(1) = id
    mw(2) = ior(shiftl(nbytes, 16), op)
    ok = send_all(clients(ci)%fd, c_loc(mw), nbytes)
    if (.not. ok) then
      clients(ci)%dead = .true.
      call logmsg('send FAILED: obj '//trim(itoa(id))//' op '//trim(itoa(op)))
    end if
  end subroutine

  subroutine msend_fd(ci, id, op, passfd)
    integer, intent(in) :: ci, id, op
    integer(c_int), intent(in) :: passfd
    integer :: nbytes
    logical :: ok
    nbytes = (2 + mn) * 4
    mw(1) = id
    mw(2) = ior(shiftl(nbytes, 16), op)
    ok = send_with_fd(clients(ci)%fd, c_loc(mw), nbytes, passfd)
    if (.not. ok) clients(ci)%dead = .true.
  end subroutine

  subroutine emit0(ci, id, op)
    integer, intent(in) :: ci, id, op
    call mreset()
    call msend(ci, id, op)
  end subroutine

  subroutine emit1(ci, id, op, a)
    integer, intent(in) :: ci, id, op, a
    call mreset()
    call mput_u(a)
    call msend(ci, id, op)
  end subroutine

  subroutine send_delete_id(ci, id)
    integer, intent(in) :: ci, id
    call emit1(ci, 1, 1, id)     ! wl_display.delete_id
  end subroutine

  ! ── object table ──────────────────────────────────────────────────────

  function find_obj(ci, id) result(oi)
    integer, intent(in) :: ci, id
    integer :: oi, i
    oi = 0
    do i = 1, clients(ci)%nobjs
      if (clients(ci)%objs(i)%id == id) then
        oi = i
        return
      end if
    end do
  end function

  subroutine add_obj(ci, id, iface, ver, datai)
    integer, intent(in) :: ci, id, iface, ver, datai
    integer :: n
    if (clients(ci)%nobjs >= size(clients(ci)%objs)) return
    n = clients(ci)%nobjs + 1
    clients(ci)%nobjs = n
    clients(ci)%objs(n) = obj_t(id, iface, ver, datai)
  end subroutine

  subroutine del_obj(ci, id)
    integer, intent(in) :: ci, id
    integer :: oi, n
    oi = find_obj(ci, id)
    if (oi == 0) return
    n = clients(ci)%nobjs
    clients(ci)%objs(oi) = clients(ci)%objs(n)
    clients(ci)%nobjs = n - 1
    call send_delete_id(ci, id)
  end subroutine

  function iface_name(f) result(s)
    integer, intent(in) :: f
    character(24) :: s
    select case (f)
    case (IF_DISPLAY);     s = 'wl_display'
    case (IF_REGISTRY);    s = 'wl_registry'
    case (IF_CALLBACK);    s = 'wl_callback'
    case (IF_COMPOSITOR);  s = 'wl_compositor'
    case (IF_SHM);         s = 'wl_shm'
    case (IF_SHM_POOL);    s = 'wl_shm_pool'
    case (IF_BUFFER);      s = 'wl_buffer'
    case (IF_SURFACE);     s = 'wl_surface'
    case (IF_REGION);      s = 'wl_region'
    case (IF_OUTPUT);      s = 'wl_output'
    case (IF_SEAT);        s = 'wl_seat'
    case (IF_WM_BASE);     s = 'xdg_wm_base'
    case (IF_POSITIONER);  s = 'xdg_positioner'
    case (IF_XDG_SURFACE); s = 'xdg_surface'
    case (IF_TOPLEVEL);    s = 'xdg_toplevel'
    case (IF_POPUP);       s = 'xdg_popup'
    case (IF_POINTER);     s = 'wl_pointer'
    case (IF_KEYBOARD);    s = 'wl_keyboard'
    case (IF_SUBCOMP);     s = 'wl_subcompositor'
    case (IF_SUBSURF);     s = 'wl_subsurface'
    case (IF_DDM);         s = 'wl_data_device_manager'
    case default;          s = '?'
    end select
  end function

  ! ── incoming wire parsing helpers ─────────────────────────────────────

  function ru32(ci, pos) result(v)
    integer, intent(in) :: ci, pos
    integer(4) :: v
    v = transfer(clients(ci)%rbuf(pos:pos+3), 0_4)
  end function

  subroutine rstr(ci, pos, s)
    integer, intent(in) :: ci
    integer, intent(inout) :: pos
    character(*), intent(out) :: s
    integer :: l, i, n
    l = ru32(ci, pos)
    pos = pos + 4
    s = ' '
    n = min(l - 1, len(s))
    do i = 1, n
      s(i:i) = achar(iand(int(clients(ci)%rbuf(pos+i-1)), 255))
    end do
    pos = pos + shiftl(shiftr(l + 3, 2), 2)
  end subroutine

  function pop_fd(ci) result(fd)
    integer, intent(in) :: ci
    integer(c_int) :: fd
    integer :: i
    fd = -1
    if (clients(ci)%nfdq < 1) return
    fd = clients(ci)%fdq(1)
    do i = 1, clients(ci)%nfdq - 1
      clients(ci)%fdq(i) = clients(ci)%fdq(i+1)
    end do
    clients(ci)%nfdq = clients(ci)%nfdq - 1
  end function

  ! ── slot allocators / stacking ────────────────────────────────────────

  function new_surf(ci, sid) result(si)
    integer, intent(in) :: ci, sid
    integer :: si, i
    si = 0
    do i = 1, size(surfs)
      if (.not. surfs(i)%used) then
        si = i
        exit
      end if
    end do
    if (si == 0) return
    surfs(si) = surf_t()
    surfs(si)%used = .true.
    surfs(si)%ci = ci
    surfs(si)%sid = sid
  end function

  subroutine zpush(si)
    integer, intent(in) :: si
    if (nz < size(zlist)) then
      nz = nz + 1
      zlist(nz) = si
    end if
  end subroutine

  subroutine zremove(si)
    integer, intent(in) :: si
    integer :: i, j
    do i = 1, nz
      if (zlist(i) == si) then
        do j = i, nz - 1
          zlist(j) = zlist(j+1)
        end do
        nz = nz - 1
        return
      end if
    end do
  end subroutine

  subroutine zraise(si)
    integer, intent(in) :: si
    call zremove(si)
    call zpush(si)
    needs_paint = .true.
  end subroutine

  ! forget a surface everywhere the input/stacking state may reference it
  subroutine drop_surface(si)
    integer, intent(in) :: si
    integer :: i
    call zremove(si)
    if (kfocus == si) kfocus = 0
    if (pfocus == si) pfocus = 0
    if (drag_si == si) drag_si = 0
    do i = 1, size(surfs)
      if (surfs(i)%used .and. surfs(i)%parent_si == si) surfs(i)%parent_si = 0
    end do
    needs_paint = .true.
  end subroutine

  ! ── registry greetings ────────────────────────────────────────────────

  subroutine send_global(ci, regid, name, iface, ver)
    integer, intent(in) :: ci, regid, name, ver
    character(*), intent(in) :: iface
    call mreset()
    call mput_u(name)
    call mput_s(iface)
    call mput_u(ver)
    call msend(ci, regid, 0)
  end subroutine

  subroutine send_globals(ci, regid)
    integer, intent(in) :: ci, regid
    call send_global(ci, regid, G_COMPOSITOR, 'wl_compositor', 4)
    call send_global(ci, regid, G_SHM, 'wl_shm', 1)
    call send_global(ci, regid, G_SEAT, 'wl_seat', 7)
    call send_global(ci, regid, G_OUTPUT, 'wl_output', 2)
    call send_global(ci, regid, G_WM_BASE, 'xdg_wm_base', 1)
    call send_global(ci, regid, G_SUBCOMP, 'wl_subcompositor', 1)
    call send_global(ci, regid, G_DDM, 'wl_data_device_manager', 3)
  end subroutine

  subroutine greet_output(ci, id, ver)
    integer, intent(in) :: ci, id, ver
    call mreset()
    call mput_u(0); call mput_u(0)
    call mput_u(271); call mput_u(170)
    call mput_u(0)
    call mput_s('fabland')
    call mput_s('FORTRAN-CRT-77')
    call mput_u(0)
    call msend(ci, id, 0)                 ! geometry
    call mreset()
    call mput_u(3); call mput_u(OUTW); call mput_u(OUTH); call mput_u(60000)
    call msend(ci, id, 1)                 ! mode
    if (ver >= 2) then
      call emit1(ci, id, 3, 1)            ! scale
      call emit0(ci, id, 2)               ! done
    end if
  end subroutine

  subroutine greet_seat(ci, id, ver)
    integer, intent(in) :: ci, id, ver
    call emit1(ci, id, 0, 3)              ! capabilities: pointer | keyboard
    if (ver >= 2) then
      call mreset()
      call mput_s('fabland-seat0')
      call msend(ci, id, 1)               ! name
    end if
  end subroutine

  ! ── keyboard / pointer event senders ──────────────────────────────────

  subroutine kbd_send_keymap(ci)
    integer, intent(in) :: ci
    call mreset()
    call mput_u(1)                        ! format: xkb_v1
    call mput_u(keymap_sz)
    call msend_fd(ci, clients(ci)%kbd_id, 0, keymap_fd)
    if (clients(ci)%kbd_ver >= 4) then
      call mreset()
      call mput_u(0); call mput_u(0)      ! repeat disabled (terminal repeats)
      call msend(ci, clients(ci)%kbd_id, 5)
    end if
  end subroutine

  subroutine kbd_enter(si)
    integer, intent(in) :: si
    integer :: ci
    ci = surfs(si)%ci
    if (clients(ci)%kbd_id == 0) return
    call mreset()
    call mput_u(next_serial())
    call mput_u(surfs(si)%sid)
    call mput_u(0)                        ! keys: empty array
    call msend(ci, clients(ci)%kbd_id, 1)
    call kbd_modifiers(ci, 0)
  end subroutine

  subroutine kbd_leave(si)
    integer, intent(in) :: si
    integer :: ci
    ci = surfs(si)%ci
    if (.not. clients(ci)%used .or. clients(ci)%kbd_id == 0) return
    call mreset()
    call mput_u(next_serial())
    call mput_u(surfs(si)%sid)
    call msend(ci, clients(ci)%kbd_id, 2)
  end subroutine

  subroutine kbd_modifiers(ci, mods)
    integer, intent(in) :: ci, mods
    call mreset()
    call mput_u(next_serial())
    call mput_u(mods); call mput_u(0); call mput_u(0); call mput_u(0)
    call msend(ci, clients(ci)%kbd_id, 4)
  end subroutine

  subroutine kbd_key(ci, code, state)
    integer, intent(in) :: ci, code, state
    call mreset()
    call mput_u(next_serial())
    call mput_u(tms32())
    call mput_u(code)
    call mput_u(state)
    call msend(ci, clients(ci)%kbd_id, 3)
  end subroutine

  subroutine set_kfocus(si)
    integer, intent(in) :: si
    if (si == kfocus) return
    if (kfocus > 0) then
      if (surfs(kfocus)%used) call kbd_leave(kfocus)
    end if
    kfocus = si
    if (si > 0) call kbd_enter(si)
    needs_paint = .true.
  end subroutine

  ! deliver one keystroke (press+release with modifiers) to the focus
  subroutine send_key(code, mods)
    integer, intent(in) :: code, mods
    integer :: ci
    if (code == 0 .or. kfocus == 0) return
    if (.not. surfs(kfocus)%used) return
    ci = surfs(kfocus)%ci
    if (clients(ci)%kbd_id == 0) return
    if (mods /= 0) call kbd_modifiers(ci, mods)
    call kbd_key(ci, code, 1)
    call kbd_key(ci, code, 0)
    if (mods /= 0) call kbd_modifiers(ci, 0)
  end subroutine

  subroutine ptr_frame(ci)
    integer, intent(in) :: ci
    if (clients(ci)%ptr_ver >= 5) call emit0(ci, clients(ci)%ptr_id, 5)
  end subroutine

  subroutine ptr_enter(si, sx, sy)
    integer, intent(in) :: si, sx, sy
    integer :: ci
    ci = surfs(si)%ci
    if (clients(ci)%ptr_id == 0) return
    call mreset()
    call mput_u(next_serial())
    call mput_u(surfs(si)%sid)
    call mput_u(sx * 256); call mput_u(sy * 256)
    call msend(ci, clients(ci)%ptr_id, 0)
    call ptr_frame(ci)
  end subroutine

  subroutine ptr_leave(si)
    integer, intent(in) :: si
    integer :: ci
    ci = surfs(si)%ci
    if (.not. clients(ci)%used .or. clients(ci)%ptr_id == 0) return
    call mreset()
    call mput_u(next_serial())
    call mput_u(surfs(si)%sid)
    call msend(ci, clients(ci)%ptr_id, 1)
    call ptr_frame(ci)
  end subroutine

  subroutine ptr_motion(si, sx, sy)
    integer, intent(in) :: si, sx, sy
    integer :: ci
    ci = surfs(si)%ci
    if (clients(ci)%ptr_id == 0) return
    call mreset()
    call mput_u(tms32())
    call mput_u(sx * 256); call mput_u(sy * 256)
    call msend(ci, clients(ci)%ptr_id, 2)
    call ptr_frame(ci)
  end subroutine

  subroutine ptr_button(si, code, state)
    integer, intent(in) :: si, code, state
    integer :: ci
    ci = surfs(si)%ci
    if (clients(ci)%ptr_id == 0) return
    call mreset()
    call mput_u(next_serial())
    call mput_u(tms32())
    call mput_u(code)
    call mput_u(state)
    call msend(ci, clients(ci)%ptr_id, 3)
    call ptr_frame(ci)
  end subroutine

  subroutine ptr_axis(si, val256)
    integer, intent(in) :: si, val256
    integer :: ci
    ci = surfs(si)%ci
    if (clients(ci)%ptr_id == 0) return
    call mreset()
    call mput_u(tms32())
    call mput_u(0)                        ! vertical scroll
    call mput_u(val256)
    call msend(ci, clients(ci)%ptr_id, 4)
    call ptr_frame(ci)
  end subroutine

  ! ── hit testing / interactive input ───────────────────────────────────

  ! topmost mapped toplevel whose content contains (x, y); 0 if none
  function hit_content(x, y) result(si)
    integer, intent(in) :: x, y
    integer :: si, zi, s
    si = 0
    do zi = nz, 1, -1
      s = zlist(zi)
      if (.not. (surfs(s)%used .and. surfs(s)%mapped)) cycle
      if (x >= surfs(s)%x .and. x < surfs(s)%x + surfs(s)%w .and. &
          y >= surfs(s)%y .and. y < surfs(s)%y + surfs(s)%h) then
        si = s
        return
      end if
    end do
  end function

  ! topmost window whose frame (incl. titlebar) contains the point
  function hit_frame(x, y) result(si)
    integer, intent(in) :: x, y
    integer :: si, zi, s, y0
    si = 0
    do zi = nz, 1, -1
      s = zlist(zi)
      if (.not. (surfs(s)%used .and. surfs(s)%mapped)) cycle
      y0 = surfs(s)%y
      if (.not. surfs(s)%has_csd) y0 = surfs(s)%y - 30
      if (x >= surfs(s)%x - 2 .and. x < surfs(s)%x + surfs(s)%w + 2 .and. &
          y >= y0 .and. y < surfs(s)%y + surfs(s)%h + 2) then
        si = s
        return
      end if
    end do
  end function

  subroutine pointer_moved(px, py)
    integer, intent(in) :: px, py
    integer :: si
    ptr_x = px
    ptr_y = py
    needs_paint = .true.
    if (drag_si > 0) then
      if (surfs(drag_si)%used) then
        surfs(drag_si)%x = px - drag_dx
        surfs(drag_si)%y = max(32, py - drag_dy)
      end if
      return
    end if
    si = hit_content(px, py)
    if (si /= pfocus) then
      if (pfocus > 0) then
        if (surfs(pfocus)%used) call ptr_leave(pfocus)
      end if
      pfocus = si
      if (si > 0) call ptr_enter(si, px - surfs(si)%x, py - surfs(si)%y)
    else if (si > 0) then
      call ptr_motion(si, px - surfs(si)%x, py - surfs(si)%y)
    end if
  end subroutine

  subroutine pointer_press(btn, ctrl_held)
    integer, intent(in) :: btn
    logical, intent(in) :: ctrl_held
    integer :: si, code
    si = hit_frame(ptr_x, ptr_y)
    if (si == 0) return
    call zraise(si)
    call set_kfocus(si)
    ! ctrl+drag moves any window; titlebar drag moves decorated ones
    if (btn == 0 .and. ctrl_held) then
      drag_si = si
      drag_dx = ptr_x - surfs(si)%x
      drag_dy = ptr_y - surfs(si)%y
      return
    end if
    if (.not. surfs(si)%has_csd .and. ptr_y < surfs(si)%y) then
      ! titlebar: close dot or drag
      if ((ptr_x - (surfs(si)%x + 14))**2 + (ptr_y - (surfs(si)%y - 14))**2 <= 64) then
        if (surfs(si)%top_id /= 0) call emit0(surfs(si)%ci, surfs(si)%top_id, 1)  ! close
        return
      end if
      if (btn == 0) then
        drag_si = si
        drag_dx = ptr_x - surfs(si)%x
        drag_dy = ptr_y - surfs(si)%y
      end if
      return
    end if
    ! content: forward
    if (si == hit_content(ptr_x, ptr_y)) then
      select case (btn)
      case (0); code = BTN_LEFT
      case (1); code = BTN_MIDDLE
      case default; code = BTN_RIGHT
      end select
      call ptr_button(si, code, 1)
    end if
  end subroutine

  subroutine pointer_release(btn)
    integer, intent(in) :: btn
    integer :: code
    if (drag_si > 0 .and. btn == 0) then
      drag_si = 0
      return
    end if
    if (pfocus > 0) then
      if (surfs(pfocus)%used) then
        select case (btn)
        case (0); code = BTN_LEFT
        case (1); code = BTN_MIDDLE
        case default; code = BTN_RIGHT
        end select
        call ptr_button(pfocus, code, 0)
      end if
    end if
  end subroutine

  subroutine handle_events(evq, nev)
    type(tev), intent(in) :: evq(:)
    integer, intent(in) :: nev
    integer :: i, code, mods
    integer, parameter :: spkey(10) = [103, 108, 106, 105, 102, 107, 104, 109, 110, 111]
    do i = 1, nev
      select case (evq(i)%k)
      case (TK_CHAR)
        if (evq(i)%b == 1 .and. evq(i)%a == 17) then   ! ctrl-alt-q
          want_quit = .true.
          cycle
        end if
        call char_to_key(evq(i)%a, code, mods)
        if (evq(i)%b == 1) mods = ior(mods, MOD_ALT)
        call send_key(code, mods)
      case (TK_SPECIAL)
        select case (evq(i)%a)
        case (20)          ! F10: quit
          want_quit = .true.
        case (22)          ! F12: screenshot
          want_shot = .true.
          needs_paint = .true.
        case (11:19)       ! F1..F9
          call send_key(58 + evq(i)%a - 10, 0)
        case (1:10)
          call send_key(spkey(evq(i)%a), 0)
        end select
      case (TK_MOTION)
        call pointer_moved(evq(i)%a, evq(i)%b)
      case (TK_PRESS)
        call pointer_press(iand(evq(i)%a, 3), iand(evq(i)%a, 16) /= 0)
      case (TK_RELEASE)
        call pointer_release(iand(evq(i)%a, 3))
      case (TK_WHEEL)
        if (pfocus > 0) then
          if (surfs(pfocus)%used) call ptr_axis(pfocus, evq(i)%a * 2560)
        end if
      end select
    end do
  end subroutine

  ! ── xdg configure ─────────────────────────────────────────────────────

  subroutine send_configure(si)
    integer, intent(in) :: si
    integer :: ci
    ci = surfs(si)%ci
    call mreset()
    call mput_u(0); call mput_u(0); call mput_u(0)   ! w, h, states[]
    call msend(ci, surfs(si)%top_id, 0)
    call emit1(ci, surfs(si)%xdg_id, 0, next_serial())
    surfs(si)%configured = .true.
  end subroutine

  ! ── buffer commit ─────────────────────────────────────────────────────

  subroutine latch_buffer(si, bufid)
    integer, intent(in) :: si, bufid
    integer :: ci, oi, bi, pi, x, y, base
    integer(4), pointer :: p32(:)
    integer(4) :: v
    logical :: first_map
    ci = surfs(si)%ci
    oi = find_obj(ci, bufid)
    if (oi == 0) then
      call logmsg('latch: unknown buffer id '//itoa(bufid))
      return
    end if
    bi = clients(ci)%objs(oi)%datai
    if (bi < 1 .or. .not. bufs(bi)%used) then
      call logmsg('latch: stale buffer slot')
      return
    end if
    pi = bufs(bi)%pooli
    if (.not. c_associated(pools(pi)%mem)) then
      call logmsg('latch: pool not mapped')
      return
    end if
    if (bufs(bi)%off + bufs(bi)%h * bufs(bi)%stride > int(pools(pi)%sz)) then
      call logmsg('latch: buffer exceeds pool ('//trim(itoa(bufs(bi)%off))//'+'// &
        trim(itoa(bufs(bi)%h))//'x'//trim(itoa(bufs(bi)%stride))//' > '// &
        trim(itoa(int(pools(pi)%sz)))//')')
      return
    end if

    call c_f_pointer(pools(pi)%mem, p32, [int(pools(pi)%sz / 4)])
    if (allocated(surfs(si)%tex)) deallocate(surfs(si)%tex)
    allocate(surfs(si)%tex(bufs(bi)%w, bufs(bi)%h))
    do y = 1, bufs(bi)%h
      base = (bufs(bi)%off + (y-1) * bufs(bi)%stride) / 4
      do x = 1, bufs(bi)%w
        v = p32(base + x)
        if (bufs(bi)%fmt == 1) v = ior(iand(v, RGBMASK), AMASK)
        surfs(si)%tex(x, y) = v
      end do
    end do
    surfs(si)%w = bufs(bi)%w
    surfs(si)%h = bufs(bi)%h
    first_map = .not. surfs(si)%mapped
    surfs(si)%mapped = .true.
    if (first_map) then
      if (surfs(si)%top_id /= 0) then
        ! keep new windows on screen
        surfs(si)%x = max(8, min(surfs(si)%x, OUTW - surfs(si)%w - 8))
        surfs(si)%y = max(34, min(surfs(si)%y, OUTH - surfs(si)%h - 8))
      end if
      call logmsg('mapped surface '//trim(itoa(surfs(si)%sid))//' ('// &
        trim(itoa(surfs(si)%w))//'x'//trim(itoa(surfs(si)%h))//')')
    end if
    call emit0(ci, bufid, 0)              ! wl_buffer.release
    if (first_map .and. surfs(si)%top_id /= 0) call set_kfocus(si)  ! focus on map
  end subroutine

  ! ── request dispatch ──────────────────────────────────────────────────

  subroutine dispatch(ci, oid, opc, ap)
    integer, intent(in) :: ci, oid, opc, ap
    integer :: oi, iface, ver, p, nid, si, i, k
    integer :: a1, a2, a3, a4, a5
    integer(c_int) :: pfd
    character(64) :: bindname
    character(128) :: str
    type(c_ptr) :: mem
    integer :: gname, gver, pi, bi2

    ver = 1
    if (oid == 1) then
      iface = IF_DISPLAY
      oi = 0
    else
      oi = find_obj(ci, oid)
      if (oi == 0) then
        if (debug) call logmsg('  ? request on unknown object '//itoa(oid))
        return
      end if
      iface = clients(ci)%objs(oi)%iface
      ver = clients(ci)%objs(oi)%ver
    end if

    if (debug) call logmsg('  <- '//trim(iface_name(iface))//'@'//trim(itoa(oid)) &
      //' op '//itoa(opc))

    select case (iface)

    case (IF_DISPLAY)
      select case (opc)
      case (0)   ! sync
        nid = ru32(ci, ap)
        call emit1(ci, nid, 0, next_serial())
        call send_delete_id(ci, nid)
      case (1)   ! get_registry
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_REGISTRY, 1, 0)
        call send_globals(ci, nid)
      end select

    case (IF_REGISTRY)
      if (opc == 0) then   ! bind
        gname = ru32(ci, ap)
        p = ap + 4
        call rstr(ci, p, bindname)
        gver = ru32(ci, p)
        nid = ru32(ci, p + 4)
        select case (gname)
        case (G_COMPOSITOR)
          call add_obj(ci, nid, IF_COMPOSITOR, gver, 0)
        case (G_SHM)
          call add_obj(ci, nid, IF_SHM, gver, 0)
          call emit1(ci, nid, 0, 0)
          call emit1(ci, nid, 0, 1)
        case (G_SEAT)
          call add_obj(ci, nid, IF_SEAT, gver, 0)
          call greet_seat(ci, nid, gver)
        case (G_OUTPUT)
          call add_obj(ci, nid, IF_OUTPUT, gver, 0)
          call greet_output(ci, nid, gver)
        case (G_WM_BASE)
          call add_obj(ci, nid, IF_WM_BASE, gver, 0)
        case (G_SUBCOMP)
          call add_obj(ci, nid, IF_SUBCOMP, gver, 0)
        case (G_DDM)
          call add_obj(ci, nid, IF_DDM, gver, 0)
        end select
        call logmsg('client '//trim(itoa(ci))//' bound '//trim(bindname) &
          //' v'//itoa(gver))
      end if

    case (IF_COMPOSITOR)
      select case (opc)
      case (0)
        nid = ru32(ci, ap)
        si = new_surf(ci, nid)
        call add_obj(ci, nid, IF_SURFACE, ver, si)
      case (1)
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_REGION, ver, 0)
      end select

    case (IF_REGION)
      if (opc == 0) call del_obj(ci, oid)

    case (IF_SHM)
      if (opc == 0) then
        nid = ru32(ci, ap)
        a1 = ru32(ci, ap + 4)
        pfd = pop_fd(ci)
        pi = 0
        do i = 1, size(pools)
          if (.not. pools(i)%used) then
            pi = i
            exit
          end if
        end do
        if (pi == 0 .or. pfd < 0) return
        mem = c_mmap(c_null_ptr, int(a1, c_size_t), PROT_READ, MAP_SHARED, pfd, 0_c_long)
        pools(pi) = pool_t(.true., ci, pfd, int(a1, c_size_t), mem)
        call add_obj(ci, nid, IF_SHM_POOL, 1, pi)
        call logmsg('client '//trim(itoa(ci))//' shm pool ('// &
          trim(itoa(a1))//' bytes, fd '//trim(itoa(int(pfd)))//' via SCM_RIGHTS)')
      end if

    case (IF_SHM_POOL)
      pi = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)
        nid = ru32(ci, ap)
        a1 = ru32(ci, ap+4);  a2 = ru32(ci, ap+8)
        a3 = ru32(ci, ap+12); a4 = ru32(ci, ap+16); a5 = ru32(ci, ap+20)
        bi2 = 0
        do i = 1, size(bufs)
          if (.not. bufs(i)%used) then
            bi2 = i
            exit
          end if
        end do
        if (bi2 == 0) return
        bufs(bi2) = buf_t(.true., ci, pi, a1, a2, a3, a4, a5)
        call add_obj(ci, nid, IF_BUFFER, 1, bi2)
      case (1)
        call del_obj(ci, oid)
      case (2)
        a1 = ru32(ci, ap)
        if (c_associated(pools(pi)%mem)) k = c_munmap(pools(pi)%mem, pools(pi)%sz)
        pools(pi)%mem = c_mmap(c_null_ptr, int(a1, c_size_t), PROT_READ, MAP_SHARED, &
                               pools(pi)%fd, 0_c_long)
        pools(pi)%sz = int(a1, c_size_t)
      end select

    case (IF_BUFFER)
      if (opc == 0) then
        bufs(clients(ci)%objs(oi)%datai)%used = .false.
        call del_obj(ci, oid)
      end if

    case (IF_SURFACE)
      si = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)   ! destroy
        if (si > 0) then
          call drop_surface(si)
          if (allocated(surfs(si)%tex)) deallocate(surfs(si)%tex)
          surfs(si)%used = .false.
        end if
        call del_obj(ci, oid)
      case (1)   ! attach
        if (si > 0) surfs(si)%pend_buf = ru32(ci, ap)
      case (3)   ! frame
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_CALLBACK, 1, si)
        if (si > 0) then
          if (surfs(si)%ncbs < size(surfs(si)%cbs)) then
            surfs(si)%ncbs = surfs(si)%ncbs + 1
            surfs(si)%cbs(surfs(si)%ncbs) = nid
          end if
        end if
      case (6)   ! commit
        if (si <= 0) return
        if (surfs(si)%pend_buf > 0) then
          call latch_buffer(si, surfs(si)%pend_buf)
          needs_paint = .true.
        else if (surfs(si)%pend_buf == 0) then
          surfs(si)%mapped = .false.
          needs_paint = .true.
        end if
        surfs(si)%pend_buf = -1
        if (surfs(si)%top_id /= 0 .and. .not. surfs(si)%configured) then
          call send_configure(si)
        end if
      case default
      end select

    case (IF_CALLBACK)

    case (IF_SEAT)
      select case (opc)
      case (0)   ! get_pointer
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_POINTER, ver, 0)
        clients(ci)%ptr_id = nid
        clients(ci)%ptr_ver = ver
      case (1)   ! get_keyboard
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_KEYBOARD, ver, 0)
        clients(ci)%kbd_id = nid
        clients(ci)%kbd_ver = ver
        call kbd_send_keymap(ci)
        if (kfocus > 0) then
          if (surfs(kfocus)%used .and. surfs(kfocus)%ci == ci) call kbd_enter(kfocus)
        end if
      case (2)   ! get_touch
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_TOUCH, ver, 0)
      case (3)
        call del_obj(ci, oid)
      end select

    case (IF_POINTER)
      select case (opc)
      case (0)   ! set_cursor: politely ignored
      case (1)
        if (clients(ci)%ptr_id == oid) clients(ci)%ptr_id = 0
        call del_obj(ci, oid)
      end select

    case (IF_KEYBOARD)
      if (opc == 0) then
        if (clients(ci)%kbd_id == oid) clients(ci)%kbd_id = 0
        call del_obj(ci, oid)
      end if

    case (IF_TOUCH)
      if (opc == 0) call del_obj(ci, oid)

    case (IF_OUTPUT)
      if (opc == 0) call del_obj(ci, oid)

    case (IF_WM_BASE)
      select case (opc)
      case (0)
        call del_obj(ci, oid)
      case (1)
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_POSITIONER, 1, 0)
      case (2)
        nid = ru32(ci, ap)
        a1 = ru32(ci, ap + 4)
        i = find_obj(ci, a1)
        si = 0
        if (i > 0) si = clients(ci)%objs(i)%datai
        if (si > 0) surfs(si)%xdg_id = nid
        call add_obj(ci, nid, IF_XDG_SURFACE, 1, si)
      case (3)   ! pong
      end select

    case (IF_POSITIONER)
      if (opc == 0) call del_obj(ci, oid)

    case (IF_XDG_SURFACE)
      si = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)
        if (si > 0) then
          surfs(si)%xdg_id = 0
          surfs(si)%configured = .false.
          surfs(si)%mapped = .false.
          call drop_surface(si)
        end if
        call del_obj(ci, oid)
      case (1)   ! get_toplevel
        nid = ru32(ci, ap)
        if (si > 0) then
          surfs(si)%top_id = nid
          surfs(si)%x = 72 + mod(nwindows * 96, OUTW - 480)
          surfs(si)%y = 96 + mod(nwindows * 72, OUTH - 400)
          nwindows = nwindows + 1
          call zpush(si)
        end if
        call add_obj(ci, nid, IF_TOPLEVEL, 1, si)
      case (2)   ! get_popup
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_POPUP, 1, si)
      case (3)   ! set_window_geometry: the client decorates itself
        if (si > 0) surfs(si)%has_csd = .true.
      case (4)
        if (si > 0) surfs(si)%acked = .true.
      end select

    case (IF_TOPLEVEL)
      si = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)
        if (si > 0) then
          surfs(si)%top_id = 0
          surfs(si)%mapped = .false.
          call drop_surface(si)
        end if
        call del_obj(ci, oid)
      case (2)
        p = ap
        call rstr(ci, p, str)
        if (si > 0) surfs(si)%title = str
        call logmsg('window title: "'//trim(str)//'"')
      case (3)
        p = ap
        call rstr(ci, p, str)
        call logmsg('app id: "'//trim(str)//'"')
      case default
      end select

    case (IF_POPUP)
      if (opc == 0) call del_obj(ci, oid)

    case (IF_SUBCOMP)
      select case (opc)
      case (0)
        call del_obj(ci, oid)
      case (1)   ! get_subsurface(new_id, surface, parent)
        nid = ru32(ci, ap)
        a1 = ru32(ci, ap + 4)
        a2 = ru32(ci, ap + 8)
        si = 0
        i = find_obj(ci, a1)
        if (i > 0) si = clients(ci)%objs(i)%datai
        k = 0
        i = find_obj(ci, a2)
        if (i > 0) k = clients(ci)%objs(i)%datai
        if (si > 0) surfs(si)%parent_si = k
        call add_obj(ci, nid, IF_SUBSURF, 1, si)
      end select

    case (IF_SUBSURF)
      si = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)
        if (si > 0) surfs(si)%parent_si = 0
        call del_obj(ci, oid)
      case (1)   ! set_position
        if (si > 0) then
          surfs(si)%subx = ru32(ci, ap)
          surfs(si)%suby = ru32(ci, ap + 4)
        end if
      case default   ! place_above/below, sync/desync
      end select

    case (IF_DDM)
      select case (opc)
      case (0)
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_DATA_SRC, 1, 0)
      case (1)
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_DATA_DEV, 1, 0)
      end select

    case (IF_DATA_DEV)
      if (opc == 2) call del_obj(ci, oid)   ! release; drag/selection ignored

    case (IF_DATA_SRC)
      if (opc == 1) call del_obj(ci, oid)   ! destroy; offer/actions ignored

    case (IF_DUMMY)

    end select
  end subroutine

  ! ── per-client stream parsing ─────────────────────────────────────────

  subroutine parse_client(ci)
    integer, intent(in) :: ci
    integer :: off, id, word, msize, opc
    off = 0
    do
      if (clients(ci)%dead) exit
      if (clients(ci)%rlen - off < 8) exit
      id = ru32(ci, off + 1)
      word = ru32(ci, off + 5)
      msize = iand(shiftr(word, 16), 65535)
      opc = iand(word, 65535)
      if (msize < 8) then
        call logmsg('protocol error from client '//itoa(ci))
        clients(ci)%dead = .true.
        exit
      end if
      if (clients(ci)%rlen - off < msize) exit
      call dispatch(ci, id, opc, off + 9)
      off = off + msize
    end do
    if (off > 0) then
      if (clients(ci)%rlen > off) then
        clients(ci)%rbuf(1:clients(ci)%rlen - off) = &
          clients(ci)%rbuf(off + 1:clients(ci)%rlen)
      end if
      clients(ci)%rlen = clients(ci)%rlen - off
    end if
  end subroutine

  subroutine disconnect(ci)
    integer, intent(in) :: ci
    integer :: i, st
    do i = 1, size(surfs)
      if (surfs(i)%used .and. surfs(i)%ci == ci) then
        call drop_surface(i)
        if (allocated(surfs(i)%tex)) deallocate(surfs(i)%tex)
        surfs(i)%used = .false.
      end if
    end do
    do i = 1, size(pools)
      if (pools(i)%used .and. pools(i)%ci == ci) then
        if (c_associated(pools(i)%mem)) st = c_munmap(pools(i)%mem, pools(i)%sz)
        st = c_close(pools(i)%fd)
        pools(i)%used = .false.
      end if
    end do
    do i = 1, size(bufs)
      if (bufs(i)%used .and. bufs(i)%ci == ci) bufs(i)%used = .false.
    end do
    do i = 1, clients(ci)%nfdq
      st = c_close(clients(ci)%fdq(i))
    end do
    st = c_close(clients(ci)%fd)
    clients(ci) = client_t()
    needs_paint = .true.
    call logmsg('client '//trim(itoa(ci))//' disconnected')
  end subroutine

  ! ── rendering ─────────────────────────────────────────────────────────

  subroutine blend_px(x, y, v)
    integer, intent(in) :: x, y
    integer(4), intent(in) :: v
    integer :: a, r, g, b, rd, gd, bd
    if (x < 1 .or. x > OUTW .or. y < 1 .or. y > OUTH) return
    a = iand(shiftr(v, 24), 255)
    if (a == 255) then
      canvas(x, y) = iand(v, RGBMASK)
    else if (a > 0) then
      r = iand(shiftr(v, 16), 255)
      g = iand(shiftr(v, 8), 255)
      b = iand(v, 255)
      rd = iand(shiftr(canvas(x, y), 16), 255)
      gd = iand(shiftr(canvas(x, y), 8), 255)
      bd = iand(canvas(x, y), 255)
      r = (a * r + (255 - a) * rd) / 255
      g = (a * g + (255 - a) * gd) / 255
      b = (a * b + (255 - a) * bd) / 255
      canvas(x, y) = ior(shiftl(r, 16), ior(shiftl(g, 8), b))
    end if
  end subroutine

  subroutine fill_rect(x0, y0, x1, y1, col)
    integer, intent(in) :: x0, y0, x1, y1
    integer(4), intent(in) :: col
    integer :: x, y
    do y = max(1, y0), min(OUTH, y1)
      do x = max(1, x0), min(OUTW, x1)
        canvas(x, y) = col
      end do
    end do
  end subroutine

  subroutine dim_rect(x0, y0, x1, y1, keep256)
    integer, intent(in) :: x0, y0, x1, y1, keep256
    integer :: x, y, r, g, b
    integer(4) :: v
    do y = max(1, y0), min(OUTH, y1)
      do x = max(1, x0), min(OUTW, x1)
        v = canvas(x, y)
        r = iand(shiftr(v, 16), 255) * keep256 / 256
        g = iand(shiftr(v, 8), 255) * keep256 / 256
        b = iand(v, 255) * keep256 / 256
        canvas(x, y) = ior(shiftl(r, 16), ior(shiftl(g, 8), b))
      end do
    end do
  end subroutine

  subroutine draw_dot(cx, cy, rad, col)
    integer, intent(in) :: cx, cy, rad
    integer(4), intent(in) :: col
    integer :: x, y
    do y = cy - rad, cy + rad
      do x = cx - rad, cx + rad
        if ((x-cx)**2 + (y-cy)**2 <= rad*rad) then
          if (x >= 1 .and. x <= OUTW .and. y >= 1 .and. y <= OUTH) canvas(x, y) = col
        end if
      end do
    end do
  end subroutine

  subroutine draw_wordmark()
    character(5) :: gf(7), ga(7), gb(7), gl(7), gn(7), gd(7)
    character(5) :: glyph(7)
    character(7), parameter :: word = 'fabland'
    integer, parameter :: SC = 4
    integer :: xo, yo, i, gx, gy, px, py
    integer(4), parameter :: ink = 4079195
    gf = [character(5) :: '..XX.', '.X...', 'XXX..', '.X...', '.X...', '.X...', '.X...']
    ga = [character(5) :: '.....', '.....', '.XXX.', '....X', '.XXXX', 'X...X', '.XXXX']
    gb = [character(5) :: 'X....', 'X....', 'X.XX.', 'XX..X', 'X...X', 'XX..X', 'X.XX.']
    gl = [character(5) :: '.X...', '.X...', '.X...', '.X...', '.X...', '.X...', '..XX.']
    gn = [character(5) :: '.....', '.....', 'X.XX.', 'XX..X', 'X...X', 'X...X', 'X...X']
    gd = [character(5) :: '....X', '....X', '.XX.X', 'X..XX', 'X...X', 'X..XX', '.XX.X']
    xo = OUTW - 7 * 6 * SC - 24
    yo = OUTH - 7 * SC - 20
    do i = 1, 7
      select case (word(i:i))
      case ('f'); glyph = gf
      case ('a'); glyph = ga
      case ('b'); glyph = gb
      case ('l'); glyph = gl
      case ('n'); glyph = gn
      case ('d'); glyph = gd
      end select
      do gy = 1, 7
        do gx = 1, 5
          if (glyph(gy)(gx:gx) == 'X') then
            do py = 0, SC - 1
              do px = 0, SC - 1
                canvas(xo + (i-1)*6*SC + (gx-1)*SC + px, yo + (gy-1)*SC + py) = ink
              end do
            end do
          end if
        end do
      end do
    end do
  end subroutine

  subroutine draw_window(si)
    integer, intent(in) :: si
    integer :: x, y, sx, sy
    integer(4) :: border
    sx = surfs(si)%x
    sy = surfs(si)%y
    if (.not. surfs(si)%has_csd) then
      call dim_rect(sx + 8, sy - 28 + 8, sx + surfs(si)%w + 8, sy + surfs(si)%h + 8, 150)
      border = 7768263          ! #768ac7 unfocused
      if (si == kfocus) border = 12229367   ! #ba9af7 focused
      call fill_rect(sx - 2, sy - 30, sx + surfs(si)%w + 1, sy + surfs(si)%h + 1, border)
      call fill_rect(sx, sy - 28, sx + surfs(si)%w - 1, sy - 1, 4211275)
      call draw_dot(sx + 14, sy - 14, 5, 16736087)
      call draw_dot(sx + 32, sy - 14, 5, 16694318)
      call draw_dot(sx + 50, sy - 14, 5, 2672704)
    end if
    do y = 1, surfs(si)%h
      do x = 1, surfs(si)%w
        call blend_px(sx + x - 1, sy + y - 1, surfs(si)%tex(x, y))
      end do
    end do
  end subroutine

  subroutine draw_cursor()
    integer :: i, x, y
    integer(4), parameter :: white = 16777215, black = 0
    do i = -6, 6
      x = ptr_x + i
      y = ptr_y
      if (x >= 1 .and. x <= OUTW) then
        canvas(x, max(1, min(OUTH, y-1))) = black
        canvas(x, max(1, min(OUTH, y+1))) = black
        canvas(x, max(1, min(OUTH, y))) = white
      end if
      x = ptr_x
      y = ptr_y + i
      if (y >= 1 .and. y <= OUTH) then
        canvas(max(1, min(OUTW, x-1)), y) = black
        canvas(max(1, min(OUTW, x+1)), y) = black
        canvas(max(1, min(OUTW, x)), y) = white
      end if
    end do
  end subroutine

  subroutine repaint()
    integer :: x, y, zi, si, i
    integer :: r, g, b
    do y = 1, OUTH
      r = 22 + (9 * y) / OUTH
      g = 22 + (13 * y) / OUTH
      b = 30 + (23 * y) / OUTH
      do x = 1, OUTW
        canvas(x, y) = ior(shiftl(r, 16), ior(shiftl(g, 8), b))
      end do
    end do
    do y = 32, OUTH, 32
      do x = 1, OUTW
        canvas(x, y) = canvas(x, y) + 197379
      end do
    end do
    do x = 32, OUTW, 32
      do y = 1, OUTH
        canvas(x, y) = canvas(x, y) + 197379
      end do
    end do
    call draw_wordmark()

    do zi = 1, nz
      si = zlist(zi)
      if (.not. (surfs(si)%used .and. surfs(si)%mapped)) cycle
      if (.not. allocated(surfs(si)%tex)) cycle
      call draw_window(si)
      ! subsurfaces ride on their parent
      do i = 1, size(surfs)
        if (surfs(i)%used .and. surfs(i)%mapped .and. surfs(i)%parent_si == si) then
          if (allocated(surfs(i)%tex)) then
            block
              integer :: xx, yy
              do yy = 1, surfs(i)%h
                do xx = 1, surfs(i)%w
                  call blend_px(surfs(si)%x + surfs(i)%subx + xx - 1, &
                                surfs(si)%y + surfs(i)%suby + yy - 1, surfs(i)%tex(xx, yy))
                end do
              end do
            end block
          end if
        end if
      end do
    end do
    if (term_mode) call draw_cursor()
  end subroutine

  subroutine fire_frame_callbacks(tms)
    integer(8), intent(in) :: tms
    integer :: i, k, ci
    do i = 1, size(surfs)
      if (.not. surfs(i)%used) cycle
      ci = surfs(i)%ci
      do k = 1, surfs(i)%ncbs
        call emit1(ci, surfs(i)%cbs(k), 0, int(mod(tms, 2000000000_8)))
        call del_obj(ci, surfs(i)%cbs(k))
      end do
      surfs(i)%ncbs = 0
    end do
  end subroutine

  function any_callbacks() result(yes)
    logical :: yes
    integer :: i
    yes = .false.
    do i = 1, size(surfs)
      if (surfs(i)%used .and. surfs(i)%ncbs > 0) then
        yes = .true.
        return
      end if
    end do
  end function

  function focused_title() result(t)
    character(64) :: t
    t = ' '
    if (kfocus > 0) then
      if (surfs(kfocus)%used) t = surfs(kfocus)%title(1:64)
    end if
  end function

end module fl_core

! ═══════════════════════════════════════════════════════════════════════

program fabland
  use iso_c_binding
  use fl_libc
  use fl_png
  use fl_xkb
  use fl_term
  use fl_core
  implicit none

  character(256) :: rtdir, disp, sockpath, envbuf
  character(200) :: status
  integer :: st, i, ci, nfds, shot_every, frame_no, shot_no, stdin_slot
  integer(c_int) :: lfd, cfd
  type(pollfd_t) :: pfds(10)
  integer :: pmap(10)
  integer(8) :: n, last_paint, last_term, tnow
  character(64) :: shotname
  type(tev) :: evq(128)
  integer :: nev, nwin, zi

  call get_environment_variable('XDG_RUNTIME_DIR', rtdir, status=st)
  if (st /= 0) rtdir = '/tmp'
  call get_environment_variable('FABLAND_DISPLAY', disp, status=st)
  if (st /= 0 .or. len_trim(disp) == 0) disp = 'fabland-0'
  call get_environment_variable('FABLAND_DEBUG', envbuf, status=st)
  debug = (st == 0 .and. len_trim(envbuf) > 0)

  ! backend: terminal when stdin+stdout are a tty (or forced)
  call get_environment_variable('FABLAND_BACKEND', envbuf, status=st)
  if (st == 0 .and. len_trim(envbuf) > 0) then
    term_mode = (trim(envbuf) == 'term')
  else
    term_mode = (c_isatty(0_c_int) == 1 .and. c_isatty(1_c_int) == 1)
  end if

  shot_every = 0
  if (.not. term_mode) shot_every = 30
  call get_environment_variable('FABLAND_SHOT_EVERY', envbuf, status=st)
  if (st == 0 .and. len_trim(envbuf) > 0) read(envbuf, *) shot_every

  sockpath = trim(rtdir)//'/'//trim(disp)
  call execute_command_line('mkdir -p shots')
  allocate(canvas(OUTW, OUTH))
  call install_signal_handlers()
  call make_keymap_fd(keymap_fd, keymap_sz)

  lfd = make_listen_socket(trim(sockpath))
  if (lfd < 0) then
    call logmsg('FATAL: cannot listen on '//trim(sockpath))
    stop 1
  end if

  if (term_mode) then
    open(newunit=logu, file='fabland.log', status='replace', action='write')
    call term_init()
    call term_probe(OUTW, OUTH)
  end if

  call logmsg('+----------------------------------------------+')
  call logmsg('|  fabland -- a Wayland compositor in Fortran  |')
  call logmsg('+----------------------------------------------+')
  call logmsg('listening on '//trim(sockpath))
  call logmsg('output: '//trim(itoa(OUTW))//'x'//trim(itoa(OUTH))//'@60')
  if (term_mode) then
    call logmsg('backend: terminal (half-block truecolor)')
  else
    call logmsg('backend: headless (PNG frames in ./shots)')
  end if
  call logmsg('run clients with: WAYLAND_DISPLAY='//trim(disp))

  last_paint = now_ms()
  last_term = 0
  frame_no = 0
  shot_no = 0

  do   ! ── main event loop ──
    if (quit_flag /= 0 .or. want_quit) exit

    nfds = 1
    pfds(1)%fd = lfd
    pfds(1)%events = POLLIN
    pfds(1)%revents = 0
    pmap(1) = 0
    stdin_slot = 0
    if (term_mode) then
      nfds = 2
      pfds(2)%fd = 0
      pfds(2)%events = POLLIN
      pfds(2)%revents = 0
      pmap(2) = 0
      stdin_slot = 2
    end if
    do i = 1, size(clients)
      if (clients(i)%used) then
        nfds = nfds + 1
        pfds(nfds)%fd = clients(i)%fd
        pfds(nfds)%events = POLLIN
        pfds(nfds)%revents = 0
        pmap(nfds) = i
      end if
    end do

    st = c_poll(pfds, int(nfds, c_long), 8_c_int)
    if (quit_flag /= 0) exit

    if (iand(int(pfds(1)%revents), int(POLLIN)) /= 0) then
      cfd = c_accept(lfd, c_null_ptr, c_null_ptr)
      if (cfd >= 0) then
        ci = 0
        do i = 1, size(clients)
          if (.not. clients(i)%used) then
            ci = i
            exit
          end if
        end do
        if (ci == 0) then
          st = c_close(cfd)
        else
          clients(ci) = client_t()
          clients(ci)%used = .true.
          clients(ci)%fd = cfd
          call logmsg('client '//trim(itoa(ci))//' connected')
        end if
      end if
    end if

    nev = 0
    if (stdin_slot > 0) then
      if (iand(int(pfds(stdin_slot)%revents), int(POLLIN)) /= 0) then
        call term_read_input(evq, nev, now_ms())
      end if
      call term_tick(evq, nev, now_ms())
    end if
    if (nev > 0) call handle_events(evq, nev)

    do i = 2, nfds
      ci = pmap(i)
      if (ci == 0 .or. .not. clients(ci)%used) cycle
      if (iand(int(pfds(i)%revents), int(POLLIN)) /= 0) then
        n = recv_with_fds(clients(ci)%fd, clients(ci)%rbuf, clients(ci)%rlen, &
                          size(clients(ci)%rbuf), clients(ci)%fdq, clients(ci)%nfdq)
        if (n <= 0) then
          call disconnect(ci)
          cycle
        end if
        clients(ci)%rlen = clients(ci)%rlen + int(n)
        call parse_client(ci)
        if (clients(ci)%dead) then
          call disconnect(ci)
          cycle
        end if
      else if (iand(int(pfds(i)%revents), int(POLLHUP) + int(POLLERR)) /= 0) then
        call disconnect(ci)
      end if
    end do

    tnow = now_ms()
    if ((needs_paint .or. any_callbacks()) .and. tnow - last_paint >= 15) then
      call repaint()
      needs_paint = .false.
      last_paint = tnow
      call fire_frame_callbacks(tnow)
      if (want_shot .or. (shot_every > 0 .and. mod(frame_no, shot_every) == 0)) then
        write(shotname, '(a,i6.6,a)') 'shots/frame-', shot_no, '.png'
        call write_png(trim(shotname), canvas, OUTW, OUTH)
        shot_no = shot_no + 1
        if (want_shot) call logmsg('screenshot: '//trim(shotname))
        want_shot = .false.
      end if
      frame_no = frame_no + 1
    end if

    if (term_mode .and. tnow - last_term >= 66) then
      nwin = 0
      do zi = 1, nz
        if (surfs(zlist(zi))%used .and. surfs(zlist(zi))%mapped) nwin = nwin + 1
      end do
      status = ' fabland (fortran wayland compositor) | ' // &
        trim(itoa(nwin)) // ' window(s)'
      if (len_trim(focused_title()) > 0) &
        status = trim(status) // ' | focus: ' // trim(focused_title())
      status = trim(status) // ' | drag titlebar=move  red dot=close  F12=shot  F10=quit'
      call term_render(canvas, status)
      last_term = tnow
    end if
  end do

  ! ── shutdown ──
  if (term_mode) call term_shutdown()
  do i = 1, size(clients)
    if (clients(i)%used) call disconnect(i)
  end do
  st = c_close(lfd)
  call logmsg('bye')

end program fabland

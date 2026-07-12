! ═══════════════════════════════════════════════════════════════════════
!  fabland — a Wayland compositor written in Fortran.
!
!  Speaks the Wayland wire protocol directly over a unix socket (no
!  libwayland, no wlroots): wl_display, wl_registry, wl_compositor,
!  wl_shm (with SCM_RIGHTS fd passing + mmap), wl_seat, wl_output and
!  xdg-shell — enough for real, unmodified Wayland clients to connect,
!  create toplevel windows and render into shared-memory buffers.
!
!  Output is composited in software (shadows, borders, titlebars) and
!  written as PNG frames to ./shots/.
! ═══════════════════════════════════════════════════════════════════════
module fl_core
  use iso_c_binding
  use fl_libc
  use fl_png
  implicit none

  integer, parameter :: OUTW = 1024, OUTH = 640

  ! interface ids
  integer, parameter :: IF_DISPLAY = 1, IF_REGISTRY = 2, IF_CALLBACK = 3, &
    IF_COMPOSITOR = 4, IF_SHM = 5, IF_SHM_POOL = 6, IF_BUFFER = 7, &
    IF_SURFACE = 8, IF_REGION = 9, IF_OUTPUT = 10, IF_SEAT = 11, &
    IF_WM_BASE = 12, IF_POSITIONER = 13, IF_XDG_SURFACE = 14, &
    IF_TOPLEVEL = 15, IF_POPUP = 16, IF_DUMMY = 17

  ! registry global names
  integer, parameter :: G_COMPOSITOR = 1, G_SHM = 2, G_SEAT = 3, &
    G_OUTPUT = 4, G_WM_BASE = 5

  integer(4), parameter :: AMASK = -16777216      ! 0xFF000000
  integer(4), parameter :: RGBMASK = 16777215     ! 0x00FFFFFF

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
  integer(4), allocatable :: canvas(:,:)

  ! outgoing message scratch
  integer(4), target :: mw(1024)
  integer :: mn = 0

contains

  subroutine logmsg(s)
    character(*), intent(in) :: s
    write(*, '(a)') '[fabland] '//trim(s)
    flush(6)
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

  ! remove object and tell the client the id is free again
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
    case default;          s = '?'
    end select
  end function

  ! ── incoming wire parsing helpers ─────────────────────────────────────

  function ru32(ci, pos) result(v)
    integer, intent(in) :: ci, pos     ! pos: 1-based byte index
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

  ! ── slot allocators ───────────────────────────────────────────────────

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

  ! ── greetings sent on bind ────────────────────────────────────────────

  subroutine send_globals(ci, regid)
    integer, intent(in) :: ci, regid
    call mreset(); call mput_u(G_COMPOSITOR); call mput_s('wl_compositor'); call mput_u(4)
    call msend(ci, regid, 0)
    call mreset(); call mput_u(G_SHM); call mput_s('wl_shm'); call mput_u(1)
    call msend(ci, regid, 0)
    call mreset(); call mput_u(G_SEAT); call mput_s('wl_seat'); call mput_u(1)
    call msend(ci, regid, 0)
    call mreset(); call mput_u(G_OUTPUT); call mput_s('wl_output'); call mput_u(2)
    call msend(ci, regid, 0)
    call mreset(); call mput_u(G_WM_BASE); call mput_s('xdg_wm_base'); call mput_u(1)
    call msend(ci, regid, 0)
  end subroutine

  subroutine greet_output(ci, id, ver)
    integer, intent(in) :: ci, id, ver
    ! geometry
    call mreset()
    call mput_u(0); call mput_u(0)          ! x, y
    call mput_u(271); call mput_u(170)      ! phys mm
    call mput_u(0)                          ! subpixel unknown
    call mput_s('fabland')
    call mput_s('FORTRAN-CRT-77')
    call mput_u(0)                          ! transform normal
    call msend(ci, id, 0)
    ! mode: current | preferred
    call mreset()
    call mput_u(3); call mput_u(OUTW); call mput_u(OUTH); call mput_u(60000)
    call msend(ci, id, 1)
    if (ver >= 2) then
      call emit1(ci, id, 3, 1)   ! scale 1
      call emit0(ci, id, 2)      ! done
    end if
  end subroutine

  ! ── xdg configure ─────────────────────────────────────────────────────

  subroutine send_configure(si)
    integer, intent(in) :: si
    integer :: ci
    ci = surfs(si)%ci
    ! xdg_toplevel.configure(0, 0, states[])
    call mreset()
    call mput_u(0); call mput_u(0); call mput_u(0)
    call msend(ci, surfs(si)%top_id, 0)
    ! xdg_surface.configure(serial)
    call emit1(ci, surfs(si)%xdg_id, 0, next_serial())
    surfs(si)%configured = .true.
  end subroutine

  ! ── buffer commit: copy client pixels out of the mmap'd pool ─────────

  subroutine latch_buffer(si, bufid)
    integer, intent(in) :: si, bufid
    integer :: ci, oi, bi, pi, x, y, base
    integer(4), pointer :: p32(:)
    integer(4) :: v
    ci = surfs(si)%ci
    oi = find_obj(ci, bufid)
    if (oi == 0) return
    bi = clients(ci)%objs(oi)%datai
    if (bi < 1 .or. .not. bufs(bi)%used) return
    pi = bufs(bi)%pooli
    if (.not. c_associated(pools(pi)%mem)) return
    if (bufs(bi)%off + bufs(bi)%h * bufs(bi)%stride > int(pools(pi)%sz)) return

    call c_f_pointer(pools(pi)%mem, p32, [int(pools(pi)%sz / 4)])
    if (allocated(surfs(si)%tex)) deallocate(surfs(si)%tex)
    allocate(surfs(si)%tex(bufs(bi)%w, bufs(bi)%h))
    do y = 1, bufs(bi)%h
      base = (bufs(bi)%off + (y-1) * bufs(bi)%stride) / 4
      do x = 1, bufs(bi)%w
        v = p32(base + x)
        if (bufs(bi)%fmt == 1) v = ior(iand(v, RGBMASK), AMASK)   ! xrgb: force opaque
        surfs(si)%tex(x, y) = v
      end do
    end do
    surfs(si)%w = bufs(bi)%w
    surfs(si)%h = bufs(bi)%h
    surfs(si)%mapped = .true.
    ! copy done: hand the buffer straight back
    call emit0(ci, bufid, 0)      ! wl_buffer.release
  end subroutine

  ! ── request dispatch ──────────────────────────────────────────────────

  subroutine dispatch(ci, oid, opc, ap)
    integer, intent(in) :: ci, oid, opc, ap    ! ap: 1-based index of args
    integer :: oi, iface, ver, p, nid, si, i, k
    integer :: a1, a2, a3, a4, a5
    integer(c_int) :: pfd
    character(64) :: bindname
    character(128) :: str
    type(c_ptr) :: mem
    integer :: gname, gver, pi, bi2

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
      case (0)   ! sync(callback)
        nid = ru32(ci, ap)
        call emit1(ci, nid, 0, next_serial())   ! wl_callback.done
        call send_delete_id(ci, nid)
      case (1)   ! get_registry
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_REGISTRY, 1, 0)
        call send_globals(ci, nid)
      end select

    case (IF_REGISTRY)
      if (opc == 0) then   ! bind(name, iface_string, version, new_id)
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
          call emit1(ci, nid, 0, 0)   ! format argb8888
          call emit1(ci, nid, 0, 1)   ! format xrgb8888
        case (G_SEAT)
          call add_obj(ci, nid, IF_SEAT, gver, 0)
          call emit1(ci, nid, 0, 0)   ! capabilities: none
        case (G_OUTPUT)
          call add_obj(ci, nid, IF_OUTPUT, gver, 0)
          call greet_output(ci, nid, gver)
        case (G_WM_BASE)
          call add_obj(ci, nid, IF_WM_BASE, gver, 0)
        end select
        call logmsg('client '//trim(itoa(ci))//' bound '//trim(bindname) &
          //' v'//itoa(gver))
      end if

    case (IF_COMPOSITOR)
      select case (opc)
      case (0)   ! create_surface
        nid = ru32(ci, ap)
        si = new_surf(ci, nid)
        call add_obj(ci, nid, IF_SURFACE, ver, si)
      case (1)   ! create_region
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_REGION, ver, 0)
      end select

    case (IF_REGION)
      if (opc == 0) call del_obj(ci, oid)     ! destroy; add/subtract no-op

    case (IF_SHM)
      if (opc == 0) then   ! create_pool(new_id, fd, size)
        nid = ru32(ci, ap)
        a1 = ru32(ci, ap + 4)          ! size
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
      case (0)   ! create_buffer(new_id, offset, w, h, stride, format)
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
        call logmsg('client '//trim(itoa(ci))//' buffer '//trim(itoa(a2))//'x'// &
          trim(itoa(a3))//' fmt='//itoa(a5))
      case (1)   ! destroy (mapping stays alive while buffers reference it)
        call del_obj(ci, oid)
      case (2)   ! resize
        a1 = ru32(ci, ap)
        if (c_associated(pools(pi)%mem)) k = c_munmap(pools(pi)%mem, pools(pi)%sz)
        pools(pi)%mem = c_mmap(c_null_ptr, int(a1, c_size_t), PROT_READ, MAP_SHARED, &
                               pools(pi)%fd, 0_c_long)
        pools(pi)%sz = int(a1, c_size_t)
      end select

    case (IF_BUFFER)
      if (opc == 0) then   ! destroy
        bufs(clients(ci)%objs(oi)%datai)%used = .false.
        call del_obj(ci, oid)
      end if

    case (IF_SURFACE)
      si = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)   ! destroy
        if (si > 0) then
          if (allocated(surfs(si)%tex)) deallocate(surfs(si)%tex)
          surfs(si)%used = .false.
        end if
        call del_obj(ci, oid)
        needs_paint = .true.
      case (1)   ! attach(buffer, x, y)
        surfs(si)%pend_buf = ru32(ci, ap)
      case (3)   ! frame(callback)
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_CALLBACK, 1, si)
        if (surfs(si)%ncbs < size(surfs(si)%cbs)) then
          surfs(si)%ncbs = surfs(si)%ncbs + 1
          surfs(si)%cbs(surfs(si)%ncbs) = nid
        end if
      case (6)   ! commit
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
        ! damage / regions / transform / scale / damage_buffer / offset: no-op
      end select

    case (IF_CALLBACK)
      ! clients don't send requests to callbacks

    case (IF_SEAT)
      select case (opc)
      case (0, 1, 2)   ! get_pointer/keyboard/touch (caps=0, but be graceful)
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_DUMMY, 1, 0)
      case (3)
        call del_obj(ci, oid)
      end select

    case (IF_OUTPUT)
      if (opc == 0) call del_obj(ci, oid)   ! release

    case (IF_WM_BASE)
      select case (opc)
      case (0)   ! destroy
        call del_obj(ci, oid)
      case (1)   ! create_positioner
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_POSITIONER, 1, 0)
      case (2)   ! get_xdg_surface(new_id, surface)
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
      case (0)   ! destroy
        if (si > 0) then
          surfs(si)%xdg_id = 0
          surfs(si)%configured = .false.
          surfs(si)%mapped = .false.
        end if
        call del_obj(ci, oid)
        needs_paint = .true.
      case (1)   ! get_toplevel
        nid = ru32(ci, ap)
        if (si > 0) then
          surfs(si)%top_id = nid
          surfs(si)%x = 72 + mod(nwindows * 96, OUTW - 480)
          surfs(si)%y = 96 + mod(nwindows * 72, OUTH - 400)
          nwindows = nwindows + 1
        end if
        call add_obj(ci, nid, IF_TOPLEVEL, 1, si)
      case (2)   ! get_popup
        nid = ru32(ci, ap)
        call add_obj(ci, nid, IF_POPUP, 1, si)
      case (4)   ! ack_configure
        if (si > 0) surfs(si)%acked = .true.
      case default   ! set_window_geometry: no-op
      end select

    case (IF_TOPLEVEL)
      si = clients(ci)%objs(oi)%datai
      select case (opc)
      case (0)   ! destroy
        if (si > 0) then
          surfs(si)%top_id = 0
          surfs(si)%mapped = .false.
        end if
        call del_obj(ci, oid)
        needs_paint = .true.
      case (2)   ! set_title
        p = ap
        call rstr(ci, p, str)
        if (si > 0) surfs(si)%title = str
        call logmsg('window title: "'//trim(str)//'"')
      case (3)   ! set_app_id
        p = ap
        call rstr(ci, p, str)
        call logmsg('app id: "'//trim(str)//'"')
      case default   ! move/resize/min/max/fullscreen: politely ignored
      end select

    case (IF_POPUP)
      if (opc == 0) call del_obj(ci, oid)

    case (IF_DUMMY)
      ! opaque stand-in (wl_pointer etc.); nothing to do

    end select
  end subroutine

  ! ── per-client buffered stream parsing ────────────────────────────────

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

  ! 5x7 wordmark glyphs for "fabland", scaled up, bottom-right corner
  subroutine draw_wordmark()
    character(5) :: gf(7), ga(7), gb(7), gl(7), gn(7), gd(7)
    character(5) :: glyph(7)
    character(7), parameter :: word = 'fabland'
    integer, parameter :: SC = 4
    integer :: xo, yo, i, gx, gy, px, py
    integer(4), parameter :: ink = 4079195   ! #3e3f5b, subtle
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

  subroutine repaint()
    integer :: x, y, i, sx, sy
    integer :: r, g, b
    integer(4), parameter :: border = 12229367   ! #ba9af7
    integer(4), parameter :: tbar = 3237475      ! #316663 -> actually #3165... use computed
    ! background: vertical gradient #16161e -> #1f2335 with a faint grid
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
        canvas(x, y) = canvas(x, y) + 197379   ! +#030303
      end do
    end do
    do x = 32, OUTW, 32
      do y = 1, OUTH
        canvas(x, y) = canvas(x, y) + 197379
      end do
    end do
    call draw_wordmark()

    do i = 1, size(surfs)
      if (.not. (surfs(i)%used .and. surfs(i)%mapped)) cycle
      if (.not. allocated(surfs(i)%tex)) cycle
      sx = surfs(i)%x
      sy = surfs(i)%y
      ! drop shadow
      call dim_rect(sx + 8, sy - 28 + 8, sx + surfs(i)%w + 8, sy + surfs(i)%h + 8, 150)
      ! frame + titlebar
      call fill_rect(sx - 2, sy - 30, sx + surfs(i)%w + 1, sy + surfs(i)%h + 1, border)
      call fill_rect(sx, sy - 28, sx + surfs(i)%w - 1, sy - 1, 4211275)   ! #40404b... titlebar
      call draw_dot(sx + 14, sy - 14, 5, 16736087)   ! #ff5f57
      call draw_dot(sx + 32, sy - 14, 5, 16694318)   ! #febc2e
      call draw_dot(sx + 50, sy - 14, 5, 2672704)    ! #28c840
      ! client pixels
      do y = 1, surfs(i)%h
        do x = 1, surfs(i)%w
          call blend_px(sx + x - 1, sy + y - 1, surfs(i)%tex(x, y))
        end do
      end do
    end do
  end subroutine

  ! after a repaint, fire every pending frame callback so clients animate
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

end module fl_core

! ═══════════════════════════════════════════════════════════════════════

program fabland
  use iso_c_binding
  use fl_libc
  use fl_png
  use fl_core
  implicit none

  character(256) :: rtdir, disp, sockpath, envbuf
  integer :: st, i, ci, nfds, shot_every, frame_no, shot_no
  integer(c_int) :: lfd, cfd
  type(pollfd_t) :: pfds(9)
  integer :: pmap(9)
  integer(8) :: n, last_paint, tnow
  character(64) :: shotname

  call get_environment_variable('XDG_RUNTIME_DIR', rtdir, status=st)
  if (st /= 0) rtdir = '/tmp'
  call get_environment_variable('FABLAND_DISPLAY', disp, status=st)
  if (st /= 0 .or. len_trim(disp) == 0) disp = 'fabland-0'
  shot_every = 30
  call get_environment_variable('FABLAND_SHOT_EVERY', envbuf, status=st)
  if (st == 0 .and. len_trim(envbuf) > 0) read(envbuf, *) shot_every
  call get_environment_variable('FABLAND_DEBUG', envbuf, status=st)
  debug = (st == 0 .and. len_trim(envbuf) > 0)

  sockpath = trim(rtdir)//'/'//trim(disp)
  call execute_command_line('mkdir -p shots')
  allocate(canvas(OUTW, OUTH))

  lfd = make_listen_socket(trim(sockpath))
  if (lfd < 0) then
    call logmsg('FATAL: cannot listen on '//trim(sockpath))
    stop 1
  end if

  call logmsg('+----------------------------------------------+')
  call logmsg('|  fabland -- a Wayland compositor in Fortran  |')
  call logmsg('+----------------------------------------------+')
  call logmsg('listening on '//trim(sockpath))
  call logmsg('output: '//trim(itoa(OUTW))//'x'//trim(itoa(OUTH))//'@60 (PNG frames in ./shots)')
  call logmsg('run clients with: WAYLAND_DISPLAY='//trim(disp))

  last_paint = now_ms()
  frame_no = 0
  shot_no = 0

  do   ! ── main event loop ──
    nfds = 1
    pfds(1)%fd = lfd
    pfds(1)%events = POLLIN
    pfds(1)%revents = 0
    pmap(1) = 0
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
      if (mod(frame_no, shot_every) == 0) then
        write(shotname, '(a,i6.6,a)') 'shots/frame-', shot_no, '.png'
        call write_png(trim(shotname), canvas, OUTW, OUTH)
        shot_no = shot_no + 1
        if (debug) call logmsg('wrote '//trim(shotname))
      end if
      frame_no = frame_no + 1
    end if
  end do

end program fabland

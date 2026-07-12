! fl_drm — DRM/KMS backend: fabland as an actual display server.
! Speaks the kernel's mode-setting API directly — raw ioctls, no libdrm:
! become DRM master, pick a connected connector, set a mode, create a
! dumb buffer, scan out. The structs below are the stable kernel ABI
! from drm_mode.h, transcribed into ISO_C_BINDING types.
module fl_drm
  use iso_c_binding
  use fl_libc
  implicit none
  private
  public :: drm_open_any, drm_present, drm_ready, drm_mode_w, drm_mode_h, &
            drm_describe

  ! ioctl request codes (x86_64, computed from _IO/_IOWR in drm.h)
  integer(c_long), parameter :: IO_SET_MASTER   = int(z'641E', c_long)
  integer(c_long), parameter :: IO_GETRESOURCES = int(z'C04064A0', c_long)
  integer(c_long), parameter :: IO_GETCONNECTOR = int(z'C05064A7', c_long)
  integer(c_long), parameter :: IO_GETENCODER   = int(z'C01464A6', c_long)
  integer(c_long), parameter :: IO_CREATE_DUMB  = int(z'C02064B2', c_long)
  integer(c_long), parameter :: IO_MAP_DUMB     = int(z'C01064B3', c_long)
  integer(c_long), parameter :: IO_ADDFB        = int(z'C01C64AE', c_long)
  integer(c_long), parameter :: IO_SETCRTC      = int(z'C06864A2', c_long)
  integer(c_long), parameter :: IO_DIRTYFB      = int(z'C01864B1', c_long)

  integer, parameter :: CONN_VIRTUAL = 15   ! DRM_MODE_CONNECTOR_VIRTUAL

  type, bind(c) :: mode_res_t
    integer(c_int64_t) :: fb_id_ptr = 0, crtc_id_ptr = 0
    integer(c_int64_t) :: connector_id_ptr = 0, encoder_id_ptr = 0
    integer(c_int32_t) :: count_fbs = 0, count_crtcs = 0
    integer(c_int32_t) :: count_connectors = 0, count_encoders = 0
    integer(c_int32_t) :: min_width = 0, max_width = 0
    integer(c_int32_t) :: min_height = 0, max_height = 0
  end type

  type, bind(c) :: modeinfo_t
    integer(c_int32_t) :: clock = 0
    integer(c_int16_t) :: hdisplay = 0, hsync_start = 0, hsync_end = 0
    integer(c_int16_t) :: htotal = 0, hskew = 0
    integer(c_int16_t) :: vdisplay = 0, vsync_start = 0, vsync_end = 0
    integer(c_int16_t) :: vtotal = 0, vscan = 0
    integer(c_int32_t) :: vrefresh = 0, flags = 0, mtype = 0
    integer(c_int8_t)  :: name(32) = 0_1
  end type

  type, bind(c) :: get_connector_t
    integer(c_int64_t) :: encoders_ptr = 0, modes_ptr = 0
    integer(c_int64_t) :: props_ptr = 0, prop_values_ptr = 0
    integer(c_int32_t) :: count_modes = 0, count_props = 0, count_encoders = 0
    integer(c_int32_t) :: encoder_id = 0, connector_id = 0
    integer(c_int32_t) :: connector_type = 0, connector_type_id = 0
    integer(c_int32_t) :: connection = 0, mm_width = 0, mm_height = 0
    integer(c_int32_t) :: subpixel = 0, pad = 0
  end type

  type, bind(c) :: get_encoder_t
    integer(c_int32_t) :: encoder_id = 0, encoder_type = 0
    integer(c_int32_t) :: crtc_id = 0, possible_crtcs = 0, possible_clones = 0
  end type

  type, bind(c) :: create_dumb_t
    integer(c_int32_t) :: height = 0, width = 0, bpp = 0, flags = 0
    integer(c_int32_t) :: handle = 0, pitch = 0
    integer(c_int64_t) :: size = 0
  end type

  type, bind(c) :: map_dumb_t
    integer(c_int32_t) :: handle = 0, pad = 0
    integer(c_int64_t) :: offset = 0
  end type

  type, bind(c) :: fb_cmd_t
    integer(c_int32_t) :: fb_id = 0, width = 0, height = 0
    integer(c_int32_t) :: pitch = 0, bpp = 0, depth = 0, handle = 0
  end type

  type, bind(c) :: crtc_t
    integer(c_int64_t) :: set_connectors_ptr = 0
    integer(c_int32_t) :: count_connectors = 0
    integer(c_int32_t) :: crtc_id = 0, fb_id = 0
    integer(c_int32_t) :: x = 0, y = 0
    integer(c_int32_t) :: gamma_size = 0, mode_valid = 0
    type(modeinfo_t)   :: mode
  end type

  type, bind(c) :: dirty_t
    integer(c_int32_t) :: fb_id = 0, flags = 0, color = 0, num_clips = 0
    integer(c_int64_t) :: clips_ptr = 0
  end type

  interface
    integer(c_int) function c_ioctl_p(fd, req, arg) bind(c, name='ioctl')
      import :: c_int, c_long, c_ptr
      integer(c_int), value :: fd
      integer(c_long), value :: req
      type(c_ptr), value :: arg
    end function
  end interface

  logical :: drm_ready = .false.
  integer(c_int) :: dfd = -1
  integer :: drm_mode_w = 0, drm_mode_h = 0
  integer :: fb_pitch = 0, fb_id = 0
  type(c_ptr) :: fbmem = c_null_ptr
  character(128) :: descr = ' '

contains

  function drm_describe() result(s)
    character(128) :: s
    s = descr
  end function

  ! Try one card: master, connected connector, mode, dumb fb, scanout.
  ! virtual_only guards against ever touching a physical display unasked.
  function drm_try_card(path, virtual_only) result(ok)
    character(*), intent(in) :: path
    logical, intent(in) :: virtual_only
    logical :: ok
    character(kind=c_char), target :: cpath(len_trim(path)+1)
    type(mode_res_t), target :: res
    type(get_connector_t), target :: conn
    type(get_encoder_t), target :: enc
    type(create_dumb_t), target :: cd
    type(map_dumb_t), target :: md
    type(fb_cmd_t), target :: fb
    type(crtc_t), target :: crtc
    integer(c_int32_t), allocatable, target :: conn_ids(:), crtc_ids(:)
    type(modeinfo_t), allocatable, target :: modes(:)
    integer(c_int32_t), target :: one_conn(1)
    integer :: i, st, ci, use_crtc
    type(modeinfo_t) :: mode

    ok = .false.
    do i = 1, len_trim(path)
      cpath(i) = path(i:i)
    end do
    cpath(len_trim(path)+1) = c_null_char
    dfd = c_open(cpath, 524290_c_int, 0_c_int)     ! O_RDWR | O_CLOEXEC
    if (dfd < 0) return

    ! exclusive: if another compositor is master, this fails and we walk away
    if (c_ioctl_p(dfd, IO_SET_MASTER, c_null_ptr) /= 0) then
      st = c_close(dfd); dfd = -1; return
    end if

    st = c_ioctl_p(dfd, IO_GETRESOURCES, c_loc(res))
    if (st /= 0 .or. res%count_connectors < 1 .or. res%count_crtcs < 1) then
      st = c_close(dfd); dfd = -1; return
    end if
    allocate(conn_ids(res%count_connectors), crtc_ids(res%count_crtcs))
    res%connector_id_ptr = transfer(c_loc(conn_ids(1)), 0_c_int64_t)
    res%crtc_id_ptr = transfer(c_loc(crtc_ids(1)), 0_c_int64_t)
    res%fb_id_ptr = 0; res%encoder_id_ptr = 0
    res%count_fbs = 0; res%count_encoders = 0
    st = c_ioctl_p(dfd, IO_GETRESOURCES, c_loc(res))
    if (st /= 0) then
      st = c_close(dfd); dfd = -1; return
    end if

    do ci = 1, res%count_connectors
      conn = get_connector_t()
      conn%connector_id = conn_ids(ci)
      st = c_ioctl_p(dfd, IO_GETCONNECTOR, c_loc(conn))
      if (st /= 0) cycle
      if (conn%connection /= 1 .or. conn%count_modes < 1) cycle
      if (virtual_only .and. conn%connector_type /= CONN_VIRTUAL) cycle
      allocate(modes(conn%count_modes))
      conn%modes_ptr = transfer(c_loc(modes(1)), 0_c_int64_t)
      conn%count_props = 0; conn%count_encoders = 0
      st = c_ioctl_p(dfd, IO_GETCONNECTOR, c_loc(conn))
      if (st /= 0) then
        deallocate(modes); cycle
      end if
      mode = modes(1)                      ! preferred mode comes first
      deallocate(modes)

      use_crtc = crtc_ids(1)
      if (conn%encoder_id /= 0) then
        enc = get_encoder_t()
        enc%encoder_id = conn%encoder_id
        if (c_ioctl_p(dfd, IO_GETENCODER, c_loc(enc)) == 0) then
          if (enc%crtc_id /= 0) use_crtc = enc%crtc_id
        end if
      end if

      cd = create_dumb_t()
      cd%width = int(mode%hdisplay, c_int32_t)
      cd%height = int(mode%vdisplay, c_int32_t)
      cd%bpp = 32
      if (c_ioctl_p(dfd, IO_CREATE_DUMB, c_loc(cd)) /= 0) cycle
      md = map_dumb_t()
      md%handle = cd%handle
      if (c_ioctl_p(dfd, IO_MAP_DUMB, c_loc(md)) /= 0) cycle
      fbmem = c_mmap(c_null_ptr, int(cd%size, c_size_t), 3, MAP_SHARED, &
                     dfd, int(md%offset, c_long))
      if (.not. c_associated(fbmem)) cycle
      fb = fb_cmd_t()
      fb%width = cd%width; fb%height = cd%height
      fb%pitch = cd%pitch; fb%bpp = 32; fb%depth = 24; fb%handle = cd%handle
      if (c_ioctl_p(dfd, IO_ADDFB, c_loc(fb)) /= 0) cycle

      one_conn(1) = conn%connector_id
      crtc = crtc_t()
      crtc%set_connectors_ptr = transfer(c_loc(one_conn(1)), 0_c_int64_t)
      crtc%count_connectors = 1
      crtc%crtc_id = use_crtc
      crtc%fb_id = fb%fb_id
      crtc%mode_valid = 1
      crtc%mode = mode
      if (c_ioctl_p(dfd, IO_SETCRTC, c_loc(crtc)) /= 0) cycle

      drm_mode_w = int(mode%hdisplay)
      drm_mode_h = int(mode%vdisplay)
      fb_pitch = int(cd%pitch)
      fb_id = fb%fb_id
      drm_ready = .true.
      ok = .true.
      write(descr, '(a,i0,a,i0,a,i0,a,i0,a,i0)') trim(path)//' connector ', &
        conn%connector_id, ' (type ', conn%connector_type, ') mode ', &
        drm_mode_w, 'x', drm_mode_h, '@', mode%vrefresh
      return
    end do
    st = c_close(dfd)
    dfd = -1
  end function

  ! Probe FABLAND_DRM_CARD if set (any connector), else scan for a card
  ! with a free master AND a Virtual connector (vkms) — never a real one.
  function drm_open_any(explicit) result(ok)
    character(*), intent(in) :: explicit
    logical :: ok
    character(24) :: path
    integer :: n
    if (len_trim(explicit) > 0) then
      ok = drm_try_card(trim(explicit), .false.)
      return
    end if
    do n = 0, 7
      write(path, '(a,i0)') '/dev/dri/card', n
      ok = drm_try_card(trim(path), .true.)
      if (ok) return
    end do
    ok = .false.
  end function

  ! Copy the canvas into the scanout buffer, centered, then flush.
  subroutine drm_present(canvas, cw, ch)
    integer, intent(in) :: cw, ch
    integer(4), intent(in) :: canvas(cw, ch)
    integer(4), pointer :: fb(:)
    type(dirty_t), target :: dirty
    integer :: x, y, ox, oy, row, st, words
    if (.not. drm_ready) return
    words = fb_pitch / 4 * drm_mode_h
    call c_f_pointer(fbmem, fb, [words])
    ox = max(0, (drm_mode_w - cw) / 2)
    oy = max(0, (drm_mode_h - ch) / 2)
    do y = 1, min(ch, drm_mode_h)
      row = (oy + y - 1) * (fb_pitch / 4)
      do x = 1, min(cw, drm_mode_w)
        fb(row + ox + x) = canvas(x, y)
      end do
    end do
    dirty%fb_id = fb_id
    st = c_ioctl_p(dfd, IO_DIRTYFB, c_loc(dirty))   ! optional; EINVAL is fine
  end subroutine

end module fl_drm

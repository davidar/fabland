! fl_libc — raw libc bindings for fabland. No helper C code: every syscall
! is reached directly from Fortran via ISO_C_BINDING.
module fl_libc
  use iso_c_binding
  implicit none

  integer(c_int), parameter :: AF_UNIX = 1
  integer(c_int), parameter :: SOCK_STREAM = 1
  integer(c_int), parameter :: SOL_SOCKET = 1
  integer(c_int), parameter :: SCM_RIGHTS = 1
  integer(c_int), parameter :: MSG_NOSIGNAL = 16384        ! 0x4000
  integer(c_int), parameter :: MSG_CMSG_CLOEXEC = 1073741824 ! 0x40000000
  integer(c_int), parameter :: PROT_READ = 1
  integer(c_int), parameter :: MAP_SHARED = 1
  integer(c_short), parameter :: POLLIN = 1
  integer(c_short), parameter :: POLLERR = 8
  integer(c_short), parameter :: POLLHUP = 16

  type, bind(c) :: pollfd_t
    integer(c_int)   :: fd = -1
    integer(c_short) :: events = 0
    integer(c_short) :: revents = 0
  end type

  type, bind(c) :: iovec_t
    type(c_ptr)        :: base
    integer(c_size_t)  :: len
  end type

  type, bind(c) :: msghdr_t
    type(c_ptr)        :: name
    integer(c_int)     :: namelen
    type(c_ptr)        :: iov
    integer(c_size_t)  :: iovlen
    type(c_ptr)        :: control
    integer(c_size_t)  :: controllen
    integer(c_int)     :: flags
  end type

  type, bind(c) :: timespec_t
    integer(c_long) :: sec = 0
    integer(c_long) :: nsec = 0
  end type

  interface
    integer(c_int) function c_socket(dom, typ, proto) bind(c, name='socket')
      import :: c_int
      integer(c_int), value :: dom, typ, proto
    end function

    integer(c_int) function c_bind(fd, addr, alen) bind(c, name='bind')
      import :: c_int, c_ptr
      integer(c_int), value :: fd
      type(c_ptr), value :: addr
      integer(c_int), value :: alen
    end function

    integer(c_int) function c_listen(fd, backlog) bind(c, name='listen')
      import :: c_int
      integer(c_int), value :: fd, backlog
    end function

    integer(c_int) function c_accept(fd, addr, alen) bind(c, name='accept')
      import :: c_int, c_ptr
      integer(c_int), value :: fd
      type(c_ptr), value :: addr, alen
    end function

    integer(c_int) function c_close(fd) bind(c, name='close')
      import :: c_int
      integer(c_int), value :: fd
    end function

    integer(c_int) function c_unlink(path) bind(c, name='unlink')
      import :: c_int, c_char
      character(kind=c_char), intent(in) :: path(*)
    end function

    integer(c_int) function c_poll(fds, nfds, timeout) bind(c, name='poll')
      import :: c_int, c_long, pollfd_t
      type(pollfd_t), intent(inout) :: fds(*)
      integer(c_long), value :: nfds
      integer(c_int), value :: timeout
    end function

    integer(c_long) function c_recvmsg(fd, msg, flags) bind(c, name='recvmsg')
      import :: c_int, c_long, msghdr_t
      integer(c_int), value :: fd
      type(msghdr_t), intent(inout) :: msg
      integer(c_int), value :: flags
    end function

    integer(c_long) function c_send(fd, buf, n, flags) bind(c, name='send')
      import :: c_int, c_long, c_ptr, c_size_t
      integer(c_int), value :: fd
      type(c_ptr), value :: buf
      integer(c_size_t), value :: n
      integer(c_int), value :: flags
    end function

    type(c_ptr) function c_mmap(addr, n, prot, flags, fd, off) bind(c, name='mmap')
      import :: c_ptr, c_size_t, c_int, c_long
      type(c_ptr), value :: addr
      integer(c_size_t), value :: n
      integer(c_int), value :: prot, flags, fd
      integer(c_long), value :: off
    end function

    integer(c_int) function c_munmap(addr, n) bind(c, name='munmap')
      import :: c_ptr, c_size_t, c_int
      type(c_ptr), value :: addr
      integer(c_size_t), value :: n
    end function

    integer(c_int) function c_clock_gettime(clk, ts) bind(c, name='clock_gettime')
      import :: c_int, timespec_t
      integer(c_int), value :: clk
      type(timespec_t), intent(out) :: ts
    end function
  end interface

contains

  function now_ms() result(ms)
    integer(8) :: ms
    type(timespec_t) :: ts
    integer(c_int) :: st
    st = c_clock_gettime(1_c_int, ts)   ! CLOCK_MONOTONIC
    ms = int(ts%sec, 8) * 1000_8 + int(ts%nsec, 8) / 1000000_8
  end function

  ! Create, bind and listen on a unix stream socket at `path`.
  function make_listen_socket(path) result(fd)
    character(*), intent(in) :: path
    integer(c_int) :: fd
    integer(c_int8_t), target :: sa(110)
    integer :: i, n, st
    character(kind=c_char) :: cpath(len_trim(path)+1)

    n = len_trim(path)
    do i = 1, n
      cpath(i) = path(i:i)
    end do
    cpath(n+1) = c_null_char
    st = c_unlink(cpath)

    sa = 0_1
    sa(1) = int(AF_UNIX, 1)   ! sun_family, little-endian int16
    sa(2) = 0_1
    do i = 1, min(n, 107)
      sa(2+i) = int(iachar(path(i:i)), 1)
    end do

    fd = c_socket(AF_UNIX, SOCK_STREAM, 0_c_int)
    if (fd < 0) return
    if (c_bind(fd, c_loc(sa), int(2 + n + 1, c_int)) /= 0) then
      st = c_close(fd); fd = -1; return
    end if
    if (c_listen(fd, 16_c_int) /= 0) then
      st = c_close(fd); fd = -1; return
    end if
  end function

  ! recvmsg wrapper: reads bytes into buf(bufoff+1:) and appends any
  ! SCM_RIGHTS file descriptors to fdq. Returns byte count (0=EOF, <0=err).
  function recv_with_fds(fd, buf, bufoff, cap, fdq, nfdq) result(n)
    integer(c_int), intent(in) :: fd
    integer(c_int8_t), target, intent(inout) :: buf(*)
    integer, intent(in) :: bufoff, cap
    integer(c_int), intent(inout) :: fdq(:)
    integer, intent(inout) :: nfdq
    integer(8) :: n
    type(msghdr_t) :: mh
    type(iovec_t), target :: iov
    integer(c_int8_t), target :: cbuf(256)
    integer(8) :: clen, coff, adv
    integer :: level, ctyp, nf, k

    iov%base = c_loc(buf(bufoff+1))
    iov%len = int(cap - bufoff, c_size_t)
    mh%name = c_null_ptr; mh%namelen = 0
    mh%iov = c_loc(iov);  mh%iovlen = 1
    mh%control = c_loc(cbuf)
    mh%controllen = 256
    mh%flags = 0
    cbuf = 0_1

    n = c_recvmsg(fd, mh, MSG_CMSG_CLOEXEC)
    if (n <= 0) return

    coff = 0
    do while (coff + 16 <= int(mh%controllen, 8))
      clen  = transfer(cbuf(coff+1:coff+8), 0_8)
      level = transfer(cbuf(coff+9:coff+12), 0_c_int)
      ctyp  = transfer(cbuf(coff+13:coff+16), 0_c_int)
      if (clen < 16) exit
      if (level == SOL_SOCKET .and. ctyp == SCM_RIGHTS) then
        nf = int((clen - 16) / 4)
        do k = 1, nf
          if (nfdq < size(fdq)) then
            nfdq = nfdq + 1
            fdq(nfdq) = transfer(cbuf(coff+16+(k-1)*4+1 : coff+16+k*4), 0_c_int)
          end if
        end do
      end if
      adv = iand(clen + 7_8, not(7_8))
      coff = coff + adv
    end do
  end function

  ! Send exactly n bytes (blocking); returns .true. on success.
  function send_all(fd, p, n) result(ok)
    integer(c_int), intent(in) :: fd
    type(c_ptr), intent(in) :: p
    integer, intent(in) :: n
    logical :: ok
    integer(c_int8_t), pointer :: bytes(:)
    integer(c_int8_t), allocatable, target :: tmp(:)
    integer :: done
    integer(8) :: w

    call c_f_pointer(p, bytes, [n])
    allocate(tmp(n))
    tmp = bytes(1:n)
    done = 0
    ok = .true.
    do while (done < n)
      w = c_send(fd, c_loc(tmp(done+1)), int(n-done, c_size_t), MSG_NOSIGNAL)
      if (w <= 0) then
        ok = .false.
        return
      end if
      done = done + int(w)
    end do
  end function

end module fl_libc

! fl_png — dependency-free PNG encoder (RGB8, stored deflate blocks).
! CRC-32 and Adler-32 implemented from scratch so fabland can screenshot
! its own output without linking anything but libc.
module fl_png
  implicit none
  private
  public :: write_png

  integer(8) :: crctab(0:255)
  logical :: crc_ready = .false.

contains

  subroutine crc_init()
    integer(8) :: c
    integer :: i, j
    do i = 0, 255
      c = int(i, 8)
      do j = 1, 8
        if (iand(c, 1_8) /= 0) then
          c = ieor(shiftr(c, 1), int(z'EDB88320', 8))
        else
          c = shiftr(c, 1)
        end if
      end do
      crctab(i) = c
    end do
    crc_ready = .true.
  end subroutine

  function crc32(buf, n) result(c)
    integer(1), intent(in) :: buf(*)
    integer, intent(in) :: n
    integer(8) :: c
    integer :: i
    if (.not. crc_ready) call crc_init()
    c = int(z'FFFFFFFF', 8)
    do i = 1, n
      c = ieor(crctab(iand(ieor(c, int(iand(int(buf(i)), 255), 8)), 255_8)), shiftr(c, 8))
    end do
    c = ieor(c, int(z'FFFFFFFF', 8))
  end function

  function adler32(buf, n) result(a)
    integer(1), intent(in) :: buf(*)
    integer, intent(in) :: n
    integer(8) :: a, s1, s2
    integer :: i
    s1 = 1; s2 = 0
    do i = 1, n
      s1 = mod(s1 + iand(int(buf(i), 8), 255_8), 65521_8)
      s2 = mod(s2 + s1, 65521_8)
    end do
    a = ior(shiftl(s2, 16), s1)
  end function

  elemental function b8(v) result(b)
    integer, intent(in) :: v
    integer(1) :: b
    integer :: m
    m = iand(v, 255)
    if (m > 127) m = m - 256
    b = int(m, 1)
  end function

  subroutine put_be32(buf, pos, v)
    integer(1), intent(inout) :: buf(*)
    integer, intent(inout) :: pos
    integer(8), intent(in) :: v
    buf(pos+1) = b8(int(iand(shiftr(v, 24), 255_8)))
    buf(pos+2) = b8(int(iand(shiftr(v, 16), 255_8)))
    buf(pos+3) = b8(int(iand(shiftr(v, 8), 255_8)))
    buf(pos+4) = b8(int(iand(v, 255_8)))
    pos = pos + 4
  end subroutine

  ! pix(x, y) holds 0x00RRGGBB
  subroutine write_png(path, pix, w, h)
    character(*), intent(in) :: path
    integer, intent(in) :: w, h
    integer(4), intent(in) :: pix(w, h)
    integer(1), allocatable :: raw(:), z(:), out(:)
    integer :: rawlen, zlen, nblk, x, y, i, k, pos, u, blen, boff, cpos
    integer(8) :: crc, adl
    integer :: v

    rawlen = h * (1 + 3*w)
    allocate(raw(rawlen))
    k = 0
    do y = 1, h
      k = k + 1
      raw(k) = 0_1                 ! filter: none
      do x = 1, w
        v = pix(x, y)
        raw(k+1) = b8(iand(shiftr(v, 16), 255))
        raw(k+2) = b8(iand(shiftr(v, 8), 255))
        raw(k+3) = b8(iand(v, 255))
        k = k + 3
      end do
    end do

    ! zlib stream: 2-byte header, stored-deflate blocks, adler32
    nblk = (rawlen + 65534) / 65535
    zlen = 2 + nblk*5 + rawlen + 4
    allocate(z(zlen))
    z(1) = b8(120)   ! 0x78
    z(2) = b8(1)     ! 0x01
    pos = 2
    boff = 0
    do i = 1, nblk
      blen = min(65535, rawlen - boff)
      if (i == nblk) then
        z(pos+1) = 1_1
      else
        z(pos+1) = 0_1
      end if
      z(pos+2) = b8(iand(blen, 255))
      z(pos+3) = b8(iand(shiftr(blen, 8), 255))
      z(pos+4) = b8(iand(ieor(blen, 65535), 255))
      z(pos+5) = b8(iand(shiftr(ieor(blen, 65535), 8), 255))
      pos = pos + 5
      z(pos+1:pos+blen) = raw(boff+1:boff+blen)
      pos = pos + blen
      boff = boff + blen
    end do
    adl = adler32(raw, rawlen)
    call put_be32(z, pos, adl)

    ! assemble file: sig(8) + IHDR(25) + IDAT(12+zlen) + IEND(12)
    allocate(out(8 + 25 + 12 + zlen + 12))
    pos = 0
    out(1:8) = [b8(137), b8(80), b8(78), b8(71), b8(13), b8(10), b8(26), b8(10)]
    pos = 8
    ! IHDR
    call put_be32(out, pos, 13_8)
    cpos = pos
    out(pos+1:pos+4) = [b8(73), b8(72), b8(68), b8(82)]   ! "IHDR"
    pos = pos + 4
    call put_be32(out, pos, int(w, 8))
    call put_be32(out, pos, int(h, 8))
    out(pos+1) = 8_1    ! bit depth
    out(pos+2) = 2_1    ! color type: truecolor RGB
    out(pos+3) = 0_1; out(pos+4) = 0_1; out(pos+5) = 0_1
    pos = pos + 5
    crc = crc32(out(cpos+1:), pos - cpos)
    call put_be32(out, pos, crc)
    ! IDAT
    call put_be32(out, pos, int(zlen, 8))
    cpos = pos
    out(pos+1:pos+4) = [b8(73), b8(68), b8(65), b8(84)]   ! "IDAT"
    pos = pos + 4
    out(pos+1:pos+zlen) = z(1:zlen)
    pos = pos + zlen
    crc = crc32(out(cpos+1:), pos - cpos)
    call put_be32(out, pos, crc)
    ! IEND
    call put_be32(out, pos, 0_8)
    cpos = pos
    out(pos+1:pos+4) = [b8(73), b8(69), b8(78), b8(68)]   ! "IEND"
    pos = pos + 4
    crc = crc32(out(cpos+1:), 4)
    call put_be32(out, pos, crc)

    open(newunit=u, file=path, access='stream', form='unformatted', status='replace')
    write(u) out(1:pos)
    close(u)
  end subroutine

end module fl_png

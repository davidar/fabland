! fl_xkb — generates a complete, self-contained XKB keymap (US layout,
! no include statements) served to clients over a memfd, plus the
! ASCII -> evdev keycode bridge used by the terminal input backend.
module fl_xkb
  use iso_c_binding
  use fl_libc
  implicit none
  private
  public :: make_keymap_fd, char_to_key
  public :: MOD_SHIFT, MOD_CTRL, MOD_ALT

  ! bit values matching the keymap's modifier_map order
  integer, parameter :: MOD_SHIFT = 1   ! Shift
  integer, parameter :: MOD_CTRL = 4    ! Control
  integer, parameter :: MOD_ALT = 8     ! Mod1

contains

  function keymap_text() result(km)
    character(:), allocatable :: km
    km = 'xkb_keymap {' // new_line('a')
    call add(km, 'xkb_keycodes "fabland" {')
    call add(km, '  minimum = 8;  maximum = 255;')
    call add(km, '  <ESC> = 9;')
    call add(km, '  <AE01> = 10; <AE02> = 11; <AE03> = 12; <AE04> = 13; <AE05> = 14;')
    call add(km, '  <AE06> = 15; <AE07> = 16; <AE08> = 17; <AE09> = 18; <AE10> = 19;')
    call add(km, '  <AE11> = 20; <AE12> = 21; <BKSP> = 22; <TAB> = 23;')
    call add(km, '  <AD01> = 24; <AD02> = 25; <AD03> = 26; <AD04> = 27; <AD05> = 28;')
    call add(km, '  <AD06> = 29; <AD07> = 30; <AD08> = 31; <AD09> = 32; <AD10> = 33;')
    call add(km, '  <AD11> = 34; <AD12> = 35; <RTRN> = 36; <LCTL> = 37;')
    call add(km, '  <AC01> = 38; <AC02> = 39; <AC03> = 40; <AC04> = 41; <AC05> = 42;')
    call add(km, '  <AC06> = 43; <AC07> = 44; <AC08> = 45; <AC09> = 46; <AC10> = 47;')
    call add(km, '  <AC11> = 48; <TLDE> = 49; <LFSH> = 50; <BKSL> = 51;')
    call add(km, '  <AB01> = 52; <AB02> = 53; <AB03> = 54; <AB04> = 55; <AB05> = 56;')
    call add(km, '  <AB06> = 57; <AB07> = 58; <AB08> = 59; <AB09> = 60; <AB10> = 61;')
    call add(km, '  <RTSH> = 62; <LALT> = 64; <SPCE> = 65;')
    call add(km, '  <FK01> = 67; <FK02> = 68; <FK03> = 69; <FK04> = 70; <FK05> = 71;')
    call add(km, '  <FK06> = 72; <FK07> = 73; <FK08> = 74; <FK09> = 75; <FK10> = 76;')
    call add(km, '  <HOME> = 110; <UP> = 111; <PGUP> = 112; <LEFT> = 113; <RGHT> = 114;')
    call add(km, '  <END> = 115; <DOWN> = 116; <PGDN> = 117; <INS> = 118; <DELE> = 119;')
    call add(km, '};')
    call add(km, 'xkb_types "fabland" {')
    call add(km, '  type "ONE_LEVEL" { modifiers = none;')
    call add(km, '    level_name[Level1] = "Any"; };')
    call add(km, '  type "TWO_LEVEL" { modifiers = Shift; map[Shift] = Level2;')
    call add(km, '    level_name[Level1] = "Base"; level_name[Level2] = "Shift"; };')
    call add(km, '  type "ALPHABETIC" { modifiers = Shift+Lock;')
    call add(km, '    map[Shift] = Level2; map[Lock] = Level2;')
    call add(km, '    level_name[Level1] = "Base"; level_name[Level2] = "Caps"; };')
    call add(km, '};')
    call add(km, 'xkb_compatibility "fabland" {')
    call add(km, '  interpret Shift_L { action = SetMods(modifiers=Shift); };')
    call add(km, '  interpret Shift_R { action = SetMods(modifiers=Shift); };')
    call add(km, '  interpret Control_L { action = SetMods(modifiers=Control); };')
    call add(km, '  interpret Alt_L { action = SetMods(modifiers=Mod1); };')
    call add(km, '};')
    call add(km, 'xkb_symbols "fabland" {')
    call add(km, '  name[Group1] = "English (US)";')
    call add(km, '  key <ESC>  { [ Escape ] };')
    call add(km, '  key <AE01> { [ 1, exclam ] };')
    call add(km, '  key <AE02> { [ 2, at ] };')
    call add(km, '  key <AE03> { [ 3, numbersign ] };')
    call add(km, '  key <AE04> { [ 4, dollar ] };')
    call add(km, '  key <AE05> { [ 5, percent ] };')
    call add(km, '  key <AE06> { [ 6, asciicircum ] };')
    call add(km, '  key <AE07> { [ 7, ampersand ] };')
    call add(km, '  key <AE08> { [ 8, asterisk ] };')
    call add(km, '  key <AE09> { [ 9, parenleft ] };')
    call add(km, '  key <AE10> { [ 0, parenright ] };')
    call add(km, '  key <AE11> { [ minus, underscore ] };')
    call add(km, '  key <AE12> { [ equal, plus ] };')
    call add(km, '  key <BKSP> { [ BackSpace ] };')
    call add(km, '  key <TAB>  { [ Tab, ISO_Left_Tab ] };')
    call add(km, '  key <AD01> { [ q, Q ] };')
    call add(km, '  key <AD02> { [ w, W ] };')
    call add(km, '  key <AD03> { [ e, E ] };')
    call add(km, '  key <AD04> { [ r, R ] };')
    call add(km, '  key <AD05> { [ t, T ] };')
    call add(km, '  key <AD06> { [ y, Y ] };')
    call add(km, '  key <AD07> { [ u, U ] };')
    call add(km, '  key <AD08> { [ i, I ] };')
    call add(km, '  key <AD09> { [ o, O ] };')
    call add(km, '  key <AD10> { [ p, P ] };')
    call add(km, '  key <AD11> { [ bracketleft, braceleft ] };')
    call add(km, '  key <AD12> { [ bracketright, braceright ] };')
    call add(km, '  key <RTRN> { [ Return ] };')
    call add(km, '  key <LCTL> { [ Control_L ] };')
    call add(km, '  key <AC01> { [ a, A ] };')
    call add(km, '  key <AC02> { [ s, S ] };')
    call add(km, '  key <AC03> { [ d, D ] };')
    call add(km, '  key <AC04> { [ f, F ] };')
    call add(km, '  key <AC05> { [ g, G ] };')
    call add(km, '  key <AC06> { [ h, H ] };')
    call add(km, '  key <AC07> { [ j, J ] };')
    call add(km, '  key <AC08> { [ k, K ] };')
    call add(km, '  key <AC09> { [ l, L ] };')
    call add(km, '  key <AC10> { [ semicolon, colon ] };')
    call add(km, '  key <AC11> { [ apostrophe, quotedbl ] };')
    call add(km, '  key <TLDE> { [ grave, asciitilde ] };')
    call add(km, '  key <LFSH> { [ Shift_L ] };')
    call add(km, '  key <BKSL> { [ backslash, bar ] };')
    call add(km, '  key <AB01> { [ z, Z ] };')
    call add(km, '  key <AB02> { [ x, X ] };')
    call add(km, '  key <AB03> { [ c, C ] };')
    call add(km, '  key <AB04> { [ v, V ] };')
    call add(km, '  key <AB05> { [ b, B ] };')
    call add(km, '  key <AB06> { [ n, N ] };')
    call add(km, '  key <AB07> { [ m, M ] };')
    call add(km, '  key <AB08> { [ comma, less ] };')
    call add(km, '  key <AB09> { [ period, greater ] };')
    call add(km, '  key <AB10> { [ slash, question ] };')
    call add(km, '  key <RTSH> { [ Shift_R ] };')
    call add(km, '  key <LALT> { [ Alt_L ] };')
    call add(km, '  key <SPCE> { [ space ] };')
    call add(km, '  key <FK01> { [ F1 ] };')
    call add(km, '  key <FK02> { [ F2 ] };')
    call add(km, '  key <FK03> { [ F3 ] };')
    call add(km, '  key <FK04> { [ F4 ] };')
    call add(km, '  key <FK05> { [ F5 ] };')
    call add(km, '  key <FK06> { [ F6 ] };')
    call add(km, '  key <FK07> { [ F7 ] };')
    call add(km, '  key <FK08> { [ F8 ] };')
    call add(km, '  key <FK09> { [ F9 ] };')
    call add(km, '  key <FK10> { [ F10 ] };')
    call add(km, '  key <HOME> { [ Home ] };')
    call add(km, '  key <UP>   { [ Up ] };')
    call add(km, '  key <PGUP> { [ Prior ] };')
    call add(km, '  key <LEFT> { [ Left ] };')
    call add(km, '  key <RGHT> { [ Right ] };')
    call add(km, '  key <END>  { [ End ] };')
    call add(km, '  key <DOWN> { [ Down ] };')
    call add(km, '  key <PGDN> { [ Next ] };')
    call add(km, '  key <INS>  { [ Insert ] };')
    call add(km, '  key <DELE> { [ Delete ] };')
    call add(km, '  modifier_map Shift { Shift_L, Shift_R };')
    call add(km, '  modifier_map Control { Control_L };')
    call add(km, '  modifier_map Mod1 { Alt_L };')
    call add(km, '};')
    call add(km, '};')
  end function

  subroutine add(km, line)
    character(:), allocatable, intent(inout) :: km
    character(*), intent(in) :: line
    km = km // line // new_line('a')
  end subroutine

  subroutine make_keymap_fd(fd, sz)
    integer(c_int), intent(out) :: fd
    integer, intent(out) :: sz
    character(:), allocatable :: km
    km = keymap_text()
    fd = memfd_from_text('fabland-keymap', km)
    sz = len(km) + 1
  end subroutine

  ! Map an ASCII byte to (evdev keycode, modifier bits). code=0: unmappable.
  recursive subroutine char_to_key(ch, code, mods)
    integer, intent(in) :: ch
    integer, intent(out) :: code, mods
    character(*), parameter :: row_num  = '1234567890-='
    character(*), parameter :: row_nums = '!@#$%^&*()_+'
    character(*), parameter :: row_q    = 'qwertyuiop[]'
    character(*), parameter :: row_qs   = 'QWERTYUIOP{}'
    character(*), parameter :: row_a    = 'asdfghjkl;'''
    character(*), parameter :: row_as   = 'ASDFGHJKL:"'
    character(*), parameter :: row_z    = 'zxcvbnm,./'
    character(*), parameter :: row_zs   = 'ZXCVBNM<>?'
    character :: c
    integer :: i

    code = 0
    mods = 0
    select case (ch)
    case (13, 10);  code = 28; return   ! enter
    case (9);       code = 15; return   ! tab
    case (127, 8);  code = 14; return   ! backspace
    case (27);      code = 1;  return   ! escape
    case (32);      code = 57; return   ! space
    end select

    if (ch >= 1 .and. ch <= 26) then    ! Ctrl+letter
      call char_to_key(iachar('a') + ch - 1, code, i)
      mods = MOD_CTRL
      return
    end if
    if (ch < 33 .or. ch > 126) return

    c = achar(ch)
    i = index(row_num, c);  if (i > 0) then; code = 1 + i;  return; end if
    i = index(row_nums, c); if (i > 0) then; code = 1 + i;  mods = MOD_SHIFT; return; end if
    i = index(row_q, c);    if (i > 0) then; code = 15 + i; return; end if
    i = index(row_qs, c);   if (i > 0) then; code = 15 + i; mods = MOD_SHIFT; return; end if
    i = index(row_a, c);    if (i > 0) then; code = 29 + i; return; end if
    i = index(row_as, c);   if (i > 0) then; code = 29 + i; mods = MOD_SHIFT; return; end if
    i = index(row_z, c);    if (i > 0) then; code = 43 + i; return; end if
    i = index(row_zs, c);   if (i > 0) then; code = 43 + i; mods = MOD_SHIFT; return; end if
    select case (c)
    case ('`');  code = 41
    case ('~');  code = 41; mods = MOD_SHIFT
    case ('\');  code = 43
    case ('|');  code = 43; mods = MOD_SHIFT
    end select
  end subroutine

end module fl_xkb

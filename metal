#!/bin/sh
# metal — run fabland on real hardware, the whole line you don't have to remember.
#
#   1. switch to a spare VT: Ctrl-Alt-F3
#   2. log in
#   3. ./metal            (or ./metal /dev/dri/card1 for a different GPU)
#   4. F10 quits and hands the display back to your session (Ctrl-Alt-F2)
#
# Your session compositor releases DRM master while VT-switched away, so
# fabland can take the panel; the kernel guarantees it can never steal a
# display something else is actively driving.

cd "$(dirname "$0")" || exit 1
card=${1:-/dev/dri/card0}

if [ -n "$WAYLAND_DISPLAY" ] || [ -n "$DISPLAY" ]; then
  echo "metal: you're inside a graphical session, which still holds DRM master." >&2
  echo "metal: switch to a spare VT first (Ctrl-Alt-F3) and run this there." >&2
  exit 1
fi

if [ ! -x ./fabland ]; then
  echo "metal: ./fabland not built — run make first." >&2
  exit 1
fi

if ! id -nG | grep -qw input; then
  echo "metal: note — you're not in the 'input' group, so the mouse/touchpad" >&2
  echo "metal: won't work. One-time fix: sudo usermod -aG input $USER (re-login)." >&2
fi

# Once the compositor is up, put a terminal on the desktop so there's
# something to type into. Clients live in the fabland-test distrobox.
(
  sleep 2
  distrobox enter fabland-test -- env WAYLAND_DISPLAY=fabland-0 foot >/dev/null 2>&1 ||
    distrobox enter fabland-test -- env WAYLAND_DISPLAY=fabland-0 weston-terminal >/dev/null 2>&1
) &

exec env FABLAND_BACKEND=drm FABLAND_DRM_CARD="$card" ./fabland

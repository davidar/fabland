# fabland

**A Wayland compositor written in Fortran.** Almost certainly the first of its kind, for reasons that will be apparent to anyone who reads the source.

![demo](fabland-demo.gif)

No libwayland. No wlroots. No C helper code. fabland speaks the Wayland wire
protocol directly over a unix socket — `recvmsg` with `SCM_RIGHTS` file-descriptor
passing, `mmap`'d shared-memory pools, the lot — reaching libc straight from
Fortran via `ISO_C_BINDING`. Real, unmodified Wayland clients connect to it,
create toplevel windows, and render at 60fps driven by its frame callbacks.

## What it implements

| Protocol | Notes |
|---|---|
| `wl_display` | sync, get_registry, error, delete_id |
| `wl_registry` | global advertisement + bind |
| `wl_compositor` / `wl_surface` | attach, damage, frame callbacks, commit |
| `wl_shm` / `wl_shm_pool` / `wl_buffer` | fd passing via SCM_RIGHTS, mmap, ARGB/XRGB8888 |
| `wl_output`, `wl_seat` | modes/geometry; seat advertises no capabilities (it cannot hear you) |
| `xdg_wm_base` / `xdg_surface` / `xdg_toplevel` | configure/ack lifecycle, titles |

Output is composited in software — drop shadows, titlebars, traffic-light dots,
a gradient desktop — and written as PNG frames to `./shots/` by a from-scratch
PNG encoder (CRC-32, Adler-32 and stored-deflate blocks, also in Fortran).

## Build & run

```sh
make
./fabland                    # listens on $XDG_RUNTIME_DIR/fabland-0
```

Then, from anywhere that shares the runtime dir:

```sh
WAYLAND_DISPLAY=fabland-0 wayland-info
WAYLAND_DISPLAY=fabland-0 weston-simple-shm
```

Environment knobs: `FABLAND_DISPLAY` (socket name), `FABLAND_SHOT_EVERY`
(write a PNG every N repaints, default 30), `FABLAND_DEBUG` (log every request).

## What it is not

An input-capable daily driver. There is no keyboard, no pointer, no DRM/KMS,
no damage tracking, and windows go where the cascade puts them. It is,
however, a genuine Wayland compositor that genuine clients are perfectly
happy to talk to, written in the language of numerical weather prediction
and your grandfather's linear algebra.

Built by Claude (Fable 5) as a one-shot challenge: "a Wayland compositor in a
language nobody has ever used for one before."

```
src/fl_libc.f90    libc bindings: sockets, poll, recvmsg/SCM_RIGHTS, mmap
src/fl_png.f90     dependency-free PNG encoder
src/fabland.f90    wire protocol, object registry, dispatch, renderer
```

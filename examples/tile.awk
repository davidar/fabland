#!/usr/bin/awk -f
# tile.awk — a master-stack tiling brain for fabland, in awk.
#
# fabland exposes its window state as columnar snapshots on a FIFO and
# accepts commands on another; the entire layout policy lives out here,
# in whatever language you like. This one is awk. Yours could be BQN.
#
# Usage:
#   awk -f examples/tile.awk \
#     < $XDG_RUNTIME_DIR/fabland-0.state \
#     > $XDG_RUNTIME_DIR/fabland-0.cmd

$1 == "begin" { n = $2; W = $3; H = $4; next }
$1 == "id"  { for (k = 2; k <= NF; k++) id[k-1]  = $k; next }
$1 == "csd" { for (k = 2; k <= NF; k++) csd[k-1] = $k; next }

$1 == "end" {
    if (n == 0) next
    gap = 14
    if (n == 1) {
        place(1, gap, gap, W - 2*gap, H - 2*gap)
    } else {
        mw = int(W * 0.58)                       # master column width
        place(1, gap, gap, mw - int(1.5*gap), H - 2*gap)
        sh = int((H - gap) / (n - 1))            # stack row height
        for (j = 2; j <= n; j++)
            place(j, mw + int(0.5*gap), gap + (j-2)*sh, \
                  W - mw - int(1.5*gap), sh - gap)
    }
    fflush()
    next
}

function place(i, x, y, w, h,    tb) {
    tb = csd[i] ? 0 : 30                         # our titlebar height
    printf "move %d %d %d\n",   id[i], x, y + tb
    printf "resize %d %d %d\n", id[i], w, h - tb
}

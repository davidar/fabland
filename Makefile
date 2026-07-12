FC = gfortran
FFLAGS = -O2 -Wall -Wno-maybe-uninitialized -std=f2018

fabland: src/fl_libc.f90 src/fl_png.f90 src/fl_xkb.f90 src/fl_term.f90 src/fl_nest.f90 src/fabland.f90
	$(FC) $(FFLAGS) -J build -o $@ $^

$(shell mkdir -p build)

clean:
	rm -rf fabland build shots *.mod

.PHONY: clean

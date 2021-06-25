PROG =	simple_xy_write

SRCS =	check.f90 simple_xy_wr.f90

OBJS =	check.o simple_xy_wr.o

LIBS =	-lnetcdff -lnetcdf

F90 = gfortran
INCS = /usr/lib64/gfortran/modules
F90FLAGS = -O -I$(INCS)
LDFLAGS = 
all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.PHONY: clean
clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f .f90 .F90 .f95

.f90.o .f95.o .F90.o:
	$(F90) $(F90FLAGS) -c $<

check.o:
simple_xy_wr.o: 

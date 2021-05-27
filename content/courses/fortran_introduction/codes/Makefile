PROG =bmidata

SRCS =  bmi_calculator.f90 bmi_data.f90 csv_file.f90 prec.f90 stats.f90

OBJS =bmi_calculator.obmi_data.ocsv_file.oprec.ostats.o

LIBS =

F90 =gfortran

#F90FLAGS=-O

F90FLAGS = -g -C

LDFLAGS =

all: $(PROG)

$(PROG): $(OBJS)
        $(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.PHONY: clean

clean:
	rm-f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90 .F90 .f95

.f90.o .f95.o .F90.o:
	$(F90) $(F90FLAGS) -c $<

stats.o:prec.o
bmi_calculator.o:csv_file.o prec.o
bmi_data.o:bmi_calculator.o csv_file.o prec.o stats.o


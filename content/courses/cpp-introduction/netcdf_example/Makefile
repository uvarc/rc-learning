PROG = simple_xy_wr

SRCS =	simple_xy_wr.cpp

OBJS =	simple_xy_wr.o

LIBS =	-lnetcdf_c++

CXX = c++
CXXFLAGS = -O
LDFLAGS = 
all: $(PROG)

$(PROG): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.PHONY: clean
clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .c .cpp .cxx

.cpp.o .cxx.o:
	$(CXX) $(CXXFLAGS) -c $<

simple_xy_wr.o: simple_xy_wr.cpp

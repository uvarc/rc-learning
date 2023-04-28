PROG =	bmi

SRCS =	bmi.cxx bmistats.cxx stats.cxx

OBJS =	bmi.o bmistats.o stats.o

LIBS =	

CC = gcc
CXX = g++
CFLAGS = -O
#CXXFLAGS = -O -std=c++11
CXXFLAGS = -g -std=c++11
LDFLAGS = 
all: $(PROG)

$(PROG): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.PHONY: clean
clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .c .cpp .cxx

.c.o:
	$(CC) $(CFLAGS) -c $<

.cpp.o .cxx.o:
	$(CXX) $(CXXFLAGS) -c $<

bmi.o: bmi.cxx stats.h bmistats.h
bmistats.o: bmistats.cxx
stats.o: stats.cxx

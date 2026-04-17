from cffi import FFI

ffibuilder = FFI()

ffibuilder.cdef("""
 double sum(double x, double y); 
 double difference(double x, double y); 
 double product(double x, double y); 
 double division(double x, double y); 
 """)

# set_source() specifies the name of the Python extension module to
# create, as well as the original C source file. Conventionally, we
# name the extension module with a leading underscore. If we required
# external libraries, we would add the libraries argument with a list
# of the libraries in the form the linker would use to link them.

ffibuilder.set_source("_arithlib",
"""
     #include "arith.h"   // the C header of the library
""",
     sources=['./arith.c'])   

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)

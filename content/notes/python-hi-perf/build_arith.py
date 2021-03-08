from cffi import FFI
ffibuilder = FFI()

#First list all the function prototypes we wish to include in our module.
ffibuilder.cdef("""
  double sum(double x, double y);
  double difference(double x, double y);
  double product(double x, double y);
  double division(double x, double y);
 """)

# set_source() specifies the name of the Python extension module to
# create, as well as the original C source file.  If we had `arith.h` we
# would put `include "arith.h"` in the triple quotes.  Conventionally, we
# name the extension module with a leading underscore.
ffibuilder.set_source("_arith",
"""
  double sum(double x, double y);
  double difference(double x, double y);
  double product(double x, double y);
  double division(double x, double y);
""",
 sources = ['arith.c'],
 library_dirs = [],
)

ffibuilder.compile()


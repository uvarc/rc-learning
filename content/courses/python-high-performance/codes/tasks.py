import invoke
cpp_name="wrap_fractions.cxx"
extension_name="py_fractions"

invoke.run(
    "g++ -O3 -Wall -Werror -shared -std=c++11 -fPIC fractions.cxx "
    "-o libfractions.so "
)
invoke.run(
    "g++ -O3 -Wall -Werror -shared -std=c++11 -fPIC "
    "`python3 -m pybind11 --includes` "
    "-I /usr/include/python3.9 -I .  "
    "{0} "
    "-o {1}`python3.11-config --extension-suffix` "
    "-L. -lfractions -Wl,-rpath,.".format(cpp_name, extension_name)
)

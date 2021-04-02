#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "fractions.h"

namespace py = pybind11;

PYBIND11_MODULE(py_fractions,m) {

    m.doc() = "Fractions module";
    py::class_<Fraction>(m, "Fraction")
        .def(py::init<int, int>())
        .def("addFracs", &Fraction::addFracs);

}

---
title: NumPy and SciPy
toc: true
type: docs
draft: false
weight: 9
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 9
---

## NumPy

NumPy is not part of the base Python packages.  It is included with Anaconda and can be easily added to other Python installations.  NumPy adds many features important or useful to scientific and numeric computing.  These include

* True multidimensional arrays
* Linear algebra functions
* Fast Fourer Transform (FFT) Functions
* Random number generators 
* Tools for integrating Fortran, C, and C++ libraries.

Essential documentation is at its home [site](https://docs.scipy.org/doc/numpy/index.html).  NumPy is very powerful and our introduction will barely touch on its most important features.

### Arrays

An __array__ is an ordered data structure that contains elements all of the same type. NumPy introduces a new Python data type called the __Ndarray__ (n-dimensional array).  Like all ordered structures in Python, each element is addressed by integer _indices_.  An array has one or more _dimensions_, which NumPy generally calls _axes_.  The _bounds_ are the lowest and highest indexes.  The _rank_ is the number of dimentions.  Arrays should be regarded as fixed size once they are set up. NumPy arrays are _mutable_, so elements may be changed in place.

```python
import numpy 
A=numpy.array([1,0,0,0])
lenA=len(A)    #number of rows 
A.size         #number of elements 
A.shape        #tuple of dimensions 
```

Unlike most Python data types, arrays must be initialized before they can be addressed.  Several methods are available.

* Convert from list 
  * A=numpy.array([x,y,z])
* Size and initialize to all zeros.  The dimensions must be in a tuple.  There is no explicit maximum rank, but memory can be rapidly exhausted with large arrays.
  * A=numpy.zeros((10,10))
* Size and initialize to arbitrary contents of memory 
  * A=numpy.empty(100) 
* Size and initialize to ones 
  * A=numpy.ones((2,3,4))
* Identity matrix (NxN but only N is declared)
  * A=numpy.eye(100) 
* Initialize to all zeros, same shape as a pre-existing array 
  * B=numpy.zeros_like(A)

There are other functions that can be used to initialize but these are among the most common.

### Ndarray Types

Python is typed by _inference_, meaning that it uses context to determine type, so normally we do not declare a variable's type, but Ndarrays may and sometimes must be declared with a type.  NumPy supports more types than base Python, including single-precision floating-point (`float32`). Unless otherwise specified, the type will be taken from the type of the initial elements if any are present.  If they are not, the default is float (double precision) but it can be modified with the `dtype` keyword argument to the constructor.

```python
import numpy as np
Z=np.zeros((3,4),dtype='complex')
M=np.array([True,True,False,False])
```

Be sure to note the differences between

```python
A=np.zeros((10,10))
IM=np.zeros((10,10),dtype='int')
mask=np.zeros((10,10),dtype='bool')
```

We can explicitly declare multidimensional arrays as well.

```python
C=np.array([[1,2,3], [4,5,6]], dtype=float)
print(C)
[[ 1.  2.  3.]
 [ 4.  5.  6.]]
```

 However, this is not very practical for large arrays.  If we can declare an array as a linear sequence of numbers, we can use the built-in function `arange`.  The syntax is similar to `range` but it can take arguments of any numerical type, and it returns an Ndarray.

```python
V=np.arange(10,30,5)
print(V)
[10 15 20 25]
```

<details>
<summary>Exercise 23</summary>

Use the zeros function for the following:
Initialize an array A1 of rank 1 with size 4 and type double.
Initialize an array IU of rank 1 with size 4 and type integer.
Initialze an array M1 of rank 1 with size 4 and type Boolean.
Print each of the arrays you just created.
Initialize a rank-3 array to arbitrary elements.

</details>

### Array Elements and Slices

 Each element can be addressed by its index or indices.  As for other ordered sequences, numbering starts from zero.  Indices are enclosed in square brackets.  The dimensions are usually enclosed in a single set of square brackets.

```python
A[i,j,k]
```

Negative indices count from the last element.

```python
V[-1]   #last element
A[0,-2] #first in column 0, next to last in column 1.
```

Subarrays, or slices, are indicated by the colon-separated range operator we have seen several times now.   As usual for Python, the upper bound must be one greater than the intended upper bound, i.e. it is _non-inclusive_.  We can omit lower and upper bounds if they are the edges; `\[lb:\]` extends from `lb` to the last element, while `\[:ub\]` extends from the beginning to `ub-1`.  The entire axis is expressed as `\[:\]`.

```python
import numpy
A=numpy.zeros((100,100))
B=A[0:11,:]
C=A[S1:E1,S2:E2]
D=A[:,1]  #second column
```

Pay close attention to negative indices, especially when they are part of a slice specification.

```python
V=np.array([0,1,2,3,4])
u=np.arange(25).reshape(5,5)
V[-1]
 4
V[:-1] #note element versus slice
 [0,1,2,3]  
u[:-1,:]
 [[ 0  1  2  3  4]
 [ 5  6  7  8  9]
 [10 11 12 13 14]
 [15 16 17 18 19]]
```

### Axes

NumPy refers to dimensions as axes.  Many NumPy intrinsic functions take an optional axis argument.  

```python
a=np.arange(12).reshape(3,4)
print(a.sum())
 66
print(a.sum(0))
 [12 15 18 21]
print(a.sum(1))
 [ 6 22 38]
```

 In sum and similar methods, such as prod, the axis argument is the one to be _summed out_.  In these examples, the first sum uses all elements, resulting in a single value.  In the second two examples, we reduce the rank by one and the shape of the result removes the summed axis.

### Array Attributes

 An Ndarray is a class with attributes that can be accessed.

```python
A=np.random.random((10,10))
print(A.shape)
(10,10)
print(A.size)
100
print(A.ndim)
2
print(A.dtype)
dtype('float64')
```

### More Advanced Indexing

 An Ndarray can take as its index an integer or Boolean array.

```python
A=np.array([-2.,3.,-8.,
             -11.,12.,12.,45.,19.])
I=np.array([2,4,5])
valid=A>0
print(A[I])
[-8.,12.,12.]
print(A[valid])
[  3.  12.  12.  45.  19.]
```

### Reading and Writing Files

NumPy includes several functions that can simplify reading and writing files.  For files with a simple spreadsheet-like structure, `loadtxt` works well.  The first argument can be either a file name or a file handle.

```python
x,y=np.loadtxt(f, delimiter=',', usecols=(0, 2), unpack=True)
v=np.loadtxt(f,delimiter=',',usecols=(1,)) #usecols needs tuple
W=np.loadtxt(fin,skiprows=2)
```

If `unpack` is not specified, `loadtxt` returns the values into a rank-2 array; otherwise it returns the values into a tuple of rank-1 arrays.  Columns may optionally be selected with `usecols`.  Header rows can be ignored with `skiprows`.  Other options are available.

More generally the `genfromtxt` function can be applied.  The `loadtxt` function does not handle files with missing or irregular data, but `genfrom txt` can to an extent.

```python
data = np.genfromtxt(infile, delimiter=';', comments='#', skip_footer=1)
```

For a simple data dump and restore we can use the `save` and `load` commands.  The counterpart to `loadtxt` is `savetxt` for saving a rank-1 or rank-2 array.

```python
np.savetxt(outfile,A,delimiter=',')
```

The `save` function will dump an  array or pickled object in binary format to a .npy file.

```python
np.save(A)
np.savez(A)  #compresses, saves as .npz
```

Its counterpart is `load`

```python
A=np.load(f,A)
```

### Array Operations

NumPy provides many functions, including a large selection of mathematical functions, that accept array arguments as well as scalars.  They operate on the arrays _elementwise_.

```python
T=numpy.ones(4)
Z=3.0*T+numpy.ones_like(T)
I=numpy.array([1,0,0,0])
A=math.pi*I
B=numpy.sin(A)
C=A/B  #remember: elementwise
```

### Frequently Used NumPy Functions

NumPy provides many built-in functions for array manipulation or mathematical/statistical calculations.

| Array Manipulation | Mathematical Operations    |
| ------------------ | -------------------------- |
| arange             | abs, cos, sin, tan         |
| array              | average, mean, median, std |
| all, any, where    | ceil, floor                |
| compress           | dot                        |
| copy               | sum, product               |
| ones, zeros, empty | min, max                   |
| fromfile, loadtxt  | argmin, argmax             |
| reduce             | nan, isnan                 |
| repeat, reshape    | inf, isinf                 |
| rollaxis, swapaxis | linspace                   |
| transpose          | lstsq                      |

This is just a sample; the full reference can be examined at the [manual](https://docs.scipy.org/doc/numpy/reference/routines.html).

### Ufuncs

Functions that accept both arrays and scalars are called __ufuncs__ for "universal functions".  You can write your own ufuncs easily.  The functions are subject to some restrictions:

* The function must not change its input parameters.  
* The function cannot print anything, or do any other form of IO
* The function cannot exit (stop the run)
* The function must return a single value (no tuples or other compound type)

Functions that adhere to these rules are said to be _pure_.  The prohibition on printing does complicate debugging somewhat.

Example:

```python
import numpy as np 

def F2C(T):
    return 5.*(T-32.)/9.

TempsF=np.arange(0.,213.,2.)
TempsC=F2C(TempsF)
print(TempsC)
print(F2C(50.))
```

### Optimization

Python can be quite slow, as is the case for most interpreted languages. Loops are generally the slowest constructs.  NumPy array functions are highly optimized and often can eliminate loops.

Example: the built-in sum over an array or array slice can replace the corresponding loops, and is much faster.

```python
s=0
for e in V:
    s+=s+e
```

Use instead

```python
s=V.sum()
```

## SciPy

SciPy, the \_sci_entific \_py_thon library, builds on NumPy to provide a set of modules and packages that add functions for data analysis and numerical computations.  These include 

* special functions 
* optimizations 
* linear algebra 
* quadrature (numerical integration)
* interpolation
* signal processing
* basic statistics 

Its homepage at [www.scipy.org](www.scipy.org) has details and documentation.  The SciPy library is part of the "SciPy ecosystem" that also includes NumPy, Sympy, and Pandas.  We will not discuss Sympy but it is a well-developed computer algebra system (CAS) that is also incorporated into several other packages such as SageMath.

### Importing SciPy Packages

Importing from scipy alone brings only the base packages, which provide a fairly minimal set of tools.  Individual packages must be imported expliclty.

```python
from scipy import linalg, optimize
```

For a loaded package, type `help(name)` to see a summary of available functions.

```python
help(linalg)
```

The SciPy [reference guide](https://docs.scipy.org/doc/scipy/reference/) is an invaluable resource for SciPy usage.

Simple linalg example:

```python
import numpy as np
from scipy import linalg
A = np.array([[1, 2], [3, 4]])
b = np.array([[5], [6]])
c=linalg.solve(A, b)
print(c)
```

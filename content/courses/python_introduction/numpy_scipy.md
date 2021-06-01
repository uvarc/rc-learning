---
title: NumPy and SciPy
toc: true
type: docs
draft: false
weight: 90
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 90
---

## NumPy

NumPy is not part of the base Python packages.  It is included with Anaconda and can be easily added to other Python installations.  NumPy adds many features important or useful to scientific and numeric computing.  These include

* True multidimensional arrays
* Linear algebra functions
* Fast Fourer Transform (FFT) Functions
* Random number generators 
* Tools for integrating Fortran, C, and C++ libraries.

NumPy is very powerful and our introduction will barely touch on its most important features.

### Arrays

An __array__ is an ordered data structure that contains elements all of the same type. NumPy introduces a new Python data type called the __Ndarray__ (n-dimensional array).  Like all ordered structures in Python, each element is addressed by integer _indices_.  An array has one or more _dimensions_, which NumPy generally calls _axes_.  The _bounds_ are the lowest and highest indexes.  The _rank_ is the number of dimensions.  Arrays should be regarded as fixed size once they are set up. NumPy arrays are _mutable_, so elements may be changed in place.

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

Subarrays, or slices, are indicated by the colon-separated range operator we have seen several times now.   As usual for Python, the upper bound must be one greater than the intended upper bound, i.e. it is _non-inclusive_.  We can omit lower and upper bounds if they are the edges; `[lb:]` extends from `lb` to the last element, while `[:ub]` extends from the beginning to `ub-1`.  The entire axis is expressed as `[:]`.

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

SciPy, the *Sci*entific *Py*thon library, builds on NumPy to provide a set of modules and packages that add functions for data analysis and numerical computations.  These include 

* special functions 
* optimizations 
* linear algebra 
* quadrature (numerical integration)
* interpolation
* signal processing
* basic statistics 

Its homepage at [www.scipy.org](www.scipy.org) has details and documentation.  The SciPy library is part of the "SciPy ecosystem" that also includes NumPy, Sympy, and Pandas.  We will not discuss Sympy but it is a well-developed computer algebra system (CAS) that is also incorporated into several other packages such as SageMath.

### Importing SciPy Packages

Importing from scipy alone brings only the base packages, which provide a fairly minimal set of tools.  Individual packages must be imported explicitly.

```python
from scipy import linalg, optimize
```

For an imported package, type `help(name)` to see a summary of available functions.

```python
help(linalg)
```
Simple linalg example:

```python
import numpy as np
from scipy import linalg
A = np.array([[1, 2], [3, 4]])
b = np.array([[5], [6]])
c=linalg.solve(A, b)
print(c)
```
### Resources

Essential documentation for NumPy is at its home [site](https://docs.scipy.org/doc/numpy/index.html).  

The SciPy [reference guide](https://docs.scipy.org/doc/scipy/reference/) is an invaluable resource for SciPy usage.

### Projects

Remember that you need to add the `import numpy` (or commonly used `import numpy as np`) statement in your script before you can use the numpy package.

#### Project 1 
Write a Python script that performs the following operations:
 
a) Create a numpy array, x, of values from -1.0 to 1.0 inclusive, with step sizes of 0.01.  Use numpy.pi (or np.pi) for pi. 
b) Create a numpy array, y, where y = sin(pi*x) + cos(3pi*x/2)
c) Determine and print the mean y value.
d) Determine and print the minimum value of y and the maximum value of y over the range of x.  Also print the corresponding x coordinates where y reaches a minimum and where y reaches a maximum. **Hint:** Look up the argmin and argmax functions.   Pay attention to the difference between index and value.
e) Go back to the first chapter and review how to make a plot of y versus x using Matplotlib.  Add code to plot y as a function of x.
f) Add a line to accept user input to specify the x start, x end, and stride values.  Your finished code should get these values from the user, print the values and x-coordinate of the max and min for y, and display a plot of y versus x. Upload the plot for input values of starting x=-2., ending x=2., stride=.01.

#### Project 2 
Download the file [cpi.csv](/data/cpi.csv)

Please use NumPy arrays for this project, even though lists would work.

This file is the value of $100 from a baseline in January 1913 (the oldest consistent data available) to January 2020, as computed by the US Bureau of Labor Statistics; this gives us the consumer price index. The ratio of any two years is an estimate of the change in cost of living from the one to the other. 

a) Write a function that takes two years and returns this ratio.  Note that the order will matter.

The derivative of the CPI is a crude estimate of the inflation rate.  Write a function that takes an array and returns another array using the formula infl\_rate=(cpi[yr+1]-cpi[yr])/12

Note that the returned array will have one fewer element than the original.

b) Write a program that uses your two functions to

read cpi.csv by any means you choose.

Request two years and a price from the user.  Compute the corresponding price for the second year provided by the user. 

Plot the CPI and your approximation to the inflation rate.  
  Plot inflation rate versus the midpoint of the years (so would start in 
  June 1913, i.e. 1913.5, and end June 2019, i.e. 2019.5).

c) In 1954 a color TV cost approximately $1295 in that year's dollars.  How much would that be in 2020 (as of January) dollars? 

d) Look at the Matplotlib documentation and find how to use subplots to plot CPI and inflation rate one above the other.

e) Read about exceptions in the Files chapter and add them to your code to test for the existence of the file before attempting to open it.  Add with/as to the reading of the file (you can surround a numpy call with this also).

f) Convert your file with the functions into a module.  Isolate the calls to input/output, including plotting, by using a main() function.  Add the `if __name__=="__main__"` so that it will not be invoked if you import your module.

#### Project 3
Modify your bmistats.py and your main program from Project 3 of the [Functions and Modules](/courses/python_introduction/functions_modules) chapter to use NDarrays rather than lists.  Use a NumPy function to read the data.  Use NumPy intrinsics rather than hand-coding mean and standard deviation.  Use your BMI function as a ufunc to compute all the BMI values at once and return a new array.

If you wrote an outlier detector, modify it to use the `np.percentile(a,m)` function to compute the upper and lower quartiles using the IQR.  See this [example](https://www.dasca.org/world-of-big-data/article/identifying-and-removing-outliers-using-python-packages).  

Admire how much less code and work is required when you use NumPy.

#### Project 4  
Find the maximum of a 3d surface by “brute force” evaluation of x, y, z values.

![bruteforce.png](/courses/python_introduction/exercise-bruteforce.png)

a) Generate a list of N random values for each of x and y over the above range. Use numpy arrays. For testing you can use N=8,000,000.

b) Write a function that determines the x/y coordinates that define the maximum z value of the 3d surface. Once the code is working, vary N and compare how the x,y,z max values change.

c) Optional: Plot the surface using the matplotlib package (see below).  Be careful, you may want to reduce the number of points to plots while experimenting with the best approach.

d) Optional: Create a module for your z calculating function.  Import that module into a main script.  Use the `if __name__ = "__main__":` code block in your calling script.


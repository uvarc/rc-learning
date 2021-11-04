---
title: NumPy
toc: true
type: book
draft: false
weight: 100
---

NumPy is not part of the base Python packages.  It is included with Anaconda and can be easily added to other Python installations.  NumPy adds many features important or useful to scientific and numeric computing.  These include

* True multidimensional arrays
* Linear algebra functions
* Fast Fourer Transform (FFT) Functions
* Random number generators 
* Tools for integrating Fortran, C, and C++ libraries.

NumPy is very powerful and our introduction will barely touch on its most important features.

## Arrays

An __array__ is an ordered data structure that contains elements all of the same type. NumPy introduces a new Python data type called the __Ndarray__ (n-dimensional array).  Like all ordered structures in Python, each element is addressed by integer _indices_.  An array has one or more _dimensions_, which NumPy generally calls _axes_.  The _bounds_ are the lowest and highest indexes.  The _rank_ is the number of dimensions.  Arrays should be regarded as fixed size once they are set up. NumPy arrays are _mutable_, so elements may be changed in place.

```python
import numpy 
A=numpy.array([1,0,0,0])
lenA=len(A)    #number of rows 
A.size         #number of elements 
A.shape        #tuple of dimensions 
```

Unlike most mutable Python data types, arrays must be initialized before they can be addressed.  Several methods are available.

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
* Initialize to random numbers in the half-open interval [0.0, 1.0)
  * R=numpy.random.random((4,5,7))

There are other functions that can be used to initialize but these are among the most common.

### Ndarray Types

Python is typed by _inference_, meaning that it uses context to determine type, so normally we do not declare a variable's type, but Ndarrays may and sometimes must be declared with a type.  NumPy supports more types than base Python, including single-precision floating-point (`float32`). Unless otherwise specified, the type will be taken from the type of the initial elements if any are present.  If they are not, the default is float (double precision) but it can be modified with the `dtype` keyword argument to the constructor.

To save some typing, we'll use `import numpy as np` to import numpy and `np` to reference it.

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
V=np.arange(10,30,5)  # start,end,stride 
print(V)
[10 15 20 25]
```

<details>
<summary>Exercise 25</summary>

Use the zeros function for the following:

* Initialize an array A1 of rank 1 with size 4 and type double.
* Initialize an array IU of rank 1 with size 4 and type integer.
* Initialize an array M1 of rank 1 with size 4 and type Boolean.
* Print each of the arrays you just created.
* Initialize a rank-3 array to arbitrary elements.

Print each array.

</details>

#### String Arrays

NumPy isn't really aimed at handling strings, but sometimes we need an array of strings.  In NumPy the 'str' type is more like a fixed-size array of characters.
The default length is one character.

```no-highlight
>>> import numpy as np
>>> Sa=np.zeros((4),dtype='str')
>>> Sa
array(['', '', '', ''], dtype='<U1')
>>> Sa[0]='hello'
>>> Sa
array(['h', '', '', ''], dtype='<U1')

>>>Sa2=np.array(['abc','def','ghi'])
>>>Sa2[1]='define'
>>>Sa2
array(['abc', 'def', 'ghi'], dtype='<U3')
```

Often, fixed-length strings are adequate, and they are faster than arbitrary-length strings, but if we really must create an array with variable-length strings we can use `dtype='object'`
```no-highlight
>>> Sa3=np.array(['abc','def','ghi'],dtype='object')
>>> Sa3[1]='define'
>>> Sa3
array(['abc', 'define', 'ghi'], dtype=object)
```

## Array Elements and Slices

Each element can be addressed by its index or indices.  As for other ordered sequences, numbering starts from zero.  Indices are enclosed in square brackets.  The dimensions are usually enclosed in a single set of square brackets.

```python
A[i,j,k]
```

Negative indices count from the last element.

```python
V[-1]   #last element
A[0,-2] #first in column 0, next to last in column 1.
```

### Slicing Arrays

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
### More Advanced Indexing

 An Ndarray can take as its index an integer or Boolean array.

```python
A=np.array([-2.,3.,-8.,-11.,12.,12.,45.,19.])
I=np.array([2,4,5])
valid=A>0
print(valid)
[False  True False False  True  True  True  True]
print(A[I])
[-8.,12.,12.]
print(A[valid])
[  3.  12.  12.  45.  19.]
```

## Axes

NumPy refers to dimensions as axes.  Many NumPy intrinsic functions take an optional axis argument.  

```python
a=np.arange(12).reshape(3,4)
print(a)
[[ 0  1  2  3]
 [ 4  5  6  7]
 [ 8  9 10 11]]
print(a.sum())
 66
print(a.sum(0))  # across rows for ech column
 [12 15 18 21]
print(a.sum(1))  # across columns for each row
 [ 6 22 38]
```

 In sum and similar methods, such as prod, the axis argument is the one to be _summed out_.  In these examples, the first sum uses all elements, resulting in a single value.  In the second two examples, we reduce the rank by one and the shape of the result removes the summed axis.

## Array Attributes

An Ndarray is a class with attributes that can be accessed.

```no-highlight
>>>A=np.random.random((10,10))
>>>print(A.shape)
(10,10)
>>>print(A.size)
100
>>>print(A.ndim)
2
>>>print(A.dtype)
dtype('float64')
```

## Array Operations

NumPy provides many functions, including a large selection of mathematical functions, that accept array arguments as well as scalars.  They operate on the arrays _elementwise_.  The arithmetic operators are also defined to work elementwise on arrays; this is called _operator overloading_ since these operators can work either on ordinary scalar numerical variables or on Ndarrays.

```python
T=numpy.ones(4)
Z=3.0*T+numpy.ones_like(T)
I=numpy.array([1,0,0,0])
A=math.pi*I
B=numpy.sin(A)
C=A/B  #remember: elementwise
```

Array operations and NumPy built-in functions can be many times faster than loops, so they should be utilized whenever possible.

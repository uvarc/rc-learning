---
title: Working with Ndarrays
toc: true
type: docs
draft: false
weight: 102

menu:
    python_introduction:
        parent: NumPy, Matplotlib, SciPy
        weight: 102
---

Numpy arrays are easy to use and have many properties and built-in functions that make them a good choice for many applications, especially but not exclusively working with numerical data. 

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

**Exercise**

Generate an NxM two-dimensional array `A` with the smallest element 1 and the largest one `N*M*incr`.  Make sure your values are inclusive and the size of the array is correct.  Use reshape to convert from the one-d `arange` to the NxM array. N, M, and incr should be variables.

Create another array B the same shape, such that it is 0.25*A+.01.

Create a third array C that is the elementwise sum of A and B.

Create a fourth array D that sums C over the second axis.  Print the shape, size, and the values of D.

Print the first N-2 rows and M-3 columns of B. Be sure B is large enough. Use she shape of B to check that this is true.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/exercises/numpy_arrays.py" lang="python" >}}
{{< /spoiler >}}

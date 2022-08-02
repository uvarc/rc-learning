---
title: NumPy
toc: true
type: docs
draft: false
weight: 101

menu:
    python_introduction:
        parent: NumPy, Matplotlib, SciPy
        weight: 101
---

NumPy is very powerful and our introduction will barely touch on its most important features.

## Arrays

An __array__ is an ordered data structure that contains _elements_ all of the same type. NumPy introduces a new Python data type called the __Ndarray__ (n-dimensional array).  Like all ordered structures in Python, each element is addressed by integer _indices_.  An array has one or more _dimensions_, which NumPy generally calls _axes_.  The _bounds_ are the lowest and highest indexes.  The lower bound of an Ndarray is always 0. The _rank_ is the number of dimensions.  Arrays should be regarded as fixed size once they are set up. NumPy arrays are _mutable_, so elements may be changed in place.

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

### Array Attributes

An Ndarray is an object with _attributes_ that can be accessed.

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
The `shape` is a tuple that contains the extents of each of the dimensions.  The `size` is the total number of elements in the array.  The `ndim` is the number of dimensions (rank) of the array.

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

**Exercise**

Use the zeros function for the following:

* Initialize an array A1 of rank 1 with size 4 and type double.
* Initialize an array IU of rank 1 with size 4 and type integer.
* Initialize an array M1 of rank 1 with size 4 and type Boolean.
* Print each of the arrays you just created.
* Initialize a rank-3 array to arbitrary elements.

Print each array.

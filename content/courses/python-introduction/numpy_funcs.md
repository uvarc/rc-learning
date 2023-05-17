---
title: NumPy Functions
toc: true
type: docs
draft: false
weight: 103

menu:
    python-introduction:
        parent: NumPy, Matplotlib, SciPy
        weight: 103
---

NumPy provides many built-in functions for array manipulations, mathematical/statistical calculations, and reading files.

### Reading and Writing Files

NumPy includes several functions that can simplify reading and writing files.  For files with a simple spreadsheet-like structure, `loadtxt` works well.  The first argument can be either a file name or a file handle.

```python
x,y=np.loadtxt(f, delimiter=',', usecols=(0, 2), unpack=True)
v=np.loadtxt(f,delimiter=',',usecols=(1,)) #usecols needs tuple
W=np.loadtxt(fin,skiprows=2)
```
If `unpack` is not specified, `loadtxt` returns the values into a rank-2 array; otherwise it returns the values into a tuple of rank-1 arrays.  Columns may optionally be selected with `usecols`.  Header rows can be ignored with `skiprows`.  Other options are available.  The `loadtxt` function assumes the delimiter is whitespace, so if it is another character it must be specified through the `delimiter` argument.

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

### Some Frequently Used NumPy Functions

{{< table >}}
| Array Manipulation | Mathematical Operations    |
| ------------------ | -------------------------- |
| arange             | abs, cos, sin, tan         |
| array              | average, mean, median, std |
| argmin, argmax     | min, max                   |
| all, any, where    | ceil, floor                |
| compress           | dot, matmul                |
| copy               | sum, product               |
| ones, zeros, empty | min, max                   |
| reduce             | nan, isnan                 |
| repeat, reshape    | inf, isinf                 |
| rollaxis, swapaxis | linspace                   |
| transpose          | lstsq                      |
{{< /table >}}

This is just a sample; the full reference can be examined at the [manual](https://docs.scipy.org/doc/numpy/reference/routines.html).

### Random Values

Some NumPy functionality is implemented through _subpackages_.  One of the more widely used subpackage is the `random` module.  Base Python has a random module, but just as the Python `math` module cannot operate on Ndarrays, neither can the base `random` return arrays of numbers.  

There are now two sets of random functions.  The "legacy" functions are in the [RandomState](https://numpy.org/doc/stable/reference/random/legacy.html#numpy.random.RandomState) class.

The `random_sample` function generates uniformly-distributed pseudorandom numbers on the interval [0,1).  

```python
import numpy as np
x=np.random.random_sample()  #a single value
y=np.random.random_sample(10) #a one-d array of 10
z=np.random.random_sample(4,5) #a two-d array of shape 4x5
w=np.random.rand(4,5)  #rand is a wrapper around random_sample
```
Other functions include
```python
np.random.randint(1,11) #a random integer between [1,11) 11 not included
np.random.randint(1,11,size=10) #one-d array of random numbers
np.random.random_integers(1,10,size=10) #like above but inclusive of upper
np.random.choice([2,4,6,8]) #random selection from the sequence
deck=list(range(1,53))
np.random.shuffle(deck) #overwrites its argument
np.random.randn(4,5) #4x5 array of normally-distributed random numbers.
```
The newer class is the [Generators](https://numpy.org/doc/stable/reference/random/legacy.html#numpy.random.Generator) class.  The names of the methods are generally the same.  To invoke Generator functions start off by calling the constructor.  In this example, PCG64 is the random-number generator algorithm.
```python
from numpy.random import Generator, PCG64
rng = Generator(PCG64())
rng.standard_normal()
```

### Ufuncs

Functions that accept both arrays and scalars are called __ufuncs__ for "universal functions".  The mathematical, statistical, and random functions we have discussed are examples of built-in ufuncs.  You can write your own ufuncs easily.  These functions are subject to some restrictions:

* The function must not change its input parameters.  
* The function cannot print anything, or do any other form of IO
* The function cannot exit (stop the run)
* The function must return a single value (no tuples or other compound type)

Functions that adhere to these rules are said to be _pure_.  The prohibition on printing does complicate debugging somewhat.  Your functions must be thoroughly debugged for scalar inputs before testing with arrays. 

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

Example: 
The built-in sum over an array or array slice can replace the corresponding loops, and is much faster.

```python
s=0
for e in V:
    s+=s+e
```

Use instead

```python
s=V.sum()
```

**Exercise**

Download the [bodyfat.csv](/data/bodyfat.csv) file.  The weight is the third collumn and the height is the fourth column (in units of pounds and inches).  Write a program that contains a ufunc for converting pounds to kg and another to convert inches to meters.  Write a ufunc to compute BMI from metric height and weight.  Read the bodyfat.csv file and use the ufuncs appropriately to create a new array of the BMI values.  Look up the NumPy functions to print the mean and standard deviation of the values as well as the maximum and minimum values.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/bmi_code.py" lang="python" >}}
{{< /spoiler >}}

## Resources

Essential documentation for NumPy is at its home [site](https://docs.scipy.org/doc/numpy/index.html).

The documentation includes a beginner's [tutorial](https://numpy.org/doc/stable/user/quickstart.html).


---
title: NumPy and SciPy Functions
toc: true
type: book
draft: false
weight: 110
---

NumPy and SciPy (*Sci*entific *Py*thon) are closely linked and frequently are used together.  Both provide a large selection of built-in functions.

## NumPy Functions 

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


```python
import numpy as np 

def F2C(T):
    return 5.*(T-32.)/9.

TempsF=np.arange(0.,213.,2.)
TempsC=F2C(TempsF)
print(TempsC)
print(F2C(50.))
```

<details>
<summary> Exercise 26 </summary>

Download the [bodyfat.csv](/data/bodyfat.csv) file.  The weight is the third collumn and the height is the fourth column (in units of pounds and inches).  Write a program that contains a ufunc for converting pounds to kg and another to convert inches to meters.  Write a ufunc to compute BMI from metric height and weight.  Read the bodyfat.csv file and use the ufuncs appropriately to create a new array of the BMI values.  Look up the NumPy functions to print the mean and standard deviation of the values as well as the maximum and minimum values.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/bmi_code.py" lang="python" >}}
{{< /spoiler >}}

</details>

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

## SciPy

SciPy builds on NumPy to provide a set of modules and packages that add functions for data analysis and numerical computations.  These include 

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

<details>
<summary> Exercise

```python
import numpy as np
from scipy import linalg
A = np.array([[1, 2], [3, 4]])
b = np.array([[5], [6]])
c=linalg.solve(A, b)
print(c)
```

<details>
<summary> Exercise 27 </summary>
See examples of simple image processing [here](https://www.tutorialspoint.com/scipy/scipy_ndimage.htm) and [here](https://data-flair.training/blogs/image-processing-with-scipy-and-numpy/) and [here](https://note.nkmk.me/en/python-numpy-image-processing/) as well as many other sites.  Anaconda should have the PIL or Pillow (Python Imaging Library) pre-installed.
Use the SciPy `ndimage` to tint a photograph.
1. Import numpy and matplotlab.pyplot. Import the scipy misc package
2. Extract the sample picture "face" (a raccoon).
3. Use matplotlab function imshow to look at the picture.  You may still need to use plt.show().
4. Use the NumPy array(img) function to get an numpy array. Specify dtype='float'.
4. Get the shape of the image array.
5. Tone down the blue by .9 and the red by .95.  The order of color channels in the image is RBG.
6. The multiplication will use floats which are not permitted for an image (the pixel values must be integers between 0 and 255).  Store the tinted array into a new array with dtype='int'.  You can do this in one step if you think about it.
7. Show the new image.  If you use plt.figure() and only one plt.show() you can view the images side by side.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/scipy_img.py" lang="python" >}}
{{< /spoiler >}}

## Resources

Essential documentation for NumPy is at its home [site](https://docs.scipy.org/doc/numpy/index.html).  

The SciPy [reference guide](https://docs.scipy.org/doc/scipy/reference/) is an invaluable resource for SciPy usage.

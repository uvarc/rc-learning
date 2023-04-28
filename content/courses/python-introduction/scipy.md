---
title: SciPy
toc: true
type: docs
draft: false
weight: 120

menu:
    python-introduction:
        parent: NumPy, Matplotlib, SciPy
        weight: 120
---

The SciPy library is part of the "SciPy ecosystem" that also includes NumPy, Sympy, and Pandas.  We will not discuss Sympy but it is a well-developed computer algebra system (CAS) that is also incorporated into several other packages such as SageMath.
Its homepage at [www.scipy.org](www.scipy.org) has details and documentation.  

### Importing SciPy Packages

Importing from scipy alone brings only the base packages, which provide a fairly minimal set of tools.  Individual packages must be imported explicitly.

```python
from scipy import linalg, optimize
```

For an imported package, type `help(name)` to see a summary of available functions.

```python
help(linalg)
```

**Exercises**

Type in the following code to solve a linear system of equations.

```python
import numpy as np
from scipy import linalg
A = np.array([[1, 2], [3, 4]])
b = np.array([[5], [6]])
c=linalg.solve(A, b)
print(c)
```

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
{{< code-download file="/courses/python-introduction/exercises/scipy_img.py" lang="python" >}}
{{< /spoiler >}}

## Resources

The SciPy [reference guide](https://docs.scipy.org/doc/scipy/reference/) is an invaluable resource for SciPy usage.

---
title: "Scientific Image Processing with Python OpenCV"
author: "Karsten Siller"
highlight_style: "github"
date: 2021-11-02T00:00:00-05:00
toc: true  
type: article
draft: false
---
<img src="opencv_logo.png" style="width:60%;height:60%"></img>



# Introduction

From the [OpenCV project documentation](https://docs.opencv.org/master/d1/dfb/intro.html):

> OpenCV (Open Source Computer Vision Library: http://opencv.org) is an open-source library that includes several hundreds of computer vision algorithms. The document describes the so-called OpenCV 2.x API, which is essentially a C++ API, as opposed to the C-based OpenCV 1.x API (C API is deprecated and not tested with "C" compiler since OpenCV 2.4 releases)

This workshop assumes a working knowledge of the Python programming language and basic understanding of image processing concepts.

Introductions to Python can be found [here](/courses/programming_python_scientists_engineers/python-interpreter/) and [here](/courses/python_introduction/).

---

# Getting Started

**Python code examples**

The Python scripts and data files for this workshop can be [downloaded from here](data/opencv-workshop.zip). On your computer, unzip the downloaded folder and use it as working directory for this workshop.

**Python programming environment**

The Anaconda environment from [Anaconda Inc.](https://anaconda.com/) is widely used because it bundles a Python interpreter, most of the popular packages, and development environments. It is cross platform and freely available. There are two somewhat incompatible versions of Python; version 2.7 is deprecated but still fairly widely used. Version 3 is the supported version. 

**Note: The latest Biopython package version (1.77+) requires Python 3.**

1. Visit the [Anaconda download website](https://www.anaconda.com/products/individual#Downloads) and download the installer for Python 3 for your operating system (Windows, Mac OSX, or Linux). We recommend to use the graphical installer for ease of use.

2. Launch the downloaded installer, follow the onscreen prompts and install the Anaconda distribution on your local hard drive.

The [Anaconda Documentation](https://docs.anaconda.com/anaconda/user-guide/getting-started/) provides an introduction to the Ananconda environment and bundled applications. For the purpose of this workshop we focus on the `Anaconda Navigator` and `Spyder`. 

## Navigator

Once you have installed Anaconda, start the Navigator application: 
* [Instructions for Windows](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-nav-win)
* [Instructions for Mac](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-nav-mac)
* [Instructions for Linux](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-nav-lin)

You should see a workspace similar to the screenshot, with several options for working environments, some of which are not installed. We will use `Spyder` which should already be installed. If not, click the button to install the package.

![AnacondaNavigator](/notes/biopython/anaconda-navigator.png)

## Spyder

Now we will switch to Spyder. Spyder is an Integrated Development Environment, or IDE, aimed at Python. It is well suited for developing longer, more modular programs. 

1. To start it, return to the `Anaconda Navigator` and click on the `Spyder` tile. It may take a while to open (watch the lower left of the Navigator). 
2. Once it starts, you will see a layout with an editor pane on the left, an explorer pane at the top right, and an iPython console on the lower right. This arrangement can be customized but we will use the default for our examples. Type code into the editor. The explorer window can show files, variable values, and other useful information. The iPython console is a frontend to the Python interpreter itself. It is comparable to a cell in JupyterLab.

![AnacondaNavigator](/notes/biopython/anaconda-spyder.png)

## Installation of the OpenCV package

It is recommended to install the `opencv-python` package from PyPI using the `pip install` command. Detailed instructions are available [here](https://biopython.org/wiki/Download).

**On your own computer:**
Start the `Anaconda Prompt` command line tool following the instructions for your operating system.
* Start Anaconda Prompt on [Windows](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-prompt-win)
* Start Anaconda Prompt on [Mac](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-prompt-mac), or open a terminal window.
* [Linux:](https://docs.anaconda.com/anaconda/user-guide/getting-started/#open-prompt-lin) Just open a terminal window.

At the prompt, type the following command and press enter/return:
```bash
pip install opencv-python matplotlib
```
This command will install the latest `opencv-python` package version in your current Anaconda Python environment.  The `matplotlib` package is used for plotting and image display. It is part of the Anaconda default packages.

**On Rivanna (UVA's HPC platform):**

[Rivanna](https://www.rc.virginia.edu/userinfo/rivanna/overview/) offers several Anaconda distributions with different Python versions. Before you use Python you need to load one of the `anaconda` software modules and then run the `pip install` command. 

```bash
module load anaconda
pip install --user opencv-python matplotlib
```
**Note:** You have to use the `--user` flag which instructs the interpreter to install the package in your home directory. Alternativley, create your own custom [Conda environment](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html) first and run the `pip install opencv-python matplotlib` command in that environment (without `--user` flag) 

To confirm succesful package installation, start the Spyder IDE (see [here](#spyder)). In the `IPython console` pane, type the following command and press `enter/return`:

```
import cv2
print (cv2.__version__)
```

If the package is installed correctly, the output will show the openCV version number.

# Loading, Displaying and Saving Images

The `imread` function is used to read images from files. Images are represented as a multi-dimensional NumPy arrays. The multi-dimensional properties are stored in an image's `shape` attribute, e.g no. of rows (height) x no. of columns (width) x no. of channels (depth).


```
import cv2

# load the input image and show its dimensions
image = cv2.imread("clown.png")
(h, w, d) = image.shape
print('width={}, height={}, depth={}'.format(w, h, d))
```

**Output:**
```
width=320, height=200, depth=3
```

We can use an openCV function to display the image to our screen. 
```
# open with OpenCV and press a key on our keyboard to continue execution
cv2.imshow('Image', image)
cv2.waitKey(0)
cv2.destroyAllWindows()
```

![](clown.png)

The `cv2.imshow()` method displays the image on our screen. The `cv2.waitKey()` function waits for a key to be pressed. This is important otherwise our image would display and disappear faster than we’d even see the image. The call of `destroyAllWindows()` should be placed at the end of any script that uses the `imshow` function.

>**Note:** Before you run the code through the Spyder IDE, go to `Run` > `Run configuration per file` and select `Execute in dedicated console` first. Then, when you run the code uyou need to actually click the image window opened by OpenCV and press a key on your keyboard to advance the script. OpenCV cannot monitor your terminal for input so if you a press a key in the terminal OpenCV will not notice.  

Alternatively, we can use the `matplotlib` package to display an image.

```
plt.imshow(cv2.cvtColor(R,cv2.COLOR_BGR2RGB))
```

We can use the imwrite() function to save images.  For example:
```
filename = 'clown.png'
cv2.imwrite(image, filename)
```

# Accessing Image Pixels

Since an image's underlying pixel information is stored in multi-dimensional numpy arrays, we can use common numpy operations to slice and dice image regions, including the images channels.

We can use the following code to extract the red, green and blue intensity values of a specific image pixel at x=100 and y=50.

```
(b, g, r) = image[100, 50]
print("red={}, green={}, blue={}".format(r, g, b))
````

**Output:**
```
red=184, green=35, blue=15
```

>Note that the channels of an RGB image are stored in Blue, Green, Red order.  



# Image Slicing and Cropping

It is also very easy to extract a rectangular region of interest from of an image and storing it as a cropped copy. Let's extract the pixels for 60<=x<160 and 320<=y<420 from our original image. The resulting cropped image has a width and height of 100x100 pixels.

```
roi = image[60:160, 320:420]
cv2.imshow('ROI', roi)
cv2.waitKey(0)
````

# Resizing Images

```
resized = cv2.resize(image,(500,500))
```

Note that we are _forcing_ the resized image into a square 500x500 pixel format. To avoid distortion of the resized image, we can calculate the width/height aspect ratio of the original image and use it to calculate the new height based on the original width * aspect ratio (or new width based on original height / aspect ratio). 

```
# resize width while preserving height proportions
width = image.shape[0]
height = image.shape[1]
aspect = width/height
new_width = 500
new_height = int(new_width * aspect)
resized2 = cv2.resize(image,(new_width, new_height))
```

```
# display the two resized images
_,ax = plt.subplots(1,2)
ax[0].imshow(cv2.cvtColor(resized, cv2.COLOR_BGR2RGB))
ax[0].axis('off')
ax[1].imshow(cv2.cvtColor(resized2, cv2.COLOR_BGR2RGB))
ax[1].axis('off')
```



![](clown-resized.png)

# Splitting and Merging of Color Channels

```
# Split color channels
(B, G, R) = cv2.split(image)
# create 2x2 grid for displaying images
_, axarr = plt.subplots(2,2)
axarr[0,0].imshow(R, cmap='gray')
axarr[0,0].axis('off')
axarr[0,0].set_title('red')

axarr[0,1].imshow(G, cmap='gray')
axarr[0,1].axis('off')
axarr[0,1].set_title('green')

axarr[1,0].imshow(B, cmap='gray')
axarr[1,0].axis('off')
axarr[1,0].set_title('blue')

axarr[1,1].imshow(cv2.cvtColor(image, cv2.COLOR_BGR2RGB))
axarr[1,1].axis('off')
axarr[1,1].set_title('RGB')
```

![](clown-split.png)

Let's take the blue and green channel only and merge them back into a new RGB image, effectively masking the red channel.  For this we'll need define a new numpy array with the same width and height as the original image and a depth of 1 (single channel) all filled with zero values. Since the indiviudal channels of an RGB image are 8-bit numpy arrays we choose the numpy uint8 data type.

```
import numpy as np
zeros = np.zeros((image.shape[0], image.shape[1]), dtype=np.uint8)
print (B.shape, zeros.shape)
merged = cv2.merge([B, G, zeros])
_,ax = plt.subplots(1,1)
ax.imshow(cv2.cvtColor(merged, cv2.COLOR_BGR2RGB))
ax.axis('off')
```

![](clown-merged.png)

## Exercise 1

# Filters

## Morphological Filters

## Denoising

OpenCV provide four simple to use denoising tools

1. `cv2.fastNlMeansDenoising()` - works with a single grayscale images
2. `cv2.fastNlMeansDenoisingColored()` - works with a color image.
3. `cv2.fastNlMeansDenoisingMulti()` - works with image sequence captured in short period of time (grayscale images)
4. `cv2.fastNlMeansDenoisingColoredMulti()` - same as above, but for color images.

Common arguments are:
* `h`: parameter deciding filter strength. Higher h value removes noise better, but removes details of image also. (10 may be a good starting point)
* `hForColorComponents`: same as h, but for color images only. (normally same as h)
* `templateWindowSize`: should be odd. (recommended 7)
* `searchWindowSize`: should be odd. (recommended 21)

Let's try this with a noisy version of the clown image. This is a color RGB image and so we'll try the `cv2.fastNlMeansDenoisingColored()` filter.  This is the noisy input image `clown-noisy.png`.

![](clown-noisy.png)

And here's a simple script to demonstrate how it works.

```
import numpy as np
import cv2
from matplotlib import pyplot as plt

noisy = cv2.imread('clown-noisy.png')

# define denoising parameters
h = 15  
hColor = 15
templateWindowSize = 7
searchWindowSize = 21

# denoise and save
denoised = cv2.fastNlMeansDenoisingColored(noisy,None,h,hColor,templateWindowSize,searchWindowSize)
cv2.imwrite('clown-denoised.png', denoised)

# display
plt.subplot(121),plt.imshow(cv2.cvtColor(noisy, cv2.COLOR_BGR2RGB), interpolation=None)
plt.subplot(122),plt.imshow(cv2.cvtColor(denoised, cv2.COLOR_BGR2RGB), interpolation=None)
plt.show()
```

![](clown-noisy-denoised.png)

## Exercise 2

# Thresholding & Segmentation

## Exercise 3

# Image Quantification

## Exercise 4

# Resources

* [Introduction to OpenCV](https://docs.opencv.org/master/d1/dfb/intro.html)
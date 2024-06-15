---
title: Project Set 3
toc: true
type: docs
draft: false
weight: 125
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
---

Functions, modules, NumPy, Matplotlib.

Remember that you need to add the `import numpy` (or commonly used `import numpy as np`) statement in your script before you can use the numpy package.  Similarly, we must import Matplotlib.
```python
import numpy as np
import matplotlib.pyplot as plt
```

## Project 10 
Write a Python script that performs the following operations:
 
a) Create a numpy array, x, of values from -1.0 to 1.0 inclusive, with step sizes of 0.01.  Use numpy.pi (or np.pi) for pi. 

b) Create a numpy array, y, where y = sin(pi*x) + cos(3pi*x/2)

c) Determine and print the mean y value.

d) Determine and print the minimum value of y and the maximum value of y over the range of x.  Also print the corresponding x coordinates where y reaches a minimum and where y reaches a maximum. **Hint:** Look up the argmin and argmax functions.   Pay attention to the difference between index and value.

e) Go back to the first chapter and review how to make a plot of y versus x using 
Matplotlib.  Add code to plot y as a function of x.

f) Add a line to accept user input to specify the x start, x end, and stride values.  Your finished code should get these values from the user, print the values and x-coordinate of the max and min for y, and display a plot of y versus x. Upload the plot for input values of starting x=-2., ending x=2., stride=.01.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_3/numpy_basics.py" lang="python" >}}
{{< /spoiler >}}

## Project 11 

You may use your choice of lists or NumPy arrays for this project.

Download the file [cpi.csv](/data/cpi.csv)
This file is the value of $100 from a baseline in January 1913 (the oldest consistent data available) to January 2020, as computed by the US Bureau of Labor Statistics; this gives us the consumer price index. The ratio of any two years is an estimate of the change in cost of living from the one to the other. 

a) Write a function that takes two years and returns this ratio.  Note that the order will matter.

The derivative of the CPI is a crude estimate of the inflation rate.  Write a function that takes two arrays and returns another array using the formula infl\_rate=(cpi[yr+1]-cpi[yr])/12

Note that the returned array will have one fewer element than the original.

b) Write a program that uses your two functions to

read cpi.csv by any means you choose.

Request two years and a price from the user.  Compute the corresponding price for the second year provided by the user. 

Plot the CPI and your approximation to the inflation rate.  
  Plot inflation rate versus the midpoint of the years (so would start in 
  June 1913, i.e. 1913.5, and end June 2019, i.e. 2019.5).

c) In 1954 a color TV cost approximately $1295 in that year's dollars.  How much would that be in 2020 (as of January) dollars? 

d) Look at the Matplotlib documentation and find how to use subplots to plot CPI and inflation rate one above the other.

e) (More advanced) Read about exceptions in the Files chapter and add them to your code to test for the existence of the file before attempting to open it.  Add with/as to the reading of the file (you can surround a numpy call with this also).

f) (More advanced) Convert your file with the functions into a module.  Isolate the calls to input/output, including plotting, by using a main() function.  Add the `if __name__=="__main__"` so that it will not be invoked if you import your module.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_3/inflation.py" lang="python" >}}
{{< /spoiler >}}

## Project 12
Write a program that reads the file [bodyfat.csv](/data/bodyfat.csv).  

-  Extract the body fat percentage, weight, and height data from each row (the first, third, and fourth columns).  We do not need the age data for the current project.

- Use your BMI function as a ufunc to compute all the BMI values at once and return a new array.

- In your bmistats.py file, add a `main` function that runs a test by computing the BMI for a set of heights and weights and returning the category corresponding to that BMI.  Compute the correct results and add code to compare them with your code's results.

Plot the BMI versus bodyfat as a scatterplot.

- Add the if __name__=="__main__" code so that `main` will be executed only if the file is the main module.  Run your tests.

The bodyfat.csv file contains an outlier, probably due to a typo. Add a function to your bmistats file to find outliers. Find the outlier in your list and remove it (don't forget to remove the corresponding bodyfat element).  
You may use the `np.percentile(a,m)` function to compute the upper and lower quartiles using the IQR.  See this [example](https://www.dasca.org/world-of-big-data/article/identifying-and-removing-outliers-using-python-packages).  
Plot the corrected data.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_3/bmistats.py" lang="python" >}}
{{< /spoiler >}}

## Project 13
Download [cville_2017_april.csv](/data/cville_2017_april.csv), which contains April 2017 weather data for Charlottesville, VA.
- Read the file into appropriate NumPy arrays
- Make a line plot of average wind speed for each day
- Add main titles, and label axes to be more descriptive
- Play with the bottom axis (x axis) to make sure all dates are visible
- Make a bar and line plot showing average wind speed (in bars) and max wind gust (as a line). Add legend to distinguish between the two.
- Make stacked bar chart of minimum and maximum temperatures for each day
- Make grouped bar chart of minimum and maximum temperatures for each day
- Plot the number of each weather 'condition'. Plot sunny days, partly cloudy days, and rain days. There are several ways to do this.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_3/cville_2017_apr.py" lang="python" >}}
{{< /spoiler >}}


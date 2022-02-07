---
title: Project Set 3
toc: true
type: book
draft: false
weight: 140
---

### Project 10

**A** Write a program that obtains the sum of the numbers from 1 to some specified positive (>0) integer N. Request the value of N as console input from the user. 
Write a function to implement the sum.  Be sure it checks for input that is an
integer greater than or equal to 0.  Do not use the Gauss formula, do this via “brute force.”

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/sums_table.py" lang="python" >}}
{{< /spoiler >}}

**B** Modify your program to print a table of the sums of the first M numbers, where now M is read from the user's input.  Check that M is greater than or equal to 1.
Print a header that indicates the columns are Integer and Sum. Try to line up your output as best you can using formats or f-strings.

## NumPy

Remember that you need to add the `import numpy` (or commonly used `import numpy as np`) statement in your script before you can use the numpy package.

### Project 11 
Write a Python script that performs the following operations:
 
a) Create a numpy array, x, of values from -1.0 to 1.0 inclusive, with step sizes of 0.01.  Use numpy.pi (or np.pi) for pi. 

b) Create a numpy array, y, where y = sin(pi*x) + cos(3pi*x/2)

c) Determine and print the mean y value.

d) Determine and print the minimum value of y and the maximum value of y over the range of x.  Also print the corresponding x coordinates where y reaches a minimum and where y reaches a maximum. **Hint:** Look up the argmin and argmax functions.   Pay attention to the difference between index and value.

e) Go back to the first chapter and review how to make a plot of y versus x using 
Matplotlib.  Add code to plot y as a function of x.

f) Add a line to accept user input to specify the x start, x end, and stride values.  Your finished code should get these values from the user, print the values and x-coordinate of the max and min for y, and display a plot of y versus x. Upload the plot for input values of starting x=-2., ending x=2., stride=.01.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/numpy_basics.py" lang="python" >}}
{{< /spoiler >}}

### Project 12 

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
{{< code-download file="/courses/python_introduction/solns/inflation.py" lang="python" >}}
{{< /spoiler >}}

### Project 13
Write a program that reads the file [bodyfat.csv](/data/bodyfat.csv).  

-  Extract the body fat percentage, weight, and height data from each row (the first, third, and fourth columns).  We do not need the age data for the current project.

- Create a file that contains the function you wrote to compute BMI in Exercise 18.  Write a function that takes a BMI value and returns the category (Underweight, Normal, Overweight, Obese I-III) as an integer.  Add another function that takes a list argument and returns the mean of the list elements. Add another function to compute the standard deviation.  Call this file `bmistats.py`

Use your BMI function as a ufunc to compute all the BMI values at once and return a new array.

- In your bmistats.py file, add a `main` function that runs a test by computing the BMI for a set of heights and weights and returning the category corresponding to that BMI.  Compute the correct results and add code to compare them with your code's results.

- Add the if __name__=="__main__" code so that `main` will be executed only if the file is the main module.  Run your tests.

The bodyfat.csv file contains an outlier, probably due to a typo. Add a function to your bmistats file to find outliers. Find the outlier in your list and remove it (don't forget to remove the corresponding bodyfat element).  
You may use the `np.percentile(a,m)` function to compute the upper and lower quartiles using the IQR.  See this [example](https://www.dasca.org/world-of-big-data/article/identifying-and-removing-outliers-using-python-packages).  
Plot the corrected data.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/bmistats.py" lang="python" >}}
{{< /spoiler >}}

## Pandas 
These are in generally in order of difficulty, from easiest to most difficult. We have not covered all of these items in our lectures. There are many good resources online, e.g. you can Google: "python pandas create new dataframe".

### Project 14
Create a new dataframe using the following table:
|  Coach    |    School |  Email  |  Career Wins  | National Championships |
|Tony Bennett| Virginia | tbennett@virginia.edu | 346  | 1  |
|Roy Williams | North Carolina | rwilliams@unc.edu | 871 | 3 |
|Mike Krzyzewski | Duke | coachk@duke.edu | 1132 | 5 |
|Tom Izzo | Michigan State | tizzo@msu.edu | 606 | 2 |
|Jim Boeheim | Syracuse | jboeheim@syracuse.edu | 944 | 1 |

- Begin by importing pandas
- Create a new column that is the ratio of career wins to national championships.
- Rename your columns with a variable name. This is not required but often makes your code more readable. 
- Print a list of all columns.
- split a subset of your dataframe based on some condition. Do it at least twice. Once using .loc and once using .iloc.
- use the groupby() method to group your dataframe in some way that makes sense for your data

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj14.py" lang="python" >}}
{{< /spoiler >}}

### Project 15
Take the following code block:
```
import pandas

df1 = pd.DataFrame({'ID #': [1,2,3,4,5,6,7,8,9,10],
                    'Names': ['John','Dave','Mary','Sarah','Mohammed','Rohan','Prisha','Vijay','Ananya','Raj'],
                    'Country': ['USA','USA','USA','UK','India','India','UK','India','UK','India']})

df2 = pd.DataFrame({'ID #': [1,2,3,4,5,6,7,8,9,10],
                    'Salary': [50000, 60000, 65000, 53000, 59000, 74000, 86000, 41000, 94000, 66000],
                    'Age': [24, 46, 51, 29, 33, 38, 70, 46, 49, 35]})
```

- Join the two dataframes together using the [merge function](https://pandas.pydata.org/pandas-docs/stable/user_guide/merging.html)
- How many people come from each country? (Hint: Don't just count them. Which function allows you to see that easily?)
- Reshape the data and create a pivot table view of people by country using the [pivot_table function](https://www.geeksforgeeks.org/python-pandas-pivot_table/). Also include the name, age, and salary in the results

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/pivot_table_example.py" lang="python" >}}
{{< /spoiler >}}

# Matplotlib
These examples will be used in conjuction with Pandas, as the two libraries are commonly used together. 

### Project 16
Download the file [cigarette-smoking-behaviour-2018-census.csv](/data/cigarette-smoking-behaviour-2018-census.csv), which is about cigarette smoking in New Zealand.
- Read the file into a pandas dataframe
- Make a bar plot in Matplotlib of types of cigarette smokers ('Regular Smoker', 'Ex-Smoker', etc.) and their count
- Because we have a total number of respondents, let's make a new column that is a ratio of # of each category / total number of respondents

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/cigarette_smoking_nz.py" lang="python" >}}
{{< /spoiler >}}
 
### Project 17
Download [cville_2017_april.xlsx](/data/cville_2017_april.xlsx), which contains April 2017 weather data for Charlottesville, VA.
- Read the file into a pandas dataframe
- Make a line plot of average wind speed for each day
- Add main titles, and label axes to be more descriptive
- Play with the bottom axis (x axis) to make sure all dates are visible
- Make a bar and line plot showing average wind speed (in bars) and max wind gust (as a line). Add legend to distinguish between the two.
- Make stacked bar chart of minimum and maximum temperatures for each day
- Make grouped bar chart of minimum and maximum temperatures for each day
- Plot the number of each weather 'condition'. Plot sunny days, partly cloudy days, and rain days. There are several ways to do this.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/cville_2017_apr.py" lang="python" >}}
{{< /spoiler >}}

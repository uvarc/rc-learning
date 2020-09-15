---
title: "Matplotlib, SciPy, and Pandas"
date: 2019-04-29T11:06:47-04:00
draft: false
toc: true
type: docs
weight: 8
menu:
    programming_python_scientists_engineers:
       parent: Programming in Python for Scientists and Engineers
       weight: 8
---

## Episode 8 - Matplotlib, SciPy, and Pandas

{{< youtube UlawTIw6eVg >}}

{{% staticref "files/Episode-8-Guide.pdf" "newtab" %}}Download the Episode Guide{{% /staticref %}}

{{% staticref "files/Episode-8-Exercises.pdf" "newtab" %}}Download the Exercises{{% /staticref %}}

Now that we understand ndarrays, we can start using other packages that utilize them. In 
particular, we're going to look at Matplotlib, SciPy, and Pandas. Matplotlib is a package that can make a wide variety of plots and graphs. SciPy contains many useful mathematical functions as well as a number of subpackages that provide specialized capabilities. And Pandas a Python data analysis library.

We will start with Matplotlib. The following code makes a sample plot.

```python
import numpy as np
import matplotlib.pyplot as plt
x=np.linspace(-4.,4.,401)
y=1./(np.pi*(1.+x**2))
plt.title("Y versus X")
plt.plot(x,y)
plt.show()
```

We import the packages we need. Then we use a NumPy function called `linspace` to 
generate 401 points from -4.0 to 4.0 *inclusive*. (We give `linspace` endpoints and not a 
range.) We compute a function of those points and then make a simple line plot. The 
`plt.show()` is required if you run a script, but if you type directly into a iPython console it will show the plot when it's created.

To make a fancier plot follow the example here:

```python
import numpy as np
import matplotlib.pyplot as plt
x1 = np.linspace(0.0, 5.0)
x2 = np.linspace(0.0, 2.0)
y1 = np.cos(2 * np.pi * x1) * np.exp(-x1)
y2 = np.cos(2 * np.pi * x2)
plt.subplot(2, 1, 1)
plt.plot(x1, y1, 'yo-')
plt.title('A tale of 2 subplots')
plt.ylabel('Damped oscillation')
plt.subplot(2, 1, 2)
plt.plot(x2, y2, 'r.-')
plt.xlabel('time (s)')
plt.ylabel('Undamped')
plt.show()
```

This code sets up two independent variables x1 and x2, and a function defined on each one, y1 and y2. Subplots are used to place multiple plots on the same graph. For this example we set up two rows and one column (2,1,..). The first plot is plot 1 (notice that we count from 1 here) and the second is plot 2. In the first call to `plot` we plot y1 versus x1 using yellow circles jointed by lines. In the second call we use red dots joined by lines. The title must be drawn as part of the first subplot to place it at the top. 

Now let's look at an example of a two-dimensional plot.

```python
import numpy as np
import matplotlib.pyplot as plt
x=np.linspace(-2.*np.pi,2.*np.pi,100)
y=np.linspace(-4.*np.pi,4.*np.pi,200)
X,Y=np.meshgrid(x,y)
Z=np.sin(X)+np.cos(Y)
plt.contour(X,Y,Z)
```

A meshgrid is a set of tuples, one for each grid point, for which each point has both an x and a y value. Thus y values are replicated along X and x values are copied along Y. By this means, the computation of Z has the values of both x and y at each point. For filled contours use

```python
plt.contourf(X,Y,Z)
```


Now we will generate a surface plot of the same function. We need to add an import:

```python
from mpl_toolkits.mplot3d import Axes3D
```

The `figure` function creates a new figure. Without it, the old figure will be overwritten.

```python
fig=plt.figure()
my_axes=fig.add_subplot(111,projection='3d')
my_axes.plot_surface(X,Y,Z)
```
	
We can create many more types of graphs with Matplotlib. Students should look at the gallery and example code at <http://matplotlib.org>.

Now we'll look at SciPy. This is a large collection of modules and subpackages and we will not study them in detail. More information is at <http://scipy.org> (information about affiliated projects, including NumPy, Pandas, and others, is also at that URL). Particularly useful is <https://docs.scipy.org/doc/scipy/reference/>.

Our example uses scipy.linalg to solve a system of linear equations.

x + 2y + 3z = 11
4x + 5y + 6z = 12
7x + 8y + 9z = 13

In matrix form this is

1 2 3 = 11
4 5 6 = 12
7 8 9 = 13

The code to solve this is simple:

```python
from scipy import linalg
A=np.array([[1,2,3],[4,5,6],[7,8,9]])
B=np.array([11,12,13])
X=linalg.solve(A,B)
print(X)
```

We can find many other useful mathematical algorithms in SciPy, including special functions, numerical integration, signal processing, and optimization packages. However, we leave these as an exercise for the student and continue to Pandas.

Pandas is a package for data analytics. It is available within Anaconda and can be installed if it is not present in other installations of Python. It introduces a new data structure called a DataFrame. The DataFrame concept is borrowed from the R programming language. It can be conceptualized as a representation of a spreadsheet. It stores column names from a header, columns in the form of another data structure called a Series, and other information about the data. If the data are of appropriate types they can be extracted into NumPy arrays.


Our example uses another package called seaborn, a package based on Matplotlib for statistical visualizations. In the videos the version of Anaconda we used did not include it by default, so we demonstrated how to install it, but newer versions of Anaconda may include it by default. You can check whether it is included by examining the list of Installed packages in Environments through the Anaconda Navigator. You can also try to import it

```python
import seaborn as sb
```

If this succeeds without generating an error, the package is installed. If it is not present you will need to install it.

We assume for the rest of the episode that you have imported the necessary packages with

```python
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
```

as well as seaborn as sb. Seaborn includes a famous dataset from the 1930s about 
characteristics of various iris cultivars. We will load this dataset and use it to illustrate some of the capabilities of Pandas.

```python
iris=sb.load_dataset('iris')
```

First let's look at the column headers and the first few lines of data.

```python
iris.head()
```

To see the end of the data type

```python
iris.tail(20)
```

This shows the last 20 lines (the default is 5 lines). The column names are stored as

```python
iris.columns
```

while the data values are

```python
iris.values
```

If we so wish, we could use the values attribute to extract the numerical data values into a NumPy array

```python
data_vals=iris.values[:,:4].astype(float)
```

We used the NumPy built-in function astype to convert from a more generalized type 
"object" into an array of floating-point values. 

We can summarize the data with

```python
iris.describe()
```

If we want to extract data about the column labeled `"sepal_width"` we use

```python
sepal_w=iris["sepal_width"]
```

We have shown how to extract the data into a NumPy array, but we can also use the Pandas built-in `iloc` to create a new DataFrame that is a subset of the original.

```python
dset=iris.iloc[:,0:4]
dset.columns
```

Now let's compute some statistics about the data.

```python
iris.mean()
```

This is over all the data, but we may want to group it by species.

```python
iris.groupby('species').mean()
```

Finally, let's plot a histogram of data by species. Without Pandas this could require dozens of lines of NumPy and Matplotlib code. With Pandas it is a single statement.

```python
iris.hist(by=’species’)
plt.show()
```

We hope this episode has inspired you to learn more about the important packages Matplotlib, SciPy, and Pandas. Now that we can manipulate data, we will next learn how to read and write it from and to files.
Tutorials for Matplotlib can be found at

<http://www.scipy-lectures.org/intro/matplotlib/matplotlib.html>

<http://www.randalolson.com/2014/06/28/how-to-make-beautiful-data-visualizations-inpython-with-matplotlib/>

Matplotlib provides a tutorial for a subset of Matplotlib and NumPy called pyplot at

<https://matplotlib.org/users/pyplot_tutorial.html>

Once you are comfortable with Matplotlib you may want to look at Plotly

<https://plot.ly/python/>

Anaconda users can install it with

```python
conda install plotly
```

For more advanced 3D plots, Mayavi is a good option (Python 2.7 only)

<http://www.scipy-lectures.org/packages/3d_plotting/index.html>

Students of earth sciences might be interested in Basemap for plots

<https://matplotlib.org/basemap/>

```python
conda install basemap
```

and xarray for working with data, particularly in NetCDF format, in a Pandas-like syntax.

```python
conda install xarray
```

There are fewer tutorials for SciPy because it is a larger package with multiple subpackages. The standard tutorial for the major packages is at
<https://docs.scipy.org/doc/scipy/reference/tutorial/>

A good general site is
<http://www.scipy-lectures.org/>

They provide a tutorial at
<http://www.scipy-lectures.org/intro/scipy.html>

Another aspect of SciPy is the “scikits.” These are generally less well developed and not as comprehensive as base SciPy packages, but can contain some useful functionality. The most popular is scikits-learn for machine learning
<http://www.scipy-lectures.org/packages/scikit-learn/index.html>

There are a number of Pandas tutorials. The official Pandas site provides a guide at
<http://pandas.pydata.org/pandas-docs/stable/tutorials.html>

A good introductory tutorial is at
<http://synesthesiam.com/posts/an-introduction-to-pandas.html>

<details>
<summary>VIEW EXERCISE SOLUTION (`Episode_8_Solution.py`)</summary>

```python
#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 30 08:37:33 2018

Description: This script demonstrates the basic use of the Pandas package, in particular
the filtering, groupby, aggregation, and plotting functionality. 

Python 2.7

@author: ks
"""

import pandas as pd
import os
from matplotlib import pyplot as plt 
import calendar
from __future__ import print


def read_rawdata(data_file, header_file, ozoneday_col):
    """
    Reads raw data from data_file into a pandas DataFrame with column values
    assigned based on values read from header_file.
    An additional label (ozonday_col) that is not provided by header_file
    is appended to label the last column in the DataFrame.
    Returns the raw data DataFrame object.
    """
    # check if files exist
    if not os.path.isfile(data_file) or not os.path.isfile(header_file):
        return None
    
    # read file with column headers
    datainfo = pd.read_csv(
        header_file,                    
        header=None,                     # this file does not have any column headers
        skiprows=range(2),               # skip first two rows; they contain additional annotations that we dont need
        names=['column name','data type'],    # set column labels
        delimiter=':')                   # the ':' separates the column name from the data type in each row

    # fix column labels by adding missing label for 'Ozone day' to columnnames
    columnnames = list(datainfo['column name'])
    columnnames.append(ozoneday_col)

    # read data file and set column names
    data = pd.read_csv(
        data_file,              # name of txt file to open
        header=None,            # this file does not have any column headers
        names=columnnames,      # set column names
        delimiter=',')          # the ',' separates the column name from the data type in each row
    return data


def convertColumnValues(data, date_col, ozoneday_col):
    """ 
    Returns copy of DataFrame object with conversion of date column values 
    to datetime64 and ozone day colum values to boolean.
    - data : DataFrame object
    - date_col: string that specifies the column with date values
    - ozoneday_col: string that specifies the column with ozone classification
    """
    converted = data
    converted[date_col] = converted[date_col].apply(pd.to_datetime)
    converted[converted.columns[1:]] = converted[converted.columns[1:]].apply(pd.to_numeric, errors='coerce')
    converted[ozoneday_col] = converted[ozoneday_col].astype('bool')
    return converted

    
def ozonedays_per_year(data, date_col, ozoneday_col):
    """ 
    Returns DataFrame with number of ozone days per year.
    - data : DataFrame object
    - date_col: string that specifies the column with date values
    - ozoneday_col: string that specifies the column with ozone classification
    """
    groupparam = [data[date_col].dt.year]
    grouped_od_column = data.groupby(groupparam)[ozoneday_col]
    odays_per_year = grouped_od_column.sum()
    
    # convert resulting DataSeries to DataFrame and rename index to 'Year'
    df = odays_per_year.to_frame(ozoneday_col)
    df.index.names = ['Year']

    # rename column with sum values
    df.rename(index=str, columns={ozoneday_col:'# of ozone days'}, inplace=True)
    return df


def ozonedays_by_month(data, date_col, ozoneday_col):
    """ 
    Returns DataFrame with total number of ozone days (value) by calendar month.
    - data : DataFrame object
    - date_col: string that specifies the column with date values
    - ozoneday_col: string that specifies the column with ozone classification
    """
    # group data and use .sum aggregate function
    groupparam = [data[date_col].dt.month]#dt.to_period('M')
    grouped_od_column = data.groupby(groupparam)[ozoneday_col]
    odays_by_month = grouped_od_column.sum()
    
    # Convert resulting DataSeries to DataFrame and set index to reflect month names rather than month numbers
    df = odays_by_month.to_frame(ozoneday_col)#, index=monthidx)
    monthidx = [calendar.month_abbr[x] for x in range(13)]
    df['Month'] = pd.Series(monthidx) # create new column 'Month' with month labels
    df.set_index('Month', inplace=True, drop=True)
    
    # Rename column with sum values
    df.rename(index=str, columns={ozoneday_col:'# of ozone days'}, inplace=True)
    return df


def precipitation_analysis(data, precp_col, precp_thr, ozoneday_col):
    """
    Returns the GroupBy object and a legend dictinary after peforming the grouping 
    by precipitation value and ozone classification.
    - data : DataFrame object
    - precp_col : string that specifies the column with precipitation data
    - precp_thr : float value used as threshold to categorize precipitation data 
    - ozoneday_col : string that specifies the column with ozone classification
    """ 
    # group data by precp threshold and ozone day classification --> 4 groups
    groupparam = [data[precp_col] > precp_thr,ozoneday_col]
    results = data.groupby(groupparam)
    
    # the group names in results are (bool, bool) tuples 
    # create a dictionary we can use as legend for the four groups
    legenddict = {}
    legenddict[(False,False)]='Precp <= %.2f, no ozone day' % precp_thr
    legenddict[(False,True)]='Precp <= %.2f, ozone day'  % precp_thr
    legenddict[(True,False)]='Precp > %.2f, no ozone day'  % precp_thr
    legenddict[(True,True)]='Precp > %.2f, ozone day'  % precp_thr
    return results, legenddict


def print_ozonedays(title, ozonedays):
    """
    Prints a title string and the content of the ozonedays object.
    """
    print("\n%s" % title)
    # convert the float values to integer-like output
    print(ozonedays.to_string(header=True, index=True, float_format=lambda x: '%.0f' % x))
    
    
def print_precpdata(groupeddata, group_labels):
    """
    Creates and prints the summary statistics for a Pandas GroupBy object.
    - groupeddata : The data object obtained by a groupby operation
    - group_labels : labels for precipitation and ozone day categories
    """
    # define windspeed categorie and stats we're interested in 
    windspeed_cat = ["WSR%d" %ws for ws in range(10)]
    stats = ['min', 'max', 'mean', 'std', 'median','count']
    # iterate over all windspeed categrories, pull out of groupeddata, calculate stats
    for windspeed in windspeed_cat:
        print("-------------------\n\n",windspeed)
        ws_data = groupeddata[windspeed].aggregate(stats)
        ws_data.index.names = group_labels
        print(groupeddata[windspeed].aggregate(stats))
    

def plot_ozonedays(ozonedays, title):
    """
    Createas a bar plot of a DataFrame object
    - ozonedays : a DataFrame object with the data to plot
    - title : string object to use as title for the plot
    """
    # create a new figure and axis object
    fig, ax = plt.subplots()
    ozonedays.plot.bar(title=title, ax=ax)
    xlabel = ozonedays.index.names[0]
    # xlabel is unicode string, so clean up by ignoring special characters
    xlabel = xlabel.encode('ascii','ignore')
    ax.set_xlabel(xlabel)
    ax.set_ylabel('days')

    
def plot_scatter(datagroups, title, xlabel, ylabel, legenddict):
    """
    Createas a scatter plot of values in two columns from multiple data groups 
    - datagroups : GroupBy object obtained via Pandas groupbby function
    - title : title for the plot
    - xlabel : label for the x-axis
    - ylabel : label for the y-axis
    - legenddict : dictionary that maps group names to meaningful legend labels
    """
    fig,ax = plt.subplots()
    for name,group in datagroups:
        plt.scatter(group[xlabel], group[ylabel], s=10, label=legenddict[name])
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.legend(loc='upper left', bbox_to_anchor=(1,1))
    plt.show()    
    

    
if __name__ == '__main__':
    data_file = 'eighthr.data.txt'
    header_file = 'eighthr.names.txt'
    date_label = 'Date'
    ozoneday_label = 'Ozone day'
    pthreshold = 0.5
    
    data = read_rawdata(data_file, header_file, ozoneday_label)
    if (data is not None):
        # analyze data
        data = convertColumnValues(data, date_label, ozoneday_label)
        odays_per_year = ozonedays_per_year(data, date_label, ozoneday_label)
        ocount_by_month = ozonedays_by_month(data, date_label, ozoneday_label)
        precp_groups, legenddict = precipitation_analysis(data, 'Precp', pthreshold, ozoneday_label)

        # print data as tables
        print_ozonedays('Ozone days per year:', odays_per_year)
        print_ozonedays('Aggregate ozone days by month', ocount_by_month)
        print_precpdata(precp_groups, ['Precp > %.2f' % pthreshold, ozoneday_label])
        
        # plot data
        plot_ozonedays(odays_per_year, 'Ozone days per year')
        plot_ozonedays(ocount_by_month, 'Aggregate ozone days by month')
        plot_scatter(precp_groups, 'Windspeed versus Temperature', 'T0', 'WSR0', legenddict)
    else:
        print("Missing data (%s) and/or header (%) file. Data cannot be analyzed." % (data_file, header_file))
```

</details>



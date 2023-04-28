---
title: Displaying Data in Pandas
toc: true
type: docs
draft: false
weight: 136

menu:
    python-introduction:
        parent: Pandas
        weight: 136
---

Pandas relies on Matplotlib for plotting.  We can combine them to work seamlessly with data.  To use any Pandas plotting methods, `matplotlib.pyplot` must be imported. 

## Graphs and Charts

Many basic charts can be created through the `plot` method for Dataframes. 
```python
import matplotlib.pyplot as plt
import pandas as pd
df=pd.read_csv("some_file.csv")
df.plot()  #plot all columns (must be numbers)
df[x='Column2',y='Column4'].plot() #choose columns
```

Example from the Python documentation:
{{< code-download file="/courses/python-introduction/scripts/pandas_plot.py" lang="python" >}}
 
Other types of charts and graphs are available and can be accessed through the `kind` keyword to plot, or by `df.plot.kind()`

* `df.plot.bar()`  
    Vertical bar chart
* `df.plot.barh()`  
    Horizontal bar chart
*  `df.plot.box()`
    Box plot
*  `df.plot.pie()
    Pie chart
* `df.plot.scatter()`
    Scatter plot

```python
df[x='A',y='B'].plot.scatter() 
```

Other keywords to `plot` and its kinds control the appearance of the output.
In addition, there are separate methods `df.hist()` and `df.boxplot()` that have their own sets of arguments.

If multiple columns of a dataframe are compatible numerically, they can be specified and the `plot` method will create a superimposed chart with a legend.  Remember that the index is not a column of the dataframe, so it can be a date.

This example is a modification of another one from Pandas documention.

{{< code-download file="/courses/python-introduction/scripts/multiplot.py" lang="python" >}}

Bar charts are similarly simple to create, with nice default labeling.

{{< code-download file="/courses/python-introduction/scripts/barchart.py" lang="python" >}}

## Tables

A Dataframe is similar to a table, but printing it directly does not always produce good-looking output.  For Jupyter, Pandas has a "Styler" that can make a pretty table from a Dataframe.  It uses Web standards and the output is HTML.  For printing to a plot or paper, the `table` function of Matplotlib can be used within Pandas.

**Example**
This example is based on Pandas documentation, with some modifications, for the HTML output, with the `table` version based on online sources.

{{< code-download file="/courses/python-introduction/scripts/plot_table.py" lang="python" >}}

To see the "pretty" version, paste the text into a Jupyter notebook. If using the "table" version, place that into a separate cell.

## Documentation

The Pandas visualization [documentation](https://pandas.pydata.org/pandas-docs/stable/user_guide/visualization.html) is very thorough and shows a wide range of examples.

**Exercise**

Return to the [bodyfat.csv](/data/bodyfat.csv) file from a previous exercise.
Use Pandas to read the data into a Dataframe.  Use your BMI function from a
previous exercise to compute BMI for each row.  Add a new column for BMI.  Plot BMI versus body fat percentage.  Look up the documentation for `pandas.plot.scatter` for this plot.  Does one value seem out of place?

One way to remove outliers is to compute the 25% and 75% quantiles, take the difference QIF=quantile(75)-quantile(25), then use 1.5*QIF as the threshold, i.e. anything less than quantile(25)-1.5*QIF or quantile(75)+1.5*QIF is rejected.

Figure out a way to set the BMI values that are outside the cutoffs to `np.nan` using the `.loc` method.  Redo the scatter plot.  Pandas automatically removes missing data.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/bmi_pandas.py" lang="python" >}}
{{< /spoiler >}}

---
title: Pandas Plotting and Extensions
toc: true
type: book
draft: false
weight: 125
---

Pandas relies on Matplotlib for plotting.  We can combine them to work seamlessly with data.  To use any Pandas plotting methods, `matplotlib.pyplot` must be imported. 

Many basic charts can be created through the `plot` method for Dataframes. 
```python
import matplotlib.pyplot as plt
import pandas as pd
df=pd.read_csv("some_file.csv")
df.plot()  #plot all columns (must be numbers)
df[x='Column2',y='Column4'].plot() #choose columns
# Several types of graph are possible
df.plot.bar()
df.plot.hist()
df.plot.box()
df[x='A',y='B'].plot.scatter() 
```
The Pandas plot method sets `plt.figure()` internally by default.

Example from the Python documentation:
{{< code-download file="/courses/python_introduction/scripts/pandas_plot.csv" lang="python" >}}
 
<details>
<summary> Exercise 28 </summary>
Return to the [bodyfat.csv](/data/bodyfat.csv) file from a previous exercise.
Use Pandas to read the data into a Dataframe.  Use your BMI ufuncs from Exercise
26 to compute BMI for each row.  Add a new column for BMI.  Plot BMI versus body fat percentage.  Look up the documentation for `pandas.plot.scatter` for this plot.  Does one value seem out of place?

One way to remove outliers is to compute the 25% and 75% quantiles, take the difference QIF=quantile(75)-quantile(25), then use 1.5*QIF as the threshold, i.e. anything less than quantile(25)-1.5*QIF or quantile(75)+1.5*QIF is rejected.

Figure out a way to set the BMI values that are outside the cutoffs to `np.nan` using the `.loc` method.  Redo the scatter plot.  Pandas automatically removes missing data.

{{< spoiler text="Example solution" >}}
{{code-download file="/courses/python_introduction/solns/bmi_pandas.py" lang="python" >}}
{{ /spoiler >}}

</details>

### A Detailed Example

Our example will be based on the American baseball player Mike Trout's statistics, through 2019.
To follow along, download the data [file](/data/MikeTroutData.csv).

Start by reading it into a Pandas Dataframe.

```python
import pandas as pd
import matplotlib.pyplot as plt  
import matplotlib.ticker as ticker

#read data from the csv file into a Dataframe object called 'df'
df = pd.read_csv("MikeTroutData.csv")

#extract columns into variables.
year = df['Year']
hits = df['H']
at_bats = df['AB']
home_runs = df['HR']
salary = df['Salary']
```
Make a simple bar plot showing hits on the Y axis, year on the X axis. If we are willing to accept all the defaults, all that is necessary is to invoke the Matplotlib bar method directly on a subset of the Dataframe.

```python
df.plot.bar(x="Year",y="H")
```
{{< figure src="/courses/python_introduction/imgs/pandas_barchart.png" >}}

We can also use Matplotlib directly with our extracted variables.  The figure() method advances to a new plot.
```python
plt.figure()
plt.bar(year, hits)
```
{{< figure src="/courses/python_introduction/imgs/barplot1.png" >}}

Let's add some labels to make this more readable.

```python
plt.xlabel('Year')
plt.ylabel('# of Hits')
plt.suptitle('Mike Trout Hits per year')
plt.bar(year, hits)
```
{{< figure src="/courses/python_introduction/imgs/barplot2.png" >}}

Turn it into a horizontal barchart and change the color of the bars.

```python
plt.figure()
plt.xlabel('# of Hits')
plt.ylabel('Year')
plt.suptitle('Mike Trout Hits per year')
plt.barh(year, hits, color='red')
```
{{< figure src="/courses/python_introduction/imgs/barplot3.png" >}}

Make a line plot using the .plot() function instead of a barchart.

```python
plt.figure()
plt.xlabel('Year')
plt.ylabel('# of Hits')
plt.grid()
plt.plot(year, hits)
```

{{< figure src="/courses/python_introduction/imgs/lineplot1.png" >}}

We can superimpose a line and a bar plot. We will represent 'At Bats' by a red line and 'Hits' by blue bars. 

```python
plt.figure()
plt.xlabel('Year')
plt.ylabel('# of Hits')
plt.plot(year, at_bats, color='red')
plt.bar(year, hits)
```
{{< figure src="/courses/python_introduction/imgs/barline1.png" >}}

The y-label we used before is no longer appropriate, so let us add a legend.

```
plt.figure()
plt.xlabel('Year')
plt.plot(year, at_bats, color='red', label='At Bats')
plt.bar(year, hits, label='Hits')
plt.legend()        
```
{{< figure src="/courses/python_introduction/imgs/plotwithlegend1.png" >}}

Without an intervening `figure()` method, plots will be stacked.  We can utilize that to stack the bars.  We are also rotating the x-axis tick marks and labels 45 degrees.

```
plt.figure()
plt.xlabel('Year')
plt.bar(year, hits, label='Hits')
plt.bar(year, home_runs, label='Home Runs')
plt.legend()

plt.xlabel('Year')
plt.xticks(rotation=45)
plt.xticks(year)                #shows all years in label
```
{{< figure src="/courses/python_introduction/imgs/stackedbar1.png" >}}

To make a grouped bar chart, do the same as a stacked bar and move the position of one of the bars as shown below. Notice that for the second bar(), the first argument is 'year+.2'. This shifts the position on the x axis .2 units to the right of the default starting point.
```python
plt.xlabel('Year')
plt.xticks(rotation=45)
plt.xticks(year)                #shows all years in label

plt.bar(year, hits, width=.2, label='Hits')
plt.bar(year+.2, home_runs, width=.2, label='Home Runs')
plt.legend()
```
{{< figure src="/courses/python_introduction/imgs/groupedbar1.png" >}}

Suppose you are interested in exactly how many hits each bar represents. We can iterate over each bar to label it with the corresponding number.

```
plt.xlabel('Year')
plt.xticks(rotation=45)
plt.xticks(year)                #shows all years in label

plt.ylabel('# of Hits')
plt.suptitle('Mike Trout Hits per year')

for bar in plt.bar(year, hits):
    plt.text(bar.get_x() + .4,              #x position of label
             bar.get_height() - 20,         #y position of label
             bar.get_height(),              #actual value of label
             ha='center',
             va='bottom')
```
{{< figure src="/courses/python_introduction/imgs/barwithlabels.png" >}}

Let's plot how much Mike Trout is paid per home run. 

```python
cost_per_home_run = salary/home_runs

plt.xlabel('Year')
plt.xticks(rotation=45)
plt.xticks(year)

#change Y Axis to show dollar amount
fig, ax = plt.subplots()
formatter = ticker.FormatStrFormatter('$%1.0f')
ax.yaxis.set_major_formatter(formatter)

plt.ylabel('Price')
plt.suptitle('Mike Trout Yearly Cost Per Home Run')
plt.bar(year, cost_per_home_run)
```
{{< figure src="/courses/python_introduction/imgs/Formatter.png" >}}

Many plotting options can be applied directly to the Dataframe object, without the need to extract the variables. See the documentation for the Pandas [plot method](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.plot.html).

<details>
<summary> Exercise 29 </summary>

Download the file [pandas_demo.ipynb](/data/pandas_demo.ipynb) and the data files [eighthr.data](/data/eighthr.data) and [eightr.names](/data/eighthr.names). If you are using Windows, check that it does not append ".txt" to the data files.  You may need to open File Explorer, go to View, and check "File name extensions."  Open the notebook in JupyterLab or Jupyter.  Go through the exercises in each cell.

</details>

### Documentation

The Pandas visualization [documentation](https://pandas.pydata.org/pandas-docs/stable/user_guide/visualization.html) is very thorough and shows a wide range of examples.

## Xarray

Xarray is a package that provides an extension of the Pandas dataframe to more than two-dimensional data. It is not included in the base Anaconda installation so must be added.  It provides two fundamental data structures.

The `DataArray` data structure holds a multi-dimensional array with optional _labels_.  It contains the values, coordinates, dimensions if different from coords, attributes, and a name.  Only the values are required; the rest are the labels and are optional.  The data model is similar to that used by "self describing" data formats such as HDF5 and NetCDF, which are widely used in several scientific fields.  Values may be loaded from a NumPy array or a Pandas Dataframe or Series.

The extension to the DataFrame is called a `DataSet`.  It is specifically based on the NetCDF data model.  Each of the variables contained in the DataSet is a DataArray.  

Example from the Xarray documentation:
```no-highlight
temp = 15 + 8 * np.random.randn(2, 2, 3)
precip = 10 * np.random.rand(2, 2, 3)

lon = [[-99.83, -99.32], [-99.79, -99.23]]
lat = [[42.25, 42.21], [42.63, 42.59]]

# for real use cases, its good practice to supply array attributes such as
# units, but we won't bother here for the sake of brevity
ds = xr.Dataset(
     {
         "temperature": (["x", "y", "time"], temp),
         "precipitation": (["x", "y", "time"], precip),
     },
     coords={
         "lon": (["x", "y"], lon),
         "lat": (["x", "y"], lat),
         "time": pd.date_range("2014-09-06", periods=3),
         "reference_time": pd.Timestamp("2014-09-05"),
     },
)
```

Since Pandas 0.20, Xarray is the recommended package to manage higher-dimensional data, replacing the Pandas `Panel` data structure.

The Xarray documentation is available [here](https://pip.pypa.io/en/stable/getting-started/).

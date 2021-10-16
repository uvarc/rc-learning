---
title: Pandas 
toc: true
type: book
draft: false
weight: 120
---

[Pandas](pandas.pydata.org) is a Python data analysis library.  It was developed to bring a portion of the statistical capabilities of R into Python.  Pandas accomplishes this by introducing the Series and Dataframe objects to represent data, and incorporating Matplotlib and many features of NumPy into these objects to simplify data representation, analysis, and plotting.  Pandas works with the [statsmodel](http://www.statsmodels.org/stable/index.html) and [scikit-learn](https://scikit-learn.org/stable/) packages for data modeling.  Pandas supports data alignment, missing data, pivoting, grouping, merging and joining of datasets, and many other features for data analysis.

Note that we will adhere to a convention of
```python
import pandas as pd
```
This naming scheme is not required, but it is very common, much like `np` for NumPy.

### Series

The Series data structure consists of an index plus data.  It is similar to a dictionary with the differences that 

* the size is fixed
* requesting a non-existent index results in a Key Error (no dynamic creation)

Series objects are one-dimensional and can contain any type.  The indices are treated like row labels.

This simple example loads a Series with normally-distributed random numbers, then prints some of them, prints the basic statistics, and creates a line plot.

```python
import numpy as np
import matplotlib.pyplot as plt 
import pandas as pd
randns=pd.Series(np.random.randn(1000))
randns.head()
randns.tail()
randns.describe()
randns.plot()
```

If using Spyder, be sure to print(rands.head()), etc.  

Series can be sliced

```
select=randns[1:41]
```

This slices both the index and the data.  To extract values with specific row numbers, use the `iloc` method (note the square brackets).

```
some_vals=randns[1:41]
one_val=randns.iloc[11]
other_vals=randns.iloc[3:9]
```

Series are conceptually like a single column of a spreadsheet, with any headers omitted.  Values are similar to a NumPy Ndarray and most NumPy methods can be applied to the Series data, provided they are defined on the data type.  Missing data, represented by `np.nan` by default, will be omitted.

However, if the values are needed as an actual Ndarray they must be converted.

```
vals=randns.to_numpy()
```

We can load a Series with a dictionary.

```
scores=pd.Series({'Cougars':11,'Bears':9,'Cubs':8,'Tigers':6})
```

We can still slice it

```
scores[1:3]
```

We can still use `iloc` to extract by row number.  We can also use `loc` to extract by the row name.

```
scores.loc['Cubs']
```

Remember to print if using Spyder, or to run in the interpreter pane.

### Dataframes

The most important data structure in Pandas is the _Dataframe_.  It can be conceptualized as a representation of a spreadsheet.  Dataframes are two-dimensional.  Each column has a name, which can be read from the headers of a spreadsheet, rows are numbered, and datatypes may be different in different columns.  Alternatively, a Dataframe may be regarded as a dictionary with values that can be lists, Ndarrays, dictionaries, or Series.

The Dataframe is _mutable_ type.

We can create a Dataframe by passing a dictionary. Consider a simple grade-book example.

```python
grade_book=pd.DataFrame({"Name":["Jim Dandy","Betty Boop","Minnie Moocher","Joe Friday","Teddy Salad"],
	"Year":[2,4,1,2,3],"Grade":[85.4,91.7,73.2,82.3,98.5]})

print(grade_book)
```

The result of printing the Dataframe should look like this:

| | Name           | Year | Grade |
|-|--------------- | ---- | ----- |
|0| Jim Dandy      | 2    | 85.4  |
|1| Betty Boop     | 4    | 91.7  |
|2| Minnie Moocher | 1    | 73.2  |
|3| Joe Friday     | 2    | 82.3  |
|4| Teddy Salad    | 3    | 98.5  |

Now we can apply methods to the `grade_book` Dataframe.

```python
grade_book.describe()
```

We can access individual columns by name.  If the name of the column is a valid Python variable name then we may use it as an attribute; otherwise we must refer to it as we would to a dictionary key.

```python
grade_book.Name 
grade_book['Name']
grade_book.Grade.mean()
```

Columns can be deleted

```python
grade_book.drop(columns='Year')
```

A new column can be appended (the number of rows must be the same)

```python
grade_book["Letter Grade"]=["B","A","C","B","A"]
```

Extract values into an Ndarray 

```python
grades=grade_book["Grade"].values 
```

We can directly apply basic Matplotlib commands to Dataframe columns  

```python
grade_book.Grade.hist()
grade_book.Grade.plot()
```

### Accessing Rows and Columns

Pandas Dataframe rows are ordered, so you can call rows by index, just as for lists or NumPy Ndarrays.

```python
grade_book[2:4]   #returns rows with index 2,3
```
Remember that the upper bound is not included, as is usual for Python ranges.

You can use `.iloc` to achieve the same result. The `iloc` method selects rows based on their integer indexes.

```python
grade_book.iloc[[2,3]]
```

The `.loc` method is more flexible. It allows you to access row indexes based on the value of a column. `loc` is _label-based_ rather than index-based.  It is used to extract a block of rows and columns by row identifier and column name, or by a Boolean.

```
first_student= grade_book.loc[0,'Name']
second_years = grade_book.loc[grade_book['Year'] == 2]
```
The `first_student` variable picks out the content of row 0, column Name, which is a string.  The `second_years` object is a new Dataframe containing the information about all the students who are in year 2.

If we wished to extract the students whose grade is 90 or above, we could use
```python
top_students=grade_book.loc[grade_book['Grade']>=90]
```

### Reading Files

Pandas easily reads files in CSV (comma separated values) format.  The separator does not have to be a comma, but anything else must be specified through the `sep` keyword argument.

Suppose we have a file `weather.txt` containing weather data over a year for one site.  It is in comma-separated form with exactly one line of column headers.

```
wdata=pd.read_csv('weather.txt')
```

The `read_csv` function stores the column headers as the column names and the rest of the data as the columns in a dataframe object.  To see the data we would type 

```
wdata 
```

The `read_excel` method is equivalent for files stored in Excel format (.xls, .xlsx, and similar).

```
my_data=pd.read_excel('weather.xlsx')
```

### Filtering and grouping

Pandas offers a number of ways to reorganize, group, and extract data.  Conditionals are accepted much as for NumPy arrays 

```
freezing_days=wdata[data.max_temp<=0.]
```

Conditionals may be compounded

```
cold_days=wdata[(data.max_temp<=0.) & (data.max_temp>-10.)]
```

Groups can be created with `groupby`

```
temps={}
for cover, cover_data in wdata.groupby("cloud_cover"):
    temps[cover]=cover_data.mean_temp.mean()
```

We can extract values corresponding to a quantity.  For our grade-book example, we can create a new dataframe for the students who received an "A"

```
grade_book["Letter Grade"]=["B","A","C","B","A"]
top_students=grade_book[grade_book["Letter Grade"]=="A"]
top_students
```

We can create _pivot tables_ to reorganize the data.  Continuing with the grade book, we can make the names into the index and organize by grades.
(For this example the result will have quite a bit of missing data, but a more complete grade book could have more scores.)

```
student_grades=grade_book.pivot(index="Name",columns="Letter Grade",values="Grade")
student_grades
```

### Missing Data

Pandas can easily handle missing data.  Conventionally, we use the IEEE Not A Number (nan) indicator for missing data, since it is easy to check for its presence.  We can also use None; Pandas treats None and NAN as essentially the same thing.  So to check for missing data in our `student_grades` dataframe we can use `isnull()` and `notnull()`, both of which are Boolean functions so will return True or False.

```
student_grades.notnull()
```

We can fill missing values with a quantity.

```
new_grades=student_grades.fillna(0.)
```

Many other options exist.  

### Combining Pandas and Matplotlib

Pandas relies on Matplotlib for plotting.  We can combine them to work seamlessly with data.  Our example will be based on the American baseball player Mike Trout's statistics, through 2019.

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

### Exercise

Download the file [pandas_demo.ipynb](/data/pandas_demo.ipynb) and the data files [eighthr.data](/data/eighthr.data) and [eightr.names](/data/eighthr.names). If you are using Windows, check that it does not append ".txt" to the data files.  You may need to open File Explorer, go to View, and check "File name extensions."  Open the notebook in JupyterLab or Jupyter.  Go through the exercises in each cell.

### Resources

The Matplotlib [gallery](https://matplotlib.org/gallery.html) provides many examples, with downloadable source files.  Many of our examples are taken directly from this site.

Many Pandas tutorials are available online. A good starting point is [here](https://pandas.pydata.org/pandas-docs/stable/getting_started/tutorials.html).

A repository of pandas practice projects is [located here](https://www.geeksforgeeks.org/pandas-practice-excercises-questions-and-solutions/)


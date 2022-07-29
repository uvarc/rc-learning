---
title: Introduction to Pandas 
toc: true
type: book
draft: false
weight: 130
---

[Pandas](https://pandas.pydata.org) is a Python data analysis library.  It was developed to bring a portion of the statistical capabilities of R into Python.  Pandas accomplishes this by introducing the Series and DataFrame objects to represent data, and incorporating Matplotlib and many features of NumPy into these objects to simplify data representation, analysis, and plotting.  Pandas works with the [statsmodel](http://www.statsmodels.org/stable/index.html) and [scikit-learn](https://scikit-learn.org/stable/) packages for data modeling.  Pandas supports data alignment, missing data, pivoting, grouping, merging and joining of datasets, and many other features for data analysis.

Note that we will adhere to a convention of
```python
import pandas as pd
```
This naming scheme is not required, but it is very common, much like `np` for NumPy.

Pandas introduces new data structures, the most important of which are the Series and the DataFrame.

## Series

The Series data structure consists of an index plus data.  It is similar to a dictionary with the differences that 

* the size is fixed
* requesting a non-existent index results in a Key Error (no dynamic creation)

Series objects are one-dimensional and can contain any type.  The indices are treated like row labels.

This simple example loads a Series with normally-distributed random numbers, then prints some of them, prints the basic statistics, and creates a line plot.

{{< code-download file="/courses/python_introduction/scripts/pandas_series.py" lang="python" >}}

Series are conceptually like a single column of a spreadsheet, with any headers omitted.  Values are similar to a NumPy Ndarray and most NumPy methods can be applied to the Series data, provided they are defined on the data type.  Missing data, represented by `np.nan` by default, will be omitted from the data used by these methods.

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

## DataFrames

The most important data structure in Pandas is the _DataFrame_.  It can be conceptualized as a representation of a spreadsheet.  DataFrames are two-dimensional.  Each column has a name, which can be read from the headers of a spreadsheet, rows are numbered, and datatypes may be different in different columns.  Alternatively, a DataFrame may be regarded as a dictionary with values that can be lists, Ndarrays, dictionaries, or Series.

The DataFrame is a _mutable_ type.

We can create a DataFrame by passing a dictionary. Consider a simple grade-book example.

{{< code-snippet >}}
grade_book=pd.DataFrame({"Name":["Jim Dandy","Betty Boop","Minnie Moocher",
                                 "Joe Friday","Teddy Salad"],
                         "Year":[2,4,1,2,3],"Grade":[85.4,91.7,73.2,82.3,98.5]})
{{< /code-snippet >}}

```python
print(grade_book)
```

The result of printing the DataFrame should look like this:

| | Name           | Year | Grade |
|-|--------------- | ---- | ----- |
|0| Jim Dandy      | 2    | 85.4  |
|1| Betty Boop     | 4    | 91.7  |
|2| Minnie Moocher | 1    | 73.2  |
|3| Joe Friday     | 2    | 82.3  |
|4| Teddy Salad    | 3    | 98.5  |

Now we can apply methods to the `grade_book` DataFrame.

```python
grade_book.describe() #Summarizes
grade_book.head()     #print first few lines
grade_book.tail()     #print last lines
```
The `head` and `tail` methods are more useful for longer datasets. We can provide them parameters to print a specified number of rows other than the default 5.
```python
grade_book.head(2)
grade_book.tail(1)
```

### Accessing and Modifying Data

We can access individual columns by name.  If the name of the column is a valid Python variable name then we may use it as an attribute; otherwise we must refer to it as we would to a dictionary key.

```python
grade_book.Name
grade_book['Name']
grade_book.Grade.mean()
```
An individual column is of type Series.

Columns can be deleted. This does not change the original dataframe; it returns a new dataframe.  To overwrite the dataframe, add an option `inplace=True`.

```python
grades_only=grade_book.drop(columns='Year')
```

A new column can be appended (the number of rows must be the same)
```python
grade_book["Letter Grade"]=["B","A","C","B","A"]
```

Extract values into an Ndarray
```python
grades=grade_book["Grade"].values
```

To add a row, we should use `concat`.  The number of columns must match.
```python
new_row=pd.DataFrame([["Dinsdale Piranha",1,75.5]],columns=["Name","Year","Grade"])
grade_book=pd.concat([grade_book,new_row],axis=0)
```

To delete a row
```python
grade_book.drop([len(grade_book)-1])
```
This drops the last row.

### Plots

We can directly apply basic Matplotlib commands to DataFrame columns

```python
grade_book.Grade.hist()
grade_book.Grade.plot()
```

**Exercise**

Set up a dataframe with the following "weather" data (it is synthetic):
```no-highlight
Date, Minimum Temp, Maximum Temp
"2000-01-01 00:00:00",-5.87,8.79
"2000-01-02 00:00:00",-3.82,4.78
"2000-01-03 00:00:00",-4.58,5.10
"2000-01-04 00:00:00",-6.40,2.68
"2000-01-05 00:00:00",-5.50,6.18
"2000-01-06 00:00:00",-3.29,4.50
```

Run `describe`.  Print the mean values.  Extract the minimum temperature and the maximum temperature into Ndarrays.  Plot the data using Pandas.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/exercises/fake_weather_data.py" lang="python" >}}
{{< /spoiler >}}

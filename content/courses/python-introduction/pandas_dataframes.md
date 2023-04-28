---
title: Working with Dataframes
toc: true
type: docs
draft: false
weight: 132

menu:
    python-introduction:
        parent: Pandas
        weight: 132
---

## Selecting and Manipulating Data

Many of the operations we perform with data involve working with parts of it.  Pandas has many powerful ways to extract and reorganize data.

### Accessing Rows and Columns

#### Series

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
Pandas Dataframe rows are ordered, so you can call rows by index, just as for lists or NumPy Ndarrays.

```python
grade_book[2:4]   #returns third and fourth rows
```
Remember that the upper bound is not included, as is usual for Python ranges.

You can use `.iloc` to achieve the same result. The `iloc` method selects rows based on their integer indexes.

```python
grade_book.iloc[[2,3]]
```

The `.loc` method is more flexible. It allows you to access row indexes based on the value of a column. `loc` is _label-based_ rather than index-based.  It is used to extract a block of rows and columns by row identifier and column name, or by a Boolean.

```
first_student= grade_book.loc[0,'Name']
first_student_data = grade_book.loc[0]
```
The `first_student` variable picks out the content of row 0, column `Name`, which is a string in this case.  The `first_student_data` object is a new Series containing the information about the student in the first row. 

Note that when the index is an integer, `iloc[0]` and `loc[0]` are equivalent.  However, the index need not be the default integers and `loc` can use a more general type.

We can extract multiple specified columns into a new Dataframe by providing a list of columns.
```python
grades_only=grade_book.loc[:,["Name","Grade"]]
```
Observe that label slicing is _inclusive_.

### Specifying Rows and Columns

By default, Pandas row indexes are integers starting at 0.
```python
weather=pd.DataFrame({"Date":["2000-01-01 00:00:00","2000-01-02 00:00:00",
                              "2000-01-03 00:00:00","2000-01-04 00:00:00",
                              "2000-01-05 00:00:00","2000-01-06 00:00:00"],
                      "Minimum Temp":[-5.87,-3.82,-4.58,-6.40,-5.50,-3.29],
                      "Maximum Temp":[8.79,4.78,5.10,2.68,6.18,4.50],
                      "Cloud Cover":[3,5,3,2,3,5]})

print(weather)
print(weather.index)
for s in weather.index:
    print(weather.loc[s])
```
```no-highlight
                  Date  Minimum Temp  Maximum Temp  Cloud Cover
0  2000-01-01 00:00:00         -5.87          8.79            3
1  2000-01-02 00:00:00         -3.82          4.78            5
2  2000-01-03 00:00:00         -4.58          5.10            3
3  2000-01-04 00:00:00         -6.40          2.68            2
4  2000-01-05 00:00:00         -5.50          6.18            3
5  2000-01-06 00:00:00         -3.29          4.50            5
RangeIndex(start=0, stop=6, step=1)
Date            2000-01-01 00:00:00
Minimum Temp                  -5.87
Maximum Temp                   8.79
Cloud Cover                       3
Name: 0, dtype: object
# Rest of loop output omitted
```
We would probably prefer to access the data by date, rather than trying to determine which rows to use.  Pandas has a built-in [date generator](https://pandas.pydata.org/docs/reference/api/pandas.date_range.html):
```python
date_ranges=pd.date_range("2000-01-01",periods=6)
date_ranges
DatetimeIndex(['2000-01-01', '2000-01-02', '2000-01-03', '2000-01-04',
               '2000-01-05', '2000-01-06'],
              dtype='datetime64[ns]', freq='D')
```
The default is an interval (`freq`) of one day. Start and end dates can be specified.  Multiple date formats are accepted. For this example, however, we will make a list and use it as the index.  We can then select items by dates.

#### Accessing and Renaming the Column Names

If we'd like to save some typing, we can rename columns to make them conform to Python variable-naming rules.  Then we can treat the column name as an attribute.
```python
weather_df.columns=["Tmin","Tmax","Cloud Cover"]
```
Order matters, and each column must be included even if we do not wish to rename it.  In order to rename only certain columns, we can use the `rename` method, which uses a dictionary format.
```python
weather_df.rename(columns={'Minimum Temp':'Tmin','Maximum Temp':'Tmax'},inplace=True)
```

Be careful with the period ("dot") notation for column names, since if one happens to coincide with a built-in Pandas attribute or method, the method will be assumed, which may result in unpredictable or incorrect behavior. 

Without an assignment, the `columns` attribute holds the names of the columns.

{{< code file="/courses/python-introduction/scripts/weather_df.py" lang="python" >}}

{{< code file="/courses/python-introduction/scripts/weather_df.out" lang="no-highlight" >}}

This range syntax for the row range is _not_ inclusive, as is usual for Python.

#### Extracting Row Indices

The `index` attribute contains the index values
```python
weather_df.index
```
To rename the indexes we use `rename` much as for the column names.  
```python
weather_df.rename(index={'2000-01-01 00:00:00':'2000-01-01 00:00:10'},inplace=True
```

We can obtain the equivalent NumPy values for the indices.  We can also convert the index object into a list.
```python
inds_array=weather_df.index.values
inds_list=weather_df.index.tolist()
```

**Exercise**

The Seaborn package includes some sample datasets.  We will look at the "iris" dataset.
```python
import seaborn as sn
iris=sn.load_dataset('iris')
```

Describe the dataset. Print the column names. Iterate through the indexes and print the corresponding value of the species name for indexes 0 to 30 inclusive.  Print the mean petal length.  Print the series data for row 90.  Make a new dataframe that contains only the petal length, petal width, and species. Summarize the new dataframe.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/iris.py" lang="python" >}}
{{< /spoiler >}}


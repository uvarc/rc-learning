---
date: "2020-11-17"
title: "Sorting and Grouping Data with Pandas"
weight: 138
---

## Sorting DataFrames

We can sort by column, single or multiple
```python
grade_book.sort_values(by="Grade",inplace=True) #ascending order
grade_book.sort_values(by="Grade",inplace=True,ascending=False) #descending

sorted_grades=grade_book.sort_values(by=["Grade","Year"])
```

To sort by row, we must specify the row index, and all values across the rows must be the same type.  We then sort with `axis=1`.  
{{< code-snippet >}}
inds=["Jim Dandy","Betty Boop","Minnie Moocher","Joe Friday","Teddy Salad"]
grades={"Test1":[85.4,91.7,73.2,82.3,98.5],
        "Test2":[88.1,89.8,75.9,84.0,96.3],
        "Test3":[83.7,92.4,70.1,88.2,96.8],
        "Test4":[84.1,87.2,69.3,81.7,93.9]}
{{< /code-snippet >}}

grade_record=pd.DataFrame(grades,columns=["Test1","Test2","Test3","Test4"],index=inds)
```python
>>>grade_record.sort_values(by='Jim Dandy',axis=1)
                Test3  Test4  Test1  Test2
Jim Dandy        83.7   84.1   85.4   88.1
Betty Boop       92.4   87.2   91.7   89.8
Minnie Moocher   70.1   69.3   73.2   75.9
Joe Friday       88.2   81.7   82.3   84.0
Teddy Salad      96.8   93.9   98.5   96.3
```

When True, the `inplace` parameter causes the sort to overwrite the dataframe. Otherwise a new dataframe will be returned.

## Grouping

Groups can be created with `groupby`
```
clouds=weather.groupby("Cloud Cover")
```
This creates a "GroupBy" object.  We can print the first item in each group
```
print(clouds.first())
             Tmin  Tmax
Cloud Cover            
2           -6.40  2.68
3           -5.87  8.79
5           -3.82  4.78
```
We can get the group members with `get_group`
```
print(clouds.get_group(3))
                     Tmin  Tmax  Cloud Cover
2000-01-01 00:00:00 -5.87  8.79            3
2000-01-03 00:00:00 -4.58  5.10            3
2000-01-05 00:00:00 -5.50  6.18            3
```
The mean of a group is easy to compute
```
print(clouds.Tmax.mean())
Cloud Cover
2    2.68
3    6.69
5    4.64
Name: Tmax, dtype: float64
```

We can extract values corresponding to a quantity.  For our grade-book example, we can create a new dataframe for the students who received an "A"
```
grade_book["Letter Grade"]=["B","A","C","B","A"]
top_students=grade_book[grade_book["Letter Grade"]=="A"]
top_students
```

## Pivot Tables

We can create _pivot tables_ to reorganize the data.  Continuing with the grade book, we can make the names into the index and organize by grades.
(For this example the result will have quite a bit of missing data, but a more complete grade book could have more scores.)

```
student_grades=grade_book.pivot(index="Name",columns="Letter Grade",values="Grade")
student_grades
```

**Exercise**

Continuing with the weather_data notebook, group the data by state and print the first value for each state.

If you look closely, you will note there are some errors.  There are "states" DE and VA, which are the abbreviations for those states.  Correct those errors and obtain a new grouping by state.  Get the mean temperature, minimum temperature, and maximum temperature per state, using the `round` method to round to 2 digits. Determine the Python type of each of these results.  Look up how to concatenate them into a new dataframe.  Print this dataframe.  Sort the dataframe by mean average temperature, in descending order.

{{< spoiler text="Example solution, zipped Jupyter notebook" >}}
[pandas_weather_ex3.zip](/courses/python-introduction/exercises/pandas_weather_ex3.zip)
{{< /spoiler >}}

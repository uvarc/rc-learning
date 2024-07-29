---
date: "2020-11-17"
title: "Filtering Data with Pandas"
weight: 137
---

We often need to extract data based on specified criteria, and to regroup and reorganize it.  Pandas provides a wealth of methods for these operations.

## Missing Data

We frequently must filter or account for missing data.
Pandas can easily handle this task.  Conventionally, we use the IEEE Not A Number (nan/NaN) indicator for missing data, since it is easy to check for its presence.  We can also use None; Pandas treats None and NAN as essentially the same thing.  In documentation, missing data is usually referenced as NA.

To check for missing data in our `student_grades` dataframe we can use `isna()` and `notna()`, both of which are Boolean functions so will return True or False. 

{{< code-snippet >}}
dates=["2000-01-01 00:00:00","2000-01-02 00:00:00",
       "2000-01-03 00:00:00","2000-01-04 00:00:00",
       "2000-01-05 00:00:00","2000-01-06 00:00:00"]
weather=pd.DataFrame({"Minimum Temp":[-5.87,np.nan,-4.58,-6.40,-5.50,-3.29],
                      "Maximum Temp":[8.79,np.nan,5.10,2.68,6.18,4.50],
                      "Cloud Cover":[3,5,3,2,3,5]},
                      index=dates)
{{< /code-snippet >}}

```python
>>>weather.isna()
                     Minimum Temp  Maximum Temp  Cloud Cover
2000-01-01 00:00:00         False         False        False
2000-01-02 00:00:00          True          True        False
2000-01-03 00:00:00         False         False        False
2000-01-04 00:00:00         False         False        False
2000-01-05 00:00:00         False         False        False
2000-01-06 00:00:00         False         False        False
```
Most built-in numerical methods omit rows with missing data.
```python
>>>weather["Minimum Temp"].mean()
-5.128
```

We can fill missing values with a quantity.
```python
>>>new_weather=weather.fillna(-999.)
>>>new_weather
                     Minimum Temp  Maximum Temp  Cloud Cover
2000-01-01 00:00:00         -5.87          8.79            3
2000-01-02 00:00:00       -999.00       -999.00            5
2000-01-03 00:00:00         -4.58          5.10            3
2000-01-04 00:00:00         -6.40          2.68            2
2000-01-05 00:00:00         -5.50          6.18            3
2000-01-06 00:00:00         -3.29          4.50            5
```

In many cases we just wish to remove rows with missing data.
```python
>>>corrected_weather=weather.dropna()
>>>corrected_weather
                     Minimum Temp  Maximum Temp  Cloud Cover
2000-01-01 00:00:00         -5.87          8.79            3
2000-01-03 00:00:00         -4.58          5.10            3
2000-01-04 00:00:00         -6.40          2.68            2
2000-01-05 00:00:00         -5.50          6.18            3
2000-01-06 00:00:00         -3.29          4.50            5
```

We can find the number of elements that are not NA in a dataframe with `count`
```python
>>>weather.count()
Minimum Temp    5
Maximum Temp    5
Cloud Cover     6
dtype: int64
```

## Searching DataFrames

### Conditionals

We can search for values in columns that satisfy specified conditions.
Conditionals in indexes are accepted much as for NumPy arrays.  The operators are `<`,`<=`,`>`,`>=`,`==`, and `!=` as for Numpy.
```
freezing_days=weather[weather.Tmax<=0.]
```

Conditionals may be compounded
```
cold_days=weather[(weather.Tmax<=6.) & (weather.Tmin>-5.)]
```

The ampersand `&` indicates `and`. Use the pipe symbol `|` for `or`.  The parentheses are generally required when creating a compound conditional.

Conditional searches return a new DataFrame.  Returning to the grade book example, we can select students by various criteria
```python
second_years = grade_book.loc[grade_book['Year'] == 2]
top_students=grade_book.loc[grade_book['Grade']>=90]
```
With no row specification, the `loc` is optional in the above code.

### Where

The `where` method returns a dataframe with NaN values for rows where the conditional is not satisfied, and the original values where it is true.

```python
>>>grade_record.where(grade_record["Test4"]>85)
                Test1  Test2  Test3  Test4
Jim Dandy         NaN    NaN    NaN    NaN
Betty Boop       91.7   89.8   92.4   87.2
Minnie Moocher    NaN    NaN    NaN    NaN
Joe Friday        NaN    NaN    NaN    NaN
Teddy Salad      98.5   96.3   96.8   93.9
```
If we want to remove the NaNs we can add `dropna`
```python
>>>grade_record.where(grade_record["Test4"]>85).dropna()
             Test1  Test2  Test3  Test4
Betty Boop    91.7   89.8   92.4   87.2
Teddy Salad   98.5   96.3   96.8   93.9
```

**Exercise**
Return to the weather_data.ipynb notebook from the previous section. Loop through the dataframe and use some things you have learned about strings to find the first line for the station location of Richmond, Virginia.  Get the corresponding station code.
Use this to extract the data for Richmond, Virginia into a new dataframe.
Print the overall mean temperature and the means of the minimum and maximum to two decimal places. 
(If using f strings, don't forget the rule about writing quotes inside the f-string.)
Print the number of days with a minimum below freezing. Hint: "False" is 0 and True is 1.

{{< spoiler text="Example solution, zipped Jupyter notebook" >}}
[pandas_weather_ex2.zip](/courses/python-introduction/exercises/pandas_weather_ex2.zip)
{{< /spoiler >}}

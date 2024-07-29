---
date: "2020-11-17"
title: "Project Set 4"
weight: 140
isSectionHeader: true
---

## Pandas 
These are in generally in order of difficulty, from easiest to most difficult. We have not covered all of these items in our notes. There are many good resources online, e.g. you can Google: "python pandas create new dataframe".

## Project 14
Create a new dataframe using the following table:
{{< table >}}
|  Coach    |    School |  Email  |  Career Wins  | National Championships |
|-----------|-----------|---------|---------------|------------------------|
|Tony Bennett| Virginia | tbennett@virginia.edu | 346  | 1  |
|Roy Williams | North Carolina | rwilliams@unc.edu | 871 | 3 |
|Mike Krzyzewski | Duke | coachk@duke.edu | 1132 | 5 |
|Tom Izzo | Michigan State | tizzo@msu.edu | 606 | 2 |
|Jim Boeheim | Syracuse | jboeheim@syracuse.edu | 944 | 1 |
{{< /table >}}

- Begin by importing pandas
- Create a new column that is the ratio of career wins to national championships.
- Rename your columns with a variable name. This is not required but often makes your code more readable. 
- Print a list of all columns.
- split a subset of your dataframe based on some condition. Do it at least twice. Once using .loc and once using .iloc.
- use the groupby() method to group your dataframe in some way that makes sense for your data

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_4/basketball_data.py" lang="python" >}}
{{< /spoiler >}}

## Project 15
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
{{< code-download file="/courses/python-introduction/solns/proj_set_4/pivot_table_example.py" lang="python" >}}
{{< /spoiler >}}

## Project 16

Download the file [pandas_demo.ipynb](/courses/python-introduction/scripts/pandas_demo_blank.zip) and the data files [eighthr.data](/data/eighthr.data) and [eightr.names](/data/eighthr.names). If you are using Windows, check that it does not append ".txt" to the data files.  You may need to open File Explorer, go to View, and check "File name extensions."  Open the notebook in JupyterLab or Jupyter.  Go through the exercises in each cell.

{{< spoiler text="Example solution, zipped Jupyter notebook" >}}
[pandas_demo.zip](/courses/python-introduction/solns/proj_set_4/pandas_demo.zip)
{{< /spoiler >}}

### Project 17
Download the file [cigarette-smoking-behaviour-2018-census.csv](/data/cigarette-smoking-behaviour-2018-census.csv), which is about cigarette smoking in New Zealand.
- Read the file into a pandas dataframe
- Make a bar plot in Matplotlib of types of cigarette smokers ('Regular Smoker', 'Ex-Smoker', etc.) and their count
- Because we have a total number of respondents, let's make a new column that is a ratio of # of each category / total number of respondents

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_4/cigarette_smoking_nz.py" lang="python" >}}
{{< /spoiler >}}
 
### Project 18
Redo Project 14 using Pandas.

Download [cville_2017_april.xlsx](/data/cville_2017_april.xlsx), which contains April 2017 weather data for Charlottesville, VA.
- Read the file into a pandas dataframe
- Make a line plot of average wind speed for each day
- Add main titles, and label axes to be more descriptive
- Play with the bottom axis (x-axis) to make sure all dates are visible
- Make a bar and line plot showing average wind speed (in bars) and max wind gust (as a line). Add legend to distinguish between the two.
- Make stacked bar chart of minimum and maximum temperatures for each day
- Make grouped bar chart of minimum and maximum temperatures for each day
- Plot the number of each weather 'condition'. Plot sunny days, partly cloudy days, and rain days. There are several ways to do this.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/solns/proj_set_4/cville_2017_apr_pandas.py" lang="python" >}}
{{< /spoiler >}}

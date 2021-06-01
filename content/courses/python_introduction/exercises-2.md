---
title: Exercises - Day 2
toc: true
type: docs
draft: false
weight: 120
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 120
---

# NumPy

## Project 1
Create a new Python script that performs the following operations: 

- Create a numpy array, x, of values from 1.0 to 50.0, with step sizes of 0.01.
- Create a numpy array, y, where y = sin(x) + cos(1.4*x) + 0.1*x.
- Determine and print the mean y value.
- Determine and print the x coordinates where y reaches a minimum and where y reaches a maximum in the given interval. **Hint:** Lookup the argmin and argmax functions.
- Optional: Allow the user to specify the x start, end and stride values.
- Optional: Plot y as a function of x using the matplotlib package (see below)

Remember you need to add the `import numpy` (or commonly used `import numpy as np`) statement in your script before you can use the numpy package.

<details>
<summary>See solution here:</summary>
<pre>
import numpy as np

x = np.arange(0.0,50.0,0.01)
y = np.sin(x) + np.cos(1.4*x) + 0.1*x
mean_y = y.mean()
min_index = np.argmin(y) #np.where(y==y.min())
max_index = np.argmax(y) #np.where(y==y.max())
print (f"mean y:{mean_y}")
print (f"max y:{y[min_index]} at x={x[min_index]}")
print (f"max y:{y[max_index]} at x={x[max_index]}")
</pre>
</details> 

## Project 2
Find the maximum of a 3d surface by “brute force” evaluation of x, y, z values. 

![](/courses/python_introduction/exercise-bruteforce.png)

- Generate a list of N random values for each of x and y over the above range. Use numpy arrays. For testing you can use N=8,000,000.
- Write a function that determines the x/y coordinates that define the maximum z value of the 3d surface. Once the code is working, vary N and compare how the x,y,z max values change.
- Optional: Plot the surface using the matplotlib package (see below).  Be careful, you may want to reduce the number of points to plots while experimenting with the best approach.
- Optional: Create a module for your z calculating function.  Import that module into a main script.  Use the `__name__ = "__main__":` code block in your calling script.

<details>
<summary>See solution here:</summary>
<pre>
import numpy as np

m1 = np.sqrt(2.)
m2 = np.sqrt(np.pi)
s1 = 3.1
s2 = 1.4
s1_2x_sqr = 2*(s1**2)
s2_2x_sqr = 2*(s2**2)
s1_x_s2_x_sqrt_2x_pi = s1 * s2 * (np.pi * 2) ** 0.5

def calc_z(x,y):
    """Calculates z value for x/y coordinates.""" 
    z1 = 0.1 * np.sin(x) * np.sin(x*y)
    alpha = ((x - m1) ** 2)/ s1_2x_sqr
    beta = ((y - m2) ** 2)/ s2_2x_sqr
    z2 = 1 / (np.exp(alpha + beta) * s1_x_s2_x_sqrt_2x_pi) 
    return z1 + z2

N = 8000000
x = np.random.uniform(-10.0*np.pi, 10.0*np.pi, N)
y = np.random.uniform(-10.0*np.pi, 10.0*np.pi, N)
z = calc_z(x,y)
max_idx = np.argmax(z)
print (f"Max z @ x={x[max_idx]},y={y[max_idx]}, z={z[max_idx]}")
</pre>
</details> 

# Pandas 
These are in generally in order of difficulty, from easiest to most difficult. We have not covered all of these items in our lectures, so feel free to ask questions or better yet, ask Google. Ex: ("python pandas create new dataframe")

## Project 3
Create new pandas, with a size of at least 5x3 (5 rows and 3 columns). Populate the dataframe with data of your choice.
- Begin by importing pandas
- Create a new column. If your data allows, the column could be a ratio based on existing columns (ex: pay_per_hour = df['price'] / df['time']). Or it could be something unrelated
- Rename your columns with a variable name. This is not required but often makes your code more readable. 
- Print a list of all columns. Then print the value counts of all your columns.
- split a subset of your dataframe based on some condition. Do it at least twice. Once using .loc and once using .iloc.
- use the groupby() method to group your dataframe in some way that makes sense for your data

<details>
<summary>See an example solution here (Remember, yours could be totally different)</summary>
<pre>
<code>

#import pandas
import pandas as pd 

#create new dataframe
basketball_coaches = pd.DataFrame({"Name": ['Tony Bennett', 'Roy Williams', 'Mike Krzyzewski', 'Tom Izzo', 'Jim Boeheim'],
                          "School": ['Virginia', 'North Carolina', 'Duke', 'Michigan State', 'Syracuse'],
                          "Email": ['tbennett@virginia.edu', 'rwilliams@unc.edu', 'coachk@duke.edu','tizzo@msu.edu', 'jboeheim@syracuse.edu'],
                          "Career Wins": [346, 871, 1132, 606, 944],
                          "National Championships": [1, 3, 5, 2, 1]})


print(basketball_coaches)


#rename columns with a variable name
name = basketball_coaches['Name']
school = basketball_coaches['School']
email = basketball_coaches['Email']
career_wins = basketball_coaches['Career Wins']
championships = basketball_coaches['National Championships']

#make new subset of data
wins_per_championship = career_wins/championships

#or if you want it to be a part of the dataframe
basketball_coaches['Wins Per Championship'] = basketball_coaches['Career Wins'] / basketball_coaches['National Championships']



#print a list of all the columns
print(basketball_coaches.columns.tolist())



#split a subset of the dataframe based on some condition. Several examples listed here
uva_coach = basketball_coaches.loc[basketball_coaches['School'] == 'Virginia']

multiple_championships = basketball_coaches.loc[basketball_coaches['National Championships'] > 1]

first_three_coaches = basketball_coaches.iloc[:3]


</code>
</pre>
</details>


## Project 4
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

- Join the two dataframe together using the [merge function](https://pandas.pydata.org/pandas-docs/stable/user_guide/merging.html)
- How many people come from each country? (Hint: Don't just count them. Which function allows you to see that easily?)
- Reshape the data and create a pivot table view of people by country using the [pivot_table function](https://www.geeksforgeeks.org/python-pandas-pivot_table/). Also include the name, age, and salary in the results

<details>
<summary>See the solution here</summary>
<pre>
<code>
import pandas as pd


df1 = pd.DataFrame({'ID #': [1,2,3,4,5,6,7,8,9,10],
                    'Name': ['John','Dave','Mary','Sarah','Mohammed','Rohan','Prisha','Vijay','Ananya','Raj'],
                    'Country': ['USA','USA','USA','UK','India','India','UK','India','UK','India']})

df2 = pd.DataFrame({'ID #': [1,2,3,4,5,6,7,8,9,10],
                    'Salary': [50000, 60000, 65000, 53000, 59000, 74000, 86000, 41000, 94000, 66000],
                    'Age': [24, 46, 51, 29, 33, 38, 70, 46, 49, 35]})

    
#merge
result = pd.merge(df1, df2, on="ID #")

#see counts of people by country
#print(result['Country'].value_counts())

#pivot table reshape
result2 = pd.pivot_table(result, index=['Country', 'Name', 'Age', 'Salary'])
</code>
</pre>
</details>

# Matplotlib
These examples will be used in conjuction with Pandas, as the two libraries are commonly used together. 

## Project 5
Download the file [cigarette-smoking-behaviour-2018-census.csv](/data/cigarette-smoking-behaviour-2018-census.csv), which is about cigarette smoking in New Zealand.
- Read the file into a pandas dataframe
- Make a bar plot in Matplotlib of types of cigarette smokers ('Regular Smoker', 'Ex-Smoker', etc.) and their count
- Because we have a total number of respondents, let's make a new column that is a ratio of # of each category / total number of respondents
 
## Project 6
Download [cville_2017_april.xlsx](/data/cville_2017_april.xlsx), which contains April 2017 weather data for Charlottesville, VA.
- Read the file into a pandas dataframe
- Make a line plot of average wind speed for each day
- Add main titles, and label axes to be more descriptive
- Play with the bottom axis (x axis) to make sure all dates are visible
- Make a bar and line plot showing average wind speed (in bars) and max wind gust (as a line). Add legend to distinguish between the two.
- Make stacked bar chart of minimum and maximum temperatures for each day
- Make grouped bar chart of minimum and maximum temperatures for each day
- Plot the number of each weather 'condition'. Plot sunny days, partly cloudy days, and rain days. There are several ways to do this.

## Project 7
Create a new pandas dataframe, with a size of at least 5x3 (5 rows and 3 columns). Populate the dataframe with data of your choice.
- Begin by importing pandas
- Create a new column. If your data allows, the column could be a ratio based on existing columns (ex: pay_per_hour = df['price'] / df['time']). Or it could be something unrelated
- Rename your columns with a variable name. This is not required but often makes your code more readable. 
- Print a list of all columns. Then print the value counts of all your columns.
- Split a subset of your dataframe based on some condition. Do it at least twice. Once using .loc and once using .iloc.
- Use the groupby() method to group your dataframe in some way that makes sense for your data

## Project 8
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

- Join the two dataframe together using the [merge function](https://pandas.pydata.org/pandas-docs/stable/user_guide/merging.html)
- How many people come from each country? (Hint: Don't just count them. Which function allows you to see that easily?)
- Reshape the data and create a pivot table view of people by country using the [pivot_table function](https://www.geeksforgeeks.org/python-pandas-pivot_table/). Also include the name, age, and salary in the results

# More Projects

Rather than re-inventing the wheel, there are tons of example projects available online. A great repository of pandas projects is [located here](https://www.geeksforgeeks.org/pandas-practice-excercises-questions-and-solutions/).


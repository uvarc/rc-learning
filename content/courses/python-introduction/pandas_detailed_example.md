---
title: Pandas Example
toc: true
type: docs
draft: false
weight: 139
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: Pandas
---

We are now ready to pull together what we have learned about Pandas and work through a detailed example.
It will be based on the American baseball player Mike Trout's statistics, through 2019.
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
{{< figure src="/courses/python-introduction/imgs/pandas_barchart.png" >}}

We can also use Matplotlib directly with our extracted variables.  The figure() method advances to a new plot.
```python
plt.figure()
plt.bar(year, hits)
```
{{< figure src="/courses/python-introduction/imgs/barplot1.png" >}}

Let's add some labels to make this more readable.

```python
plt.xlabel('Year')
plt.ylabel('# of Hits')
plt.suptitle('Mike Trout Hits per year')
plt.bar(year, hits)
```
{{< figure src="/courses/python-introduction/imgs/barplot2.png" >}}

Turn it into a horizontal barchart and change the color of the bars.

```python
plt.figure()
plt.xlabel('# of Hits')
plt.ylabel('Year')
plt.suptitle('Mike Trout Hits per year')
plt.barh(year, hits, color='red')
```
{{< figure src="/courses/python-introduction/imgs/barplot3.png" >}}

Make a line plot using the .plot() function instead of a barchart.

```python
plt.figure()
plt.xlabel('Year')
plt.ylabel('# of Hits')
plt.grid()
plt.plot(year, hits)
```

{{< figure src="/courses/python-introduction/imgs/lineplot1.png" >}}

We can superimpose a line and a bar plot. We will represent 'At Bats' by a red line and 'Hits' by blue bars. 

```python
plt.figure()
plt.xlabel('Year')
plt.ylabel('# of Hits')
plt.plot(year, at_bats, color='red')
plt.bar(year, hits)
```
{{< figure src="/courses/python-introduction/imgs/barline1.png" >}}

The y-label we used before is no longer appropriate, so let us add a legend.

```
plt.figure()
plt.xlabel('Year')
plt.plot(year, at_bats, color='red', label='At Bats')
plt.bar(year, hits, label='Hits')
plt.legend()        
```
{{< figure src="/courses/python-introduction/imgs/plotwithlegend1.png" >}}

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
{{< figure src="/courses/python-introduction/imgs/stackedbar1.png" >}}

To make a grouped bar chart, do the same as a stacked bar and move the position of one of the bars as shown below. Notice that for the second bar(), the first argument is 'year+.2'. This shifts the position on the x-axis .2 units to the right of the default starting point.
```python
plt.xlabel('Year')
plt.xticks(rotation=45)
plt.xticks(year)                #shows all years in label

plt.bar(year, hits, width=.2, label='Hits')
plt.bar(year+.2, home_runs, width=.2, label='Home Runs')
plt.legend()
```
{{< figure src="/courses/python-introduction/imgs/groupedbar1.png" >}}

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
{{< figure src="/courses/python-introduction/imgs/barwithlabels.png" >}}

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
{{< figure src="/courses/python-introduction/imgs/Formatter.png" >}}

Many plotting options can be applied directly to the Dataframe object, without the need to extract the variables. See the documentation for the Pandas [plot method](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.plot.html).

**Exercise**

Download the data [file](/data/MikeTroutData.csv) and work through the example.

## Resources

Many Pandas tutorials are available online. A good starting point is [here](https://pandas.pydata.org/pandas-docs/stable/getting_started/tutorials.html).

A repository of pandas practice projects is [located here](https://www.geeksforgeeks.org/pandas-practice-excercises-questions-and-solutions/)


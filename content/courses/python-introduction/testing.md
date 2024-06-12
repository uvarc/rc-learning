---
title: "Testing As You Go"
draft: false
toc: true
type: docs
weight: 255
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
       parent: Testing and Debugging
---

Suppose you have some data in a file and you wish to plot it. The file is 

{{< file-download file="/courses/python-introduction/data/VaGoldfinch.txt" text="VaGoldfinch.txt" >}}

Please download this file and follow along in Jupyter or another environment of your choice.

The data in this file is the number count of Eastern Goldfinches observed at the Christmas Bird Count.  No year information is attached, but we know from other sources that the time span is 1966-2015.

An easy way to read it is to use NumPy's `loadtxt`:

```python
import numpy as np
import matplotlib.pyplot as plt
input_file="VaGoldfinch.txt"
obs=np.loadtxt(input_file,delimiter=',')
```

We need to set up the independent variable so we create an array of the years:

```python
years=np.arange(1966,2015)
plt.plot(years,obs)
```

We run this and…it doesn't work. The size of the two arrays isn't the same. What did we 
forget? We forgot the Python rule about the upper bound of arange. We fix it with:

```python
years=np.arange(1966,2016)
```

Now it works. However, the data are noisy and we'd like to smooth it. We'll use a very simple algorithm; each point will be replaced by the average of it and its two neighbors. We add a loop to do this computation:

```python
smoothed_obs[:]=obs
for i in range(len(obs)):
	smoothed_obs[i]=(obs[i+1]+obs[i]+obs[i-1])/3.
```

Another failure; an index exception. We forgot that Python always starts indices at 0, so that the range gives us integers from 0 to 49; but when `i=49`, `i+1` is `50`, which is out of bounds for our array. We can correct this error easily:

```python
for i in range(len(obs)-1):
	smoothed_obs[i]=(obs[i+1]+obs[i]+obs[i-1])/3.
```

Now plot the smoothed data versus year.

This runs but the plot looks strange; the first value seems far too large. Now we need to start looking at the values so we can figure out what the program thinks it is supposed to be doing. This is an important aspect of debugging! Just as you may not see typos in an essay you write no matter how many times you proofread it, you may have a difficult time finding your own bugs just by reading your code. So we add a print statement

```python
for i in range(len(obs)-1):
	print(i,obs[i-1],obs[i],obs[i+1])
	smoothed_obs[i]=(obs[i+1]+obs[i]+obs[i-1])/3
```

If using Jupyter, remember to reinitialize _smoothed_obs_ before each change to the `for` loop.

This prints a lot of numbers, so we must scroll back to find the first few values. We look and find the first value is 614, far too large compared to what it should be. But why? What happened? Looking back to the last numbers in the printout, we realize that we used the last value in an average for the first. Why did that happen? Looking more closely at our loop, we realize that when `i=0`, `i-1` is `-1`. That is a legal index in Python, but it's not what we want, it's the shortcut for the last element in the list. Now we change our loop to

```python
for i in range(1,len(obs)-1):
	smoothed_obs[i]=(obs[i+1]+obs[i]+obs[i-1])/3.
```

This time the plot looks correct. Neither the zeroth element nor the last element is changed at all, which is acceptable for our purposes here. If we had wished to provide some special handling for elements `0` and `len(obs)`, we would have had to add those outside our loop.


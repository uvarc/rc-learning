---
title: "Debugging Existing Code"
draft: false
toc: true
type: docs
weight: 260
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
       parent: Testing and Debugging
---

Let's try a more challenging debugging example. Download the file [dow_buggy.py](/courses/python-introduction/scripts/dow_buggy.py) and the document [Day_of_the_Week.pdf](/courses/python-introduction/data/Day_of_the_Week.pdf). The document describes an algorithm to find the day of the week for any date in the Gregorian calendar between the years 1400 and 2599. The algorithm is straightforward but has many steps, and also requires that we remind ourselves how to obtain a remainder from a division. In Python we use the modulo operator, represented by `%`; thus `7%3` is 1. Our code hard-codes in days of the week rather than obtaining input from the user, so to change the date you will have to edit the source file. 
Our first date is 30 May 2016. The code computes this to be a Thursday when it should have been a Monday. We don't know where to start, so we'll start at the beginning.

Spyder, or another IDE such as VSCode, may be a better choice than Jupyter for this project, since its built-in debugger and Variable Explorer view can be quite useful.  If you wish to use JupyterLab, install the `ipykernel` or `xeus-python` kernel, according to the [documentation](https://jupyterlab.readthedocs.io/en/stable/user/debugger.html).  If using ipykernel, make sure it is at least Version 6.0. 

Debuggers work via _breakpoints_.  We set a breakpoint on specific lines.
Most debuggers use instructions like _Next_, _Step In_, _Step Return_ or _Step_Out_, and _Continue_.  Continue means move on to the next breakpoint.  Next tells the debugger to execute the next line, but not to go line by line through a function.  Step or Step In enters the function and waits for a Next to be issued. Step Out/Return executes the rest of the function and returns.

In Spyder, click the blue "Play" icon to start debugging.  Set a breakpoint by clicking on the line number.  Note that some Spyder versions on some platforms, such as 5.1.5 on Linux, may not advance properly.  Upgrading (or, if necessary, downgrading) should solve that problem.  Stop debugging with the blue square icon.  The blue double arrow is "Continue," the arc over a dot is "Next," the down arrow over a dot is "Step In," and the up arrow over a dot is "Step Return."

{{< figure src="/courses/python-introduction/imgs/Spyder_debug.png" >}}

In a debugger-enabled JupyterLab, first paste the function into a cell, and the main body into another cell.  At the right of the top ribbon, click on the "bug" icon next to the kernel name so that it turns orange. Also expand the "bug" icon on the right-hand sidebar to open views of the equivalent of the Variable Explorer.  Then breakpoints may be set by clicking on a line.  In JupyterLab with an appropriate kernel, the functions to move through the code are the icons above the "Callstack" pane on the right-hand "debug" sidebar.  Hovering over the icons shows which is which. They are very similar to the corresponding Spyder icons.

{{< figure src="/courses/python-introduction/imgs/Jupyter_debug.png" >}}

We will set a breakpoint at the line
```python
year=2016
```

Run to the breakpoint, then step into the function.  The variables passed into `DoW` will appear in the Variable Explorer or Variables pane.  Press Next and watch the value of the variables after each line.  This should show that the variable `D` has the correct value, but `M` has the value `4`. We are off by one on the month. That's because we number the months 1 to 12, but the corresponding indices of our lookup table run 0 to 11, so we must make a change to the indexing for the month. We change

```python
M = months[month]
```
to
```python
M = months[month-1]
```

Rerun with that correction. Now M is correct. We remove the breakpoint (or Step Out and Continue) and rerun the entire program. This time we get the right answer.  

Are we then done? We have only tested the code for one date. We must test more 
thoroughly, and we must especially test “corner cases,” situations where some unusual conditions may apply. For this code, we should test at least one date in each month, and we should test dates in years that are and are not leap years, taking special note of the rule for leap years in centuries. Century years must be divisible by 400, not 4, to be a leap year, so this is an example of a corner case.

Add the following code at the bottom of your script:

```python
print("\n\nTesting first of each month")
day = 1
month = 1
while month < 13:
    print("For {:} 1 the day of the week is {:}".format(month_names[month-1],DoW(day,month,year)))
	month += 1
```

What do you get when you run it? Now we have to check with an independent calendar whether our results are correct. We are still in 2016 so we can use a calendar of that year.  In JupyterLab, disable debugging to see the output.

We find that our code says that January 1, 2016 was a Saturday, when it was actually a Friday. We are also off by one day for February 1, but March, April, and May are correct. The year 2016 was a leap year.  When we think about it, we realize that only days after February 29, 2016 will be affected by the leap year. We forgot to implement that part of the algorithm. Add the following code to `DoW` to fix this bug, right before it determines the value of C:

```python
leap_year = (century_leap_year) or (year%4==0 and year%100 > 0)
if leap_year and month<3:
    L -= 1
```

Now it looks correct for the entire year…or does it? September 1, 2016 is reported to be a Friday rather than a Thursday. Another off-by-one error, it appears. Since it's dependent on the month, we need to look at the month computation. We double-check the values in our lookup table ``months`` and discover a typo; we have a 6 rather than a 5, in the ninth position. Correcting that makes our days correct.

We need to test other days, however. Try at least the following:

```no-highlight
February 14, 2000
February 14, 1900
July 4, 1971
July 4, 1776
```

It's easy to find day of the week calculators online, but test against two of them to make sure all the methods agree. You can try your own birthdate as well.

{{< spoiler text="Corrected dow.py" >}}
{{< code-download file="/courses/python-introduction/exercises/dow.py" lang="python" >}}
{{< /spoiler >}}

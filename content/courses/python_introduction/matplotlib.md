---
title: Matplotlib
toc: true
type: book
draft: false
weight: 120
---

Matplotlib is a Python package that can be used to produce high-quality plots similar to those of MATLAB<sup>TM</sup>.  Its homepage and documentation can be found at [matplotlib.org](https://matplotlib.org).  A full complement of plot types is available, including

* line plots
* scatter plots
* histograms
* bar charts 
* pie charts 
* contour plots

Most of our sample scripts are taken directly from the Matplotlib [gallery](https://matplotlib.org/stable/gallery/index.html).

## One-Dimensional Plots

A simple example:

```python
import numpy as np
import matplotlib.pyplot as plt
x=np.linspace(-4.,4.,401)
y=1./(np.pi*(1.+x**2))
plt.plot(x,y)
plt.show()
```
This results in
![SimplePlot.png](/courses/python_introduction/imgs/SimplePlot.png)

Let us write a more sophisticated example.  This is a scatter plot with points randomly placed according to a normal distribution.

{{< code-download file="/courses/python_introduction/scripts/scatter_plot.py" lang="python" >}}

![ScatterPlot.png](/courses/python_introduction/imgs/ScatterPlot.png)

We can place more sophisticated labeling or multiple plots on a graph with `subplot`

{{< code-download file="/courses/python_introduction/scripts/subplots.py" lang="python" >}}

![Subplot.png](/courses/python_introduction/imgs/Subplot.png)

Many other options are available for annotations, legends, and so forth.

More advanced plots are provided.  The following demonstrates streamlines for vector fields, such as fluid flows.

{{< code-download file="/courses/python_introduction/scripts/streamlines.py" lang="python" >}}

![Streamplots.png](/courses/python_introduction/imgs/Streamplots.png)

Matplotlib can also make histograms, pie charts, and so forth.  These are commonly used with Pandas, and Pandas can access them directly, as we will see.

## Higher-Dimensional Plots

For higher-dimensional plots we can use `contour`, `contourf`, `surface`, and others.

Contour plot example:
{{< code-download file="/courses/python_introduction/scripts/contour.py" lang="python" >}}

The `meshgrid` function takes two rank-1 arrays and returns two rank-2 arrays, with each point labeled with both `x` and `y` values.  Notice how NumPy array operations are used to compute the function values from the meshgrid arrays.

Surface plots require the `mplot3d` package and some additional commands to set views and sometimes lighting.  

{{< code-download file="/courses/python_introduction/scripts/surface.py" lang="python" >}}

Recent versions of Matplotlib can apply _style sheets_ to change the overall appearance of plots.  For example, NumPy has modified its default style, but the older one (shown in some of our illustrations) is available as "classic."  Matplotlib can also be styled to imitate the R package `ggplot`.  See the [gallery](https://matplotlib.org/gallery/style_sheets/style_sheets_reference.html#sphx-glr-gallery-style-sheets-style-sheets-reference-py)
for the possibilities.

<details>
<summary>Exercise 26</summary>

1. Type into your choice of Spyder's interpreter pane or a JupyterLab cell the example plotting codes we have seen so far.  These were all taken from the Matplotlib gallery.

2. In the contour plot example, change `contour` to `contourf` and observe the difference.

</details>

## Seaborn

[Seaborn](https://seaborn.pydata.org/index.html) is a package built upon Matplotlib that is targeted to statistical graphics.  Seaborn can be used alone if its defaults are satisfactory, or plots can be enhanced with direct calls to Matplotlib functions.

Seaborn 0.9 or later is needed for the "relationship" plot example below. This example uses a built-in demo dataset.

{{< code-download file="/courses/python_introduction/scripts/seaborn_demo.py" lang="python" >}}

![SeabornDemo1.png](/courses/python_introduction/imgs/SeabornDemo1.png)

Many other statistical plots are available including boxplots, violin plots, distribution plots, and so forth.  The next example is a heatmap.

{{< code-download file="/courses/python_introduction/scripts/seaborn_demo2.py" lang="python" >}}

![SeabornDemo2.png](/courses/python_introduction/imgs/SeabornDemo2.png)

The call to `sns.set()` imposes the default Seaborn theme to all Matplotlib plots as well as those using Seaborn.  Seaborn provides a number of methods to modify the appearance of its plots as well as Matplotlib plots created while the settings are in scope.  For many examples see their [tutorial](https://seaborn.pydata.org/tutorial/aesthetics.html#aesthetics-tutorial) on styling plots.

Seaborn's documentation can be found [here](https://seaborn.pydata.org/)


---
title: "Exercise 1"
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 295

menu:
  hpc-intro:
    parent: Interactive Apps with Open OnDemand
---

Start a JupyterLab interactive session.  Select the Python 3 kernel.  If you are familiar with Python, you may write any code you wish.  If you do not know Python, click on a cell and type the following

```python
import numpy as np
import matplotlib.pyplot as plt
```
Hit [Shift][Enter] (hold both keys at once) to run the cell, or from the Run menu choose Run Selected Cell.

In the next cell type
```python
x=np.linspace(-1.*np.pi,np.pi,100)
y=np.sin(x)
plt.plot(x,y)
```

Run this cell.  

Close the tab, return to My Interactive Sessions in the OOD Dashboard, and delete the interactive job.

 

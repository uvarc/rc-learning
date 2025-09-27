---
title: More Slurm Options
date: "2025-02-23T00:00:00"
type: docs 
weight: 3250
menu: 
    llms-hpc:
      parent: Slurm Scripts
---


* To request a specific amount of memory per node:
  * Ex: ``` --mem=64G```

Units are given with a suffix (K, M, G, or T).  If no unit is given, megabytes is assumed.

Other options are available at [https://slurm.schedmd.com/sbatch.html](https://slurm.schedmd.com/sbatch.html)

For more information, see the RC tutorial [Using SLURM from a Terminal](https://learning.rc.virginia.edu/tutorials/slurm-from-cli/).

Tip: if you have a Jupyter notebook file (.ipynb) that you would like to run using a Slurm script, first convert it to a .py file using the following command (make sure you are in the directory that contains the file): 
  
  ```jupyter nbconvert --to python file_name.ipynb```



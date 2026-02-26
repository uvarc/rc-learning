---
title: Code Profiling
date: 2024-06-28T01:51:43Z
type: docs 
weight: 750
toc: true
menu: 
    deep-learning-hpc:
        parent: Resource Allocation and Helpful Tools
---


A code profiler provides information about how much time each line of a program takes to run.
* This information is helpful for effectively speeding up code execution time.
* There are various profilers available for Python.  A recommendation is line_profiler ([https://github.com/pyutils/line_profiler](https://github.com/pyutils/line_profiler)).

## Install _line_profiler_

__Use the PyTorch container:__
```bash
module load apptainer pytorch 
apptainer exec $CONTAINERDIR/pytorch-2.0.1.sif pip install line_profiler
```     
__Use the TensorFlow container:__
```bash
module load apptainer tensorflow 
apptainer exec $CONTAINERDIR/tensorflow-2.13.0.sif pip install line_profiler
```       
__Outside of a container:__
```bash
pip install --user line_profiler
```  

## Run _line_profiler_

1. In the code file, include the import statement as below and use `@profile` to decorate the functions you would like to profile:
```python
from line_profiler import profile
...
@profile
def fcn_to_profile(arg1, arg2, ...):
...
```

2. Run line_profiler, use the `--nv` flag for GPU use

* Pytorch:
```bash
LINE_PROFILE=1 apptainer run --nv $CONTAINERDIR/pytorch-2.0.1.sif file_name.py
```

* Tensorflow:
```bash
LINE_PROFILE=1 apptainer run --nv $CONTAINERDIR/tensorflow-2.13.0.sif file_name.py
```    

3. Profiler results will be printed out into two text files (the files are the same): profile_output.txt and profile_output_[TIMESTAMP].txt.
        
Visit the Line Profiler [documentation](https://kernprof.readthedocs.io/en/latest/#line-profiler-basic-usage) for more information.

Notes:
- Running the command **without** `LINE_PROFILE=1` will just run `file_name.py` but **not** profile it.
- `line_profiler` has a very slight overhead (for code run on GPU).  Some notive more of a slow down on strictly CPU code (~40 more seconds for code that should run in ~160 seconds).


## Check Your Knowledge

1. Install _line_profiler_.
2. Use _line_profiler_ to profile the function "train" in example1.py.
     * While the code is running, open a terminal and watch the nvidia-smi output.
     * How was the GPU utilization?
     * Which line takes the longest to run?  Does this surprise you?
     * What would you suggest we do to increase the efficiency of the code?
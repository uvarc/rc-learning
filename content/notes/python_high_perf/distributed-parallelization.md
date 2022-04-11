---
title: "Distributed Parallel Programming with MPI"
type: docs
toc: true
weight: 5
menu: 
    hp-python:
        parent: High-Performance Python
        weight: 5
---

Nearly all recent computers, including personal laptops, are multicore systems.  The central-processing units (CPUs) of these machines are divided into multiple processor cores.  These cores share the main memory (RAM) of the computer and may share at least some of the faster memory (cache).  This type of system is called a **shared-memory processing** or **symmetric multiprocessing** (SMP) computer.  

{{< figure src="/notes/python_high_perf/SMP.png" caption="Schematic of an SMP system" >}}

A computing cluster consists of a group of computers connected by a network.  High-performance clusters nearly always have a network that is faster than the Ethernet used by consumer devices.  Many use a network called InfiniBand.  A cluster is thus a **distributed-memory processor** (DMP).  Each computer, usually called a **node**, is independent of the others and can exchange data only through the network.

Multiprocessing works only on a single computer with multiple computing cores (SMP). If you have access to a computing cluster you can use distributed parallelization to run your program on multiple computers (DMP) as well as multiple cores per computer.  This requires a communications library.  

{{< figure src="/notes/python_high_perf/DMP.png" caption="Schematic of a DMP system" >}}

Before considering parallelizing your program, it is highly desirable to spend some time optimizing the _serial_ version.  Particularly if you can utilize NumPy and Pandas effectively, you may not need to try to parallelize the code.  If you still need to do so, a well-optimized, clearly-written script will make the work much easier.

## Dask

[Dask](https://docs.dask.org/en/stable/) is a framework for distributed applications.  It works with NumPy, Pandas, and Scikit-Learn, as well as some less common packages that have been customized to utilize it.  It is primarily used to distribute large datasets.  

Dask breaks the work into _tasks_ and distributes those tasks.  It can use multicore systems easily. With some care, it can also use multiple nodes on a cluster if that is available.  A task may be reading a file, or doing some work on a portion of a large dataframe, or processing JSON files.  To accomplish this, Dask constructs a _task graph_.  This lays out the order in which tasks must occur.  Independent tasks can be run in parallel, which can considerably speed up the time to solution.  Results are collected as needed.

Our examples are taken from the Dask [documentation](https://examples.dask.org/index.html).  We cover only the basics here; a more complete [tutorial](https://github.com/dask/dask-tutorial) can be downloaded as Jupyter notebooks.

Dask is best utilized for problems for which the dataset does not fit in memory.  Some overhead is associated with it, so if your optimized NumPy or Pandas code can handle the problem, it is better to use those standard packages.

### Dask Arrays

Dask arrays are much like NumPy arrays, but are decomposed into subunits that Dask generally calls _chunks_.  In the language of parallel computing, we call this _data decomposition_.

```python
>>>import dask.array as da
>>>x = da.random.random((10000, 10000), chunks=(1000, 1000))
```
Here we have created a two-dimensional array and broken it into 100 1000x1000 subarrays.  The full-sized array is not actually created so we do not need the memory required for it.  This is a general rule; avoid creating large global arrays.  Large, memory-hungry arrays can be slow to process or, if they do not fit in any available memory, can make the problem impossible to solve on most computers.

Most, but not all, NumPy functions and methods are available for Dask arrays.
```python
>>>x.mean()
```
This returns
```no-highlight
dask.array<mean_agg-aggregate, shape=(), dtype=float64, chunksize=(), chunktype=numpy.ndarray>
```
This is because the Dask functions set up a computation but most do not execute it until told to do so.  To get our answer we must invoke the `compute` method.
Dask is said to be _lazy_.
```python
>>>x.mean().compute()
0.5000237776672359
```
Certain operations that require the full result, such as plotting, will automatically invoke `compute` but it can always be done explicitly.

### Dask Dataframes

Dask also has an infrastructure built atop Pandas. As for NumPy, many but not all Pandas functions and methods are implemented. 
```python
>>>import dask
>>>import dask.dataframe as dd
>>>df = dask.datasets.timeseries()
>>>df
```
This is also lazy:
```no-hightlight
Dask DataFrame Structure:
                   id    name        x        y
npartitions=30
2000-01-01      int64  object  float64  float64
2000-01-02        ...     ...      ...      ...
...               ...     ...      ...      ...
2000-01-30        ...     ...      ...      ...
2000-01-31        ...     ...      ...      ...
Dask Name: make-timeseries, 30 tasks
```
Only the structure of the dataframe has been established.

We can invoke Pandas functions, but as for arrays we must finalize the results.
```
>>>df2 = df[df.y > 0]
>>>df3 = df2.groupby('name').x.std()
>>>df3.compute()
```

**Exercise**
In a Jupyter notebook or Spyder interpreter, run the Dataframe examples above. Now run
```python
>>>import matplotlib.pyplot as plt
```
or in Jupyter
```python
%matplotlib inline
```
Then
```python
>>>df[['x', 'y']].resample('1h').mean().head()
>>>df[['x', 'y']].resample('24h').mean().compute().plot()
```
Run
```python
>>>plt.show()
```
if necessary.

The Dask version of read_csv can read multiple files using wildcards or globs.
```python
>>>df = dd.read_csv('myfiles.*.csv')
```
When creating a Dataframe, Dask does attempt to assign a type to each column.  If it is reading multiple files it will use the first one to make this determination, which can sometimes result in errors.  We can use `dtype` to tell it what types to use.
```python
>>>df = dd.read_csv('myfiles.*.csv',dtype={'Value':'float64'})
```

### Dask Bags

Dask Bags implement collective operations like mapping, filtering, and aggregation on data that is less structured than an array or a dataframe.

This example requires the mimesis package.
```python
>>>import dask
>>>import json
>>>import os

>>>os.makedirs('data', exist_ok=True)            # Create data/ directory

>>>b = dask.datasets.make_people()               # Make records of people
>>>b.map(json.dumps).to_textfiles('data/*.json') # Encode as JSON, write to disk
import dask.bag as db
>>>b = db.read_text('data/*.json').map(json.loads)
>>>b
>>>b.take(2)  # shows the first two entries
>>>c=b.filter(lambda record: record['age'] > 30)
>>>c.take(2)
>>>b.count().compute()
```

**Exercise**
Run the above example.  Why does the expression `b` not show any values?

### Xarray

[Xarray](https://xarray.pydata.org/en/stable/) is a project that extends Pandas dataframes to more than two dimensions. It is able to use Dask invisibly to the user, with some additional keywords.
```python
>>>import xarray as xr
>>>ds = xr.tutorial.open_dataset('air_temperature',
                              chunks={'lat': 25, 'lon': 25, 'time': -1})
>>>da = ds['air']
>>>da
```
The result shows the chunks layout but not the actual data.  Without the `chunks` argument when the dataset was initiated, we would have an ordinary Xarray dataset.

Xarray is particularly well suited to geophysical data in the form of NetCDF files, but can also handle HDF5 and text data.

### Dask and Machine Learning

Machine learning is beyond our scope here, but we will make a few comments.  Dask can integrate with scikit-learn in the form of `Dask-ML`.  
```python
>>>import numpy as np
>>>import dask.array as da
>>>from sklearn.datasets import make_classification
>>>X_train, y_train = make_classification(
       n_features=2, n_redundant=0, n_informative=2,
       random_state=1, n_clusters_per_class=1, n_samples=1000)
>>>X_train[:5]
>>>N = 100
>>>X_large = da.concatenate([da.from_array(X_train, chunks=X_train.shape)
                             for _ in range(N)])
>>>y_large = da.concatenate([da.from_array(y_train, chunks=y_train.shape)
                             for _ in range(N)])
>>>X_large
>>>from sklearn.linear_model import LogisticRegressionCV
>>>from dask_ml.wrappers import ParallelPostFit
>>>clf = ParallelPostFit(LogisticRegressionCV(cv=3), scoring="r2")
>>>clf.fit(X_train, y_train)
>>>y_pred = clf.predict(X_large)
>>>y_pred
>>>clf.score(X_large, y_large)
```
Dask can also be used with Pytorch.  See the [documentation](https://examples.dask.org/machine-learning/torch-prediction.html) for an example.

## MPI

The most widely used general-purpose communications library is MPI, the Message Passing Interface.  

MPI works on multicore systems as well as multinode, but the programming model is still different from threads.

In MPI each process has an ID called its _rank_.  Ranks are numbered from 0 to _n-1_, for _n_ processes.  Each process runs completely independently.  No process shares memory with any other process whether running on the same node or not.  All communications occur over the network. 
To use MPI the programmer must manage the distribution of the data to different processes and the communication among the processes.  

MPI messages are identified by an "envelope" of metadata. This consists of the _destination_, the _source_ (the "return address"), a _communicator_ (a group of processes that will be exchanging information), and optionally a _tag_.  A communicator consisting of all processes, called COMM_WORLD, is set up when the MPI program is initiated.

The process with rank 0 is usually called the **root process**.  Since the mimumum number of processes is 1, the root process is the only one that is guaranteed to be present.  For this reason it is usually used to manage various bookkeeping tasks as well as input/output.

## The mpi4py Package

The most popular direct way to use MPI with Python is the `mpi4py` package.  It is not included in the base Anaconda distribution.  To install it into your environment in an HPC cluster such as Rivanna, load the appropriate modules for compiler and an MPI distribution.  It is important that a command `mpicc` provided by your HPC site be first in your path, since that should have been set up to communicate properly with your resource manager (SLURM etc.)
```
module load gcc/9.2.0
module load openmpi
module load anaconda
pip install --user mpi4py
```
You must use `pip` rather than `conda` because conda will install precompiled binaries and you must compile `mpi4py` explicitly.

MPI consists of dozens of functions, though most programmers need only a fraction of the total.  The mpi4py package has implemented most of them using a "Pythonic" syntax, rather than the more C-like syntax used by other languages.  One peculiarity of mpi4py is that only particular types may be communicated; in particular, only NumPy NDArrays or pickled objects are supported.  To simplify our discussion, we will ignore the versions for pickled objects.  The requirement that NumPy arrays be sent means that even single values must be represented as one-element NumPy arrays.

MPI requires more advanced programming skills so we will just show an example here.  Our Monte Carlo pi program is well suited to MPI so we can use that.

{{% code-download file="/notes/python_high_perf/codes/MonteCarloPiMPI.py" lang="python" %}}

The first invocation of MPI is the call to Get_rank.  This returns the rank of the process that calls it.  Remember that each MPI process runs as a separate executable; the only way their behaviors can be controlled individually is through the rank.  This call also initializes MPI; a separate MPI.Init is not required. The next line allows us to find out how many processes are in COMM_WORLD.  The number of processes for MPI programs is always set outside the program, and should never be hardcoded into the source code.

Next we divide up the number of "throws" into roughly equal chunks, just as we did in the corresponding Multiprocessing example.  The same list is generated, but we use _myrank_ to select the element of the list that each rank will use.

After this each process invokes the `pi` routine using `myNumPoints` for its rank.  We need to collect all the estimates and average them.  The _reduction_ collects the results from each rank (in rank order) and applies the "op" (operation).  A reduction must be a binary operation that returns another object of the same type; the operation is applied along the sequence.  In this case we want the sum so we add the result from rank 0 to that from rank 1, then we take that sum and add the result from rank 2, and so forth.  Dividing by the number of processes provides the average.  

The Reduce function returns each result to the process regarded as the root, which would normally be 0 as it is in this case, and root carries out the operation.  Thus _only_ the root process knows the final result.  We then select root to print out the value.

This example reruns the problem without distributing the throws in order to obtain a serial time for comparison purposes.  Of course, a real application would not do that.


### Running the MPI Python Program on Rivanna

In order to launch multiple tasks (or processes) of our program, we run this program through the MPI executor.  On our HPC cluster this is srun.

```
srun python MonteCarloPiMPI.py 1000000000
```

**Note you cannot launch the MPI program with `srun` on the Rivanna login nodes.**  In order to execute our program on designated compute node(s), we need to write a simple bash script that defines the compute resources we need.  We call this our job script.  For our example, the job script `pimpi.sh` looks like this:

{{% code-download file="/notes/python_high_perf/codes/pimpi.sh" lang="bash" %}}

The `#SBATCH` directives define the compute resources (`-N`, `--ntasks-per-node`, `-p`), compute wall time (`-t`), and the allocation account (`--account`) to be used. `-N 1` specifies that all MPI tasks should run on a single node.  We are limiting the number of nodes for this workshop so that everyone gets a chance to run their code on the shared resources.  

**Submitting the job:**

Open a terminal window and execute this command:
```
sbatch pimpi.sh
``` 

<br>
**Checking the Job Status:**

Check the job status with the `squeue -u` or `sacct` commands as described in the [Multiprocessing](#multiprocessing) section. 

<br>
**Checking the Output File:**

Open the `slurm-XXXXXX.out` files in a text editor and record the total run time for the job.

<br>
**Exercises**

Rerun the MPI job using 1 or 4 cpu cores by changing the `--ntasks-per-node` option.

### Scaling
On the cluster, the timings are very similar to Multiprocessing on the workstation.

| CPU Cores | Run Time |
| --- | --- |
| 1 (serial) | 400 sec|
| 4 | 102 sec |
| 8 | 60 sec |
| 16 | 31 sec |

## Dask-MPI

Dask can use `mpi4py` on a high-performance cluster.  First install mpi4py according to the instructions in the previous section, then `pip install --user dask-mpi`. 

### Schedulers

We have not discussed Dask [_schedulers_](https://docs.dask.org/en/latest/scheduling.html) previously. The scheduler is a process that managers the workers that carry out the tasks.
We have been implicitly using the _single-machine_ scheduler, which is the default. Within the single-machine scheduler are two options, _threaded_ and _processes_.  The threaded single-machine scheduler is the default for Dask Arrays, Dask Dataframes, and another Dask capability called [Delayed](https://docs.dask.org/en/latest/delayed.html).  However, as we discussed with [Multiprocessing](/notes/python_high_perf/multiprocessing), the GIL (Global Interpreter Lock) inhibits threading in general.  Most of NumPy and Pandas release the GIL so threading works well with them.  If you cannot use NumPy and Pandas then the processes scheduler is preferred.  It is much like Multiprocessing.

To use Dask-MPI we must introduce the Dask `distributed` scheduler. The `distributed` scheduler may be preferable to `processes` even on a single machine, and it is required for use across multiple nodes. 

### Running Dask-MPI

We will discuss here only the batch interface for Dask MPI.  Dask-MPI provides an `initialize` function that initializes MPI and sets up communications. You must then start a Client cluster to connect the workers.  Dask-MPI uses rank 0 for the manager process and rank 1 to mediate between manager and workers, so you must request _at least_ 3 processes.  Close the cluster at the end of your script to avoid error messages about terminated processes.  

**Example**
Convert the "timeseries" example to Dask MPI.

{{< code-download file="/notes/python_high_perf/codes/dask_df_mpi.py" lang="python" >}}

Run this simple example with

{{< code-download file="/notes/python_high_perf/codes/run-dask_mpi.py" lang="bash" >}}

The OMPI_MCA environment variable suppresses a warning message that is seldom relevant.

Using Dask-MPI is not difficult, especially in batch mode, but users interested in trying it should be sure to first understand the distributed scheduler, then study the online examples carefully.  Dask-MPI does not require explicit calls to MPI but with the convenience comes some loss of control; the algorithm must be suited to the distributed scheduler.  More information may be found in the [documentation](https://mpi.dask.org/en/latest/batch.html).

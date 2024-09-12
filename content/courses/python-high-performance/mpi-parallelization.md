---
title: "Parallel Programming with MPI"
type: docs
toc: true
weight: 55
date: "2020-11-17T00:00:00"
menu: 
    hp-python:
        parent: High-Performance Python
---

The most widely used general-purpose communications library for distributed parallelization is MPI, the Message Passing Interface.  

MPI works on multicore systems as well as multi-node, but the programming model is still different from threads.  MPI starts a specified number of _independent_ copies of the program, which then communicate with one another through the MPI library.

In MPI each process has an ID called its _rank_.  Ranks are numbered from 0 to _n-1_, for _n_ processes. No process shares memory with any other process whether running on the same node or not.  All communications occur over the network.  To use MPI the programmer must manage the distribution of the data to different processes and the communication among the processes. Each process runs the same script or program, so any difference in behavior by rank must be programmed.  

MPI messages are identified by an "envelope" of metadata. This consists of the _destination_, the _source_ (the "return address"), a _communicator_ (a group of processes that will be exchanging information), and optionally a _tag_.  A communicator consisting of all processes, called COMM_WORLD, is set up when the MPI program is initiated.

The process with rank 0 is usually called the **root process**.  Since the minimum number of processes is 1, the root process is the only one that is guaranteed to be present.  For this reason it is usually used to manage various bookkeeping tasks as well as input/output.

## The mpi4py Package

The most popular direct way to use MPI with Python is the `mpi4py` package.  It is not included in the base Anaconda distribution.  If you are running on your own multicore system you can install it directly with `conda` as usual. If you are running on an HPC cluster that uses a resource manager such as Slurm, the process is more complicated.

When installing it into your environment in an HPC cluster, you should not use `conda install` because conda will install precompiled binaries and you must use a version of MPI that will correctly communicate with the system network and the resource manager. We suggest creating a conda environment for your MPI programs.
```bash
conda create -n "mpienv" python=3.11
```
In recent versions of Anaconda, it is best to install mpi4py from the `conda-forge` channel following their instructions [here](https://conda-forge.org/docs/user/tipsandtricks/#using-external-message-passing-interface-mpi-libraries). 
This will install dummies into your environment that will be replaced by the external library when the package is imported.  Do not try to install both MPICH and OpenMPI; use the one most appropriate to your system. In our example, we will install mpi4py with OpenMPI.

First load the closest gcc to the current version of Anaconda. In our current example, this is gcc 11.4.0. Then check for available versions of OpenMPI (please use OpenMPI on UVA HPC systems) with
```bash
module spider openmpi
```
Now view the available external versions of OpenMPI from conda-forge
```bash
conda search -f openmpi -c conda-forge
```
For this example, we will use OpenMPI 4.1.4.
```bash
conda install -c conda-forge "openmpi=4.1.4=external_*"
```
Once this has completed, you can install mpi4py
```
conda install -c conda-forge mpi4py
```

MPI consists of dozens of functions, though most programmers need only a fraction of the total.  The mpi4py package has implemented most of them using a "Pythonic" syntax, rather than the more C-like syntax used by other languages.  One peculiarity of mpi4py is that only particular types may be communicated; in particular, only NumPy NDArrays or pickled objects are supported.  To simplify our discussion, we will ignore the versions for pickled objects.  The requirement that NumPy arrays be sent means that even single values must be represented as one-element NumPy arrays.

MPI requires more advanced programming skills so we will just show an example here.  Our Monte Carlo pi program is well suited to MPI so we can use that. For much more information about programming with MPI in Python, as well as C++ and Fortran, please see our [short course](/courses/parallel-computing-introduction).

{{% code-download file="/courses/python-high-performance/codes/MonteCarloPiMPI.py" lang="python" %}}

The first invocation of MPI is the call to Get_rank.  This returns the rank of the process that calls it.  Remember that each MPI process runs as a separate executable; the only way their behaviors can be controlled individually is through the rank.  This call also initializes MPI; a separate MPI.Init is not required. The next line allows us to find out how many processes are in COMM_WORLD.  The number of processes for MPI programs is always set outside the program, and should never be hardcoded into the source code.

Next we divide up the number of "throws" into roughly equal chunks, just as we did in the corresponding Multiprocessing example.  The same list is generated, but we use _myrank_ to select the element of the list that each rank will use.

After this each process invokes the `pi` routine using `myNumPoints` for its rank.  We need to collect all the estimates and average them.  The _reduction_ collects the results from each rank (in rank order) and applies the "op" (operation).  A reduction must be a binary operation that returns another object of the same type; the operation is applied along the sequence.  In this case we want the sum so we add the result from rank 0 to that from rank 1, then we take that sum and add the result from rank 2, and so forth.  Dividing by the number of processes provides the average.  

The Reduce function returns each result to the process regarded as the root, which would normally be 0 as it is in this case, and root carries out the operation.  Thus _only_ the root process knows the final result.  We then select root to print out the value.

This example reruns the problem without distributing the throws in order to obtain a serial time for comparison purposes.  Of course, a real application would not do that.

### Running the MPI Python Program on an HPC System

In order to launch multiple tasks (or processes) of our program, we run this program through the MPI executor.  On our HPC cluster this is srun.

```
srun python MonteCarloPiMPI.py 1000000000
```

**Note: you cannot launch the MPI program with `srun` on the login nodes.**  In order to execute our program on designated compute node(s), we need to write a simple bash script that defines the compute resources we need.  We call this our job script.  For our example, the job script `pimpi.sh` looks like this:

{{% code-download file="/courses/python-high-performance/codes/pympi.slurm" lang="bash" %}}

The `#SBATCH` directives define the compute resources (`-N`, `--ntasks-per-node`, `-p`), compute wall time (`-t`), and the allocation account (`--account`) to be used. `-N 1` specifies that all MPI tasks should run on a single node.  We are limiting the number of nodes for this workshop so that everyone gets a chance to run their code on the shared resources. Be sure to edit the script to activate your environment by its correct name.

**Submitting the job:**

Open a terminal window. Do not load any modules. Execute this command:
```
sbatch pympi.slurm
``` 

**Checking the job status:**

Check the job status with the `squeue -u` or `sacct` commands as described in the [Multiprocessing](#multiprocessing) section. 

**Checking the output file:**

Open the `slurm-XXXXXX.out` files in a text editor and record the total run time for the job.

**Exercise**

Rerun the MPI job using 1 or 4 cpu cores by changing the `--ntasks-per-node` option.

### Scaling
On the cluster, the timings are very similar to Multiprocessing on the workstation.

{{< table >}}
| CPU Cores | Run Time |
| --- | --- |
| 1 (serial) | 400 sec|
| 4 | 102 sec |
| 8 | 60 sec |
| 16 | 31 sec |
{{< /table >}}

## Dask-MPI

Dask can use `mpi4py` on a high-performance cluster.  First install mpi4py according to the instructions in the previous section, then `pip install --user dask-mpi`. 

### Schedulers

We have not discussed Dask [_schedulers_](https://docs.dask.org/en/latest/scheduling.html) previously. The scheduler is a process that managers the workers that carry out the tasks.  We have been implicitly using the _single-machine_ scheduler, which is the default. Within the single-machine scheduler are two options, _threaded_ and _processes_.  The threaded single-machine scheduler is the default for Dask Arrays, Dask Dataframes, and Dask Delayed.  However, as we discussed with [Multiprocessing](/courses/python-high-performance/multiprocessing), the GIL (Global Interpreter Lock) inhibits threading in general.  Most of NumPy and Pandas release the GIL so threading works well with them.  If you cannot use NumPy and Pandas then the processes scheduler is preferred.  It is much like Multiprocessing.

To use Dask-MPI we must introduce the Dask `distributed` scheduler. The `distributed` scheduler may be preferable to `processes` even on a single machine, and it is required for use across multiple nodes. 

### Running Dask-MPI

We will discuss here only the batch interface for Dask MPI.  Dask-MPI provides an `initialize` function that initializes MPI and sets up communications. You must then start a Client cluster to connect the workers.  Dask-MPI uses rank 0 for the manager process and rank 1 to mediate between manager and workers, so you must request _at least_ 3 processes.  Close the cluster at the end of your script to avoid error messages about terminated processes.  

**Example**
Convert the "timeseries" example to Dask MPI.

{{< code-download file="/courses/python-high-performance/codes/dask_df_mpi.py" lang="python" >}}

Run this simple example with

{{< code-download file="/courses/python-high-performance/codes/run_dask_mpi.slurm" lang="bash" >}}

The OMPI_MCA environment variable suppresses a warning message that is seldom relevant.

Using Dask-MPI is not difficult, especially in batch mode, but users interested in trying it should be sure to first understand the distributed scheduler, then study the online examples carefully.  Dask-MPI does not require explicit calls to MPI but with the convenience comes some loss of control; the algorithm must be suited to the distributed scheduler.  More information may be found in the [documentation](https://mpi.dask.org/en/latest/batch.html).

Full documentation for Dask-MPI is [here](http://mpi.dask.org/en/latest/).

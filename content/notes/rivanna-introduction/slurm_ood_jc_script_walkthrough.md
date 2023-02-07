---
title: The Slurm Script
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 720

menu:
  rivanna-introduction:
    parent: The Slurm Resource Manager
---

The "Hello World" demo Slurm script contains the information required to request resources and run the job.

{{< code file="/notes/rivanna-introduction/snippets/demo.slurm" lang="bash" >}}

Let's look at this script line by line.
```no-highlight
#!/bin/bash
```
This tells the system that the file is a _bash script_.  Bash is the default _shell_ on Linux.  A shell is a program that sits between the user and the operating system and enables us to interact with the system.  When you use a terminal to type commands, you are probably using bash.  But bash is also a programming language and we can write scripts for it.

```no-highlight
#SBATCH --ntasks=1
```
This is a resource request.  Every request starts with the string
```no-highlight
#SBATCH
```
Here we are requesting one _task_ (ntasks stands for number of tasks).  To Slurm, a task is a process carried out by the operating system.  Most usually, it is your program's executable.  In this case, we are asking for a single process on a single core.
```no-highlight
#SBATCH --mem=6000
```
This requests a total of 6000MB or 6GB of memory.  This is for the total amount of memory over the entire set of cores to be used, so typically the request would be larger, or we would use another form
```no-highlight
#SBATCH --mem-per-cpu=9000
```
This stands for "memory per cpu" (Slurm still often refers to a core as a "cpu").
```no-highlight
#SBATCH --partition=standard
```
This requests the partition to which the job will be submitted.
```no-highlight
#SBATCH --account=your_account
```
Specifies the allocation to be charged.  (This does not refer to your user account.)
```no-highlight
#SBATCH --time=00:10:00
```
Request that the job should run for 10 minutes.  It is a good idea to try to get an accurate estimate of the time you will need for the job, in order not to use SUs unnecessarily.
```no-highlight
echo "Hello $USER, this is node $(hostname)."
```
This line executes the job.  It is a command to bash to print a string.  Your user ID is substituted for `$USER`, and the node on which it's running for `hostname`.

When you submit the job, a process called the _scheduler_ examines your script, looking at all lines starting with `#SBATCH`.  It extracts the resources requestd and places your job in the appropriate partition.

Once your job starts, the script is executed on the compute node as a normal bash script.  In bash scripts, lines that begin with the hash symbol `#` are comments and are ignored.

There are many other options for Slurm.  See our [documentation](https://www.rc.virginia.edu/userinfo/rivanna/slurm/) or the developer's [cheat sheet](https://slurm.schedmd.com/pdfs/summary.pdf).  The templates available through OOD also provide more examples.

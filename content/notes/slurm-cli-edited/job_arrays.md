---
title: Job Arrays
date: 2023-12-11-14:11:14Z
type: docs 
weight: 3000
menu: 
    slurm-from-cli:
---

Many similar jobs can be submitted simultaneously through _job arrays_. There are some restrictions:

* It must be a batch job.
* Job arrays should be explicitly named with `-J`
* It is generally prudent to separate stdout and stderror with `-o` and `-e`

A job array is submitted with `sbatch --array=<range>`, where `range` is two digits separated by a hyphen.
```bash
$sbatch --array=0-30 myjobs.sh
```
An increment can be provided
```bash
$sbatch --array=1-7:2 myjobs.sh
```
This will number them 1, 3, 5, 7

It is also possible to provide a list
```bash
$sbatch --array=1,3,4,5,7,9 myjobs.sh
```

Each job will be provided an environment variable `SLURM_ARRAY_JOB_ID` and
each task will be assigned a `SLURM_ARRAY_TASK_ID`.  The ARRAY_JOB_ID is the overall jobid, whereas the `ARRAY_TASK_ID` will take on the values of the numbers in the specified range or list.

Slurm also provides two variables `%A` (global array ID) and `%a` (array task ID) which can be used in the `-o` and `-e` options.  If they are not used, then the different tasks will attempt to write to the same file, which can result in garbled output or file corruption, so please use them if you wish to redirect streams with those options.

To prepare a job array, set up any input files using appropriate names that will correspond to the numbers in your _range_ or _list_, e.g.
```
myinput.0.in
myinput.1.in
...
myinput.30.in
```
You would submit a job for the above files with
```bash
$sbatch --array=0-30
```
In your Slurm script you would use a command such as
```bash
python myscript.py myinput.${SLURM_ARRAY_TASK_ID}.in}
```

The script should be prepared to request resources for _one_ instance of your program.

Complete example array job script:
{{< code-download file="/notes/slurm-from-cli/scripts/array.slurm" lang="bash" >}}

To cancel an entire array, cancel the global ID
```bash
scancel 1283839
```
You can also cancel individual tasks
```bash
scancel 1283839_11
```

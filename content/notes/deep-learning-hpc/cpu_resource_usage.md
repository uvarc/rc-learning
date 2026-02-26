---
title: CPU Resource Usage
date: 2024-06-28T01:51:43Z
type: docs 
weight: 550
toc: true
menu: 
    deep-learning-hpc:
      parent: Resource Allocation and Helpful Tools  
---


## CPU Resource Usage For Running Jobs

For a  __running job__, the `sstat` command will report on CPU and memory usage.  

In this example, the job has been running on **20 cores** for about **4 days**.
{{< figure src=/notes/deep-learning-hpc/img/sstat.png caption="" width=70% height=70% >}}


## CPU Resource Usage For Completed Jobs

For a  __completed job__ , the `seff` command will return an efficiency report.

{{< figure src=/notes/deep-learning-hpc/img/CPU_Resource_Usage.png width=60% height=60% >}}

* CPU Efficiency: This is good usage for the number of cores.
* Memory Efficiency: Only about 4 GB of (CPU) memory was needed.

Visit our [documentation](https://www.rc.virginia.edu/userinfo/hpc/slurm/#cpu-and-memory-usage) about CPU and Memory Usage for more information. 


## CPU Efficiency

It may be the case that even if CPU Efficiency is a low percentage, you need all of the requested CPU cores for a specific part of the code, e.g., data preprocessing. In this case, request the number of CPU cores that you need for the **compute intensive** part of the code.


## Previous Job Numbers

The `sacct` command will return a list of previous Slurm jobs on the __current__ day.
* To get jobs run on previous days, use the `-S` flag and provide a start date
* Ex: to display all Slurm jobs submitted after 2/1/2024
```bash
$ sacct -X -S2024-02-01
```
(the `-X` flag is optional, but tells `sacct` to skip the output of intermediate steps):


## Check Your Knowledge 

* Find your most recent job number using `sacct`.
* Print out the resource usage using `seff` for the job number you found in step 2.
  * How efficient was your CPU and memory usage?
---
title: Using a Script from a Path or Job
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 750

menu:
  rivanna-introduction:
    parent: The Slurm Resource Manager
---

We had a syntax error in our Python script.  After looking at it, we realize that our Python script is written for Python 2, but the `anaconda` module loads Python 3.  We open a terminal and type
```bash
module spider anaconda
```

## From a Path

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_module_spider.png" caption="Result from checking versions of anaconda." >}}

The default is 
```bash
anaconda/2020.11-py3.8
```
We need to use
```bash
 anaconda/2019.10-py2.7
```

We have most of the files we need in the failed job's folder, so we can start a new job from the path to that folder and they will be copied.

Select the path to the job and copy it to your clipboard. From the "New Job" dropdown, choose "From Specified Path."  When the form opens, paste in the path.  It will complain that the script name is missing, but you can open Job Options as it requests, and generally it will find the correct script to copy.  You will then be able to edit it as before to change the module.  Submit the job.

When it completes, go to the directory.  Note that it copied all the files, including the Slurm output from the failed job. 

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_fixed_job_files.png" caption="Corrected job (we hope)." >}}

View the `slurm-123456.out` file.  You should see

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_fixed_job_output.png" caption="Result from the correct version of anaconda." >}}

## From Another Job

The New Job option "From Selected Job" is similar, but can only be used with existing jobs, whereas "From a Path" could copy from a master path to new jobs.  From Selected Job should be self-explanatory once you understand the Path option.

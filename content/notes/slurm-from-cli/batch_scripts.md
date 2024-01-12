---
title: Batch Scripts
date: 2023-12-11-14:11:14Z
type: docs 
weight: 500
menu: 
    slurm-from-cli:
---

Jobs are described to the resource manager in the form of a _script_.  Typically this is written in the _bash_ scripting language.  Bash is the default shell on most Linux-based systems, which includes the majority of HPC systems, so it is expected to be available to interpret the script.  However, Slurm accepts scripts in other languages if the interpreter is available.  We will consider only bash scripts in this tutorial.

To prepare a job, the user writes a script. The top of the script is a _preamble_ that describes the resource requests. The rest of the script contains the instructions to execute the job. The script is then **submitted** to the Slurm system. The Slurm workload manager examines the preamble to determine the resources needed, while ignoring the rest of the script. It uses the resource request along with a **fair share** algorithm to set the priority of the job.  The job is then placed into the requested partition to wait for the resources to become available.  

Once the job starts, the slurmd daemon runs the script as an ordinary shell script. The preamble consists of _comments_ (code that is not executed by the interpreter) so they are ignored. The rest of the script must be a valid bash shell script.


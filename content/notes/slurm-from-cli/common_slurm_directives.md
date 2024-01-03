---
title: Common SLURM Directives
date: 2023-12-11-14:11:14Z
type: docs 
weight: 900
menu: 
    slurm-from-cli:
---

The most commonly used Slurm directives are listed in the table below.  Many options have two versions, one with a single hyphen `-` followed by one letter, or two hyphens `--` followed by an equals sign `=` and a word.  Some commands have no single-letter equivalent.

Angle brackets `< >` indicate a value to be specified, and are not typed.

{{< table >}}
|  Single-hyphen Option | Double-Hyphen Option|  Action |
|  -----  | -----| ---- |
| -a \<list\>  | -\-array=\<list\> | This is a job array with parameters in \<list\> |
| -c \<ncpus\> | -\-cpus-per-task=\<ncpus\> | Number of cpus (cores) to be assigned to each task.  For threaded code. Ensures all cores are on the same node.|
| -C \<list\> | -\-constraint=\<list\> | Specify certain resource constraints |
| -D \<directory\> | -\-chdir=\<directory\> | Change to \<directory\> before starting the job. Default is directory from which job is started. |
| -e \<name\> | -\-error=\<filename\> | Separate standard error from standard output and print to file \<name\> |
| None | -\-export=\<vars\> | Specify which environment variables are to be exported. Other options are  ALL (the default) or  NONE |
| | -\-gres=\<list\> | Specify "generic consumable resources." For example  `--gres=gpu:2` |
|-J \<jobname\> | -\-job-name=\<jobname\> |	Specify a name of your choosing for the job rather than the default script name. |
| None | -\-mail-type=\<type\> | Notify me by email upon certain events. Options are NONE (default) BEGIN (job begins),  END (job ends), FAIL (job fails) , REQUEUE (job is requeued), or ALL |
| None | -\-mail-user=\<email\> |	Specify the email for notifications. |
| None | -\-mem=\<size[units]\> | 	Specify the total memory request per node, over the entire node. The default unit is megabytes. |
| None | -\-mem-per-cpu=\<size[units]\> | Memory request per allocated core. Default unit is megabytes. |
| -n \<number\> | -\-ntasks=\<number\> | Request a total number of tasks over all nodes allocated. |
| None | -\-ntasks-per-node=\<ntasks\> | Request that a minimum of `ntasks` be assigned to each node. 
| -N \<nnodes\> | -\-nodes=\<nnodes\> | Request `nnodes` nodes. Should be used only with MPI or other protocols able to use them. |
| -o \<filename\> | -\-output=\<filename\> | Specify a name of your choosing for the standard output file rather than the default of `slurm`\<jobid\>`.out`. | 
| -p \<name\>| -\-partition=\<names\> | Specify the partition to run the job. |
| -t \<time\> | -\-time=\<time\> | Set the upper limit of the runtime. Format can be `M` (a number of minutes), `MM:SS` (minutes:seconds), `HH:MM:SS` (hours:minutes:seconds), `D-H` (days-hours), `D-HH:MM` (days-hours:minutes), or `D-HH:MM:SS` (days-hours:minutes:seconds). |
{{< /table >}}

See also our [documentation](https://www.rc.virginia.edu/userinfo/rivanna/slurm/) for many more examples.

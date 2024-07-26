---
title: V - HPC Specifics
date: 2023-12-11T00:00:00-05:00
type: docs
toc: true 
weight: 50
menu: 
    hpc-from-terminal:
---

Most high-performance computing clusters have commands that are specific to that environment, and are not part of Unix per se.

## Home Directory

The default home directory provides 50GB of storage capacity, e.g., /home/mst3k.  Each user also has access to 10 TB of  __temporary__  storage in the "scratch" folder, e.g. /scratch/mst3k

The /home and /scratch directories are for personal use and not shareable with other users.

{{< warning >}}
/scratch is NOT permanent storage and files that have not been accessed for more than 90 days will be marked for deletion.
{{< /warning >}}

Research groups can lease permanent [storage](https://www.rc.virginia.edu/userinfo/storage/).

## Checking Your Storage

To see how much disk space, you have used in your home and scratch directories, open a terminal window and type  `hdquota`  at the command-line prompt:

```bash
$hdquota
Type    Location         Name                  Size Used Avail Use%
=======================================================================
home       /home        mst3k                   51G   12G   39G  24%
Scratch    /project     slurmtests             2.0P  1.9P  144T  93%
Project    /project     arcs                    16T   12T  3.8T  75%   Project    /project     rivanna_software       1.1T  4.2M  1.0T   1%   Project    /project     ds5559                  51G  3.7G   47G   8%   Value      /nv          vol174                 5.5T  1.2T  4.4T  21%
```

Note: only home, and other storage just for some

## Checking Your Allocation

To see how many SUs you have available for running jobs, type at the command-line prompt `allocations`.

```bash
$allocations
 Allocations available to Misty Tea (mst3k):
  ds5559: less than 25,000 service-units remaining
  ga_bioinfo-test: less than 100,000 service-units remaining
```

For more information about a specific allocation, please run
```bash
$allocations -a <allocation name>
```

## The Modules Environment

_Environment modules_ are not strictly a part of Unix, but are widely used by many HPC sites, including ours.  Modules enable the user to set complex paths, environment variables, and so forth, simply by loading a module.

The commands are set up automatically when you log in.  Loaded modules only affect the shell in which the command is run.

Modules required for a job must be loaded in the batch _job script_.

## Module Commands

{{< table >}}
|  Command   |  Result  |
|  -------   |  ------  |
|  module load \<name\> |  load the default module for the <name> package |
|  module load \<name\>/\<version\> | load the specific version of a package |
|  module spider \<name\> | view the available versions of a package | 
|  module spider \<name\>/\<version\> | view instructions for loading a version |
|  module purge | clear all modules |
|  module swap \<name\>/\<version1\> \<name\>/\<version2\> | switch from version 1 to version 2 |
{{< /table >}}

**Examples**
```bash
$module load gcc
$module load matlab/R2023a
$module spider R/4.3.1
$module load gcc/11.4.0 openmpi/4.1.4 R/4.3.1
$module purge
```

## Running Jobs

In an HPC environment, the tasks that users want to perform are generically called **jobs**.  These must not be confused with the "jobs" of the Unix `jobs` command, although the concepts are related. An HPC "job" is a combination of _resource requests_, any setup required such as loading modules, and the actual commands required to run the desired software.

HPC jobs are run on _compute nodes_, not on the interactive loginnodes.

In the Introduction we discussed using the OOD [job composer](/notes/rivanna-intro/features_of_ood/features_jobs_tab) to submit and monitor jobs. When working at the command line, we must use more detailed features of Slurm.  Please go through our [tutorial](/notes/slurm-from-cli) to learn how to write and submit Slurm scripts from the command line.

## Need Help?

Research Computing is ready to help you learn to use our systems efficiently.  You can [submit a ticket](https://www.rc.virginia.edu/form/support-request/).  For in-person help, please attend one of our weekly sessions of [office hours](https://www.rc.virginia.edu/support/#office-hours).

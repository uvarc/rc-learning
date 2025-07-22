---
title: HPC Overview on Loading Modules
date: 2025-7-10T00:00:00
type: docs 
weight: 300
menu: 
    genomics:
---

## Logging In

To log in to the HPC system, open a terminal and use the following command:

```bash
ssh user_id@login.hpc.virginia.edu
```

To print your working directory:

```bash
pwd
```

The `/home/user_id` directory is your home directory and provides 200GB of permanent storage. The `/scratch/user_id` directory provides up to 10TB of temporary storage. Scratch files not accessed for 90 days are automatically purged. You can check which files will be purged through our [Open OnDemand utilities](https://www.rc.virginia.edu/userinfo/hpc/ood/).

## HPC Allocations & Storage

To check your disk space usage:

```bash
hdquota
```

To check your active allocations and balances:

```bash
allocations
```

If you sign up for one of our HPC workshops, you will be assigned to the allocation `hpc_training`. After the workshop, the `hpc_training` allocation will be removed.

## Modules

A module is a software utility that allows multiple versions of a software to exist on the same system. It helps to simplify the management and access of software applications. Below are a list of helpful commands for using modules through the command-line interface.

{{< table >}}
| Command | Action |
|-----|-----|
| `module avail` | List all available modules that can be loaded immediately |
| `module spider` | List all available packages (may be a lot!) |
| `module spider <\package\>` | List all versions of <\package\>, if any |
| `module spider \<package\>/\<version\>` | Describes how to load \<package\>/\<version\>. There may be prerequisite modules |
| `module key <search term>` | Search for a module using a keyword |
| `module list` | List modules loaded in current shell |
| `module load \<package\>/[\<version\>]` | Load the module for (optionally) \<version\> of \<package\> |
| `module unload \<package\>` | Delete the changes made by the \<package\> module |
| `module purge` | Remove all module modifications to the environment |
| `module swap \<package\>/\<current\> \<package\>/\<newver\>` | Exchange one version of a package for another |
{{< /table >}}

Overall, there are three main approaches to running HPC jobs: interactively, using modules, or downloading software locally.

## More Research Computing Resources

UVA Research Computing Learning Portal:  
https://learning.rc.virginia.edu

Using UVA’s HPC system from the terminal:  
https://learning.rc.virginia.edu/notes/hpc-from-terminal/

HPC orientation session and office hours:  
https://www.rc.virginia.edu/support/#office-hours
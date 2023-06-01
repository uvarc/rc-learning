---
title: Preparing and Submitting from the Command Line
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 760

menu:
  rivanna-tutorial:
    parent: The Slurm Resource Manager
---

Users who are comfortable with the command line can submit jobs directly. This is currently necessary in order to run jobs from /scratch or /project.  

For a more in-depth introduction to using Rivanna from a command line, see our [tutorial](/notes/rivanna-cli).

### Prepare a Folder

Most of the time, it is advisable for you to have a folder for each job or set of related jobs.  You can use OOD's File Explorer to set up your directories if you are not yet comfortable using a command line.  This folder should contain all the files you need to run your job.

### Write the Script

You must first write a script.  You may use an OOD template, or one of our [examples](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#sample-slurm-command-scripts).  If you know a text editor you can use that to edit the template.  Otherwise you can use the built-in OOD text editor, or you can use pluma/gedit through FastX.  Please do not use LibreOffice.  Slurm scripts should not be created or edited on your local computer; always use a Rivanna frontend.

Slurm's default is to change into the folder from which the job was submitted, so make sure your script is where you intend the job to run.

### Submit the Script

You must navigate to the directory you created.  Suppose it is `/scratch/mst3k/myjob`.  From a terminal, use the `cd` (change directory) command.
```bash
cd /scratch/mst3k/myjob
```

Now you can submit the script with the `sbatch` command.
```bash
sbatch myscript.slurm
```

Now you can monitor the job through the OOD Active Jobs tab, or with the `squeue` command.

To use a directory in leased /project storage, substitute the appropriate path, starting with /project, for the scratch directory.  Otherwise the process is the same.


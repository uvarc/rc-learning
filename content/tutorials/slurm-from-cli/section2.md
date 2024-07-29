---
date: "2023-12-11"
title: "II - Working with Slurm Scripts"
weight: 20
---

## Writing Batch Scripts

Batch scripts should be written on a cluster login node.  Please do not use your local computer to write them, as they may not work.  You must also use a _text editor_ and not a word-processing program.

Several options are available to prepare batch scripts.  

### Graphical Editors

You can log in to a [FastX](https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/), which provides a [MATE](https://mate-desktop.org/) desktop environment. One of the tools is a graphical editor very similar to Notepad.  It is called `pluma` by MATE, but we have made it available as `gedit` if started from a terminal.  If you wish to start it from a menu, it is available from Applications&rarr;Accessories.

You can also use [Open OnDemand's](https://www.rc.virginia.edu/userinfo/rivanna/ood/overview/) built-in file manager and editor.  Create a new file from the Files menu.  Select the file and choose `Edit` from the three-dot dropdown menu to the right of the file name.  This will open a very basic text editor.

### Command-Line Editors

Editors available at the command line are [nano](https://www.nano-editor.org/), [vim](https://www.vim.org/), and [emacs](https://www.gnu.org/software/emacs/).  Nano is a simple text-only editor.  Vim is also available text-only from a command line, but a graphical version called `gvim` can be invoked from a MATE Desktop through the Applications&rarr;Accessories menu. Emacs can also be started from the Accessories menu but, if a graphical environment, will start a graphical user interface.  If invoked within a text-only environment, it will fall back to a text interface.

## Our First Slurm Script

This example illustrates the main parts of a Slurm script. 

In a bash script, any text beyond the `#` is ignored.

{{< code-download file="/tutorials/slurm-from-cli/scripts/hello.slurm" lang="bash" >}}

This script runs the Python code

{{< code-download file="/tutorials/slurm-from-cli/scripts/hello.py" lang="python" >}}

## The Hello.slurm Script

The first line says that this script should be run by the `bash` interpreter.
```bash
#!/bin/bash
```

The lines starting with `#SBATCH` are the resource requests.  They are called "pseudocomments" since they have meaning to Slurm.  There must be no space between `#` and `SBATCH` and the string must start in the first column of the line. 

```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=32000 # mb total memory
#SBATCH --time=2:00:00
#SBATCH --partition=interactive
#SBATCH --account=hpc_training
```
Here we are requesting
  * 1 node, 1 task
  * 32GB of memory (measured in MB). Strictly speaking this will be "Gibibyes."
  * 2 hours of running time.
  * The standard partition (queue).  A partition must be specified.
  * The account (allocation) group `hpc_training`

The next lines set up the environment to run our job.
```bash
module purge
module load anaconda
```

It is good practice to purge all modules first, since Slurm "remembers" any modules set in the environment where the script is launched.  Next we load the module we need to run our program, the Python distribution Anaconda.

Finally, we execute our job.
```bash
python hello.py
```

We have chosen to name this script `hello.slurm`, but it can have any name.

**Exercise 1**

Download the hello.slurm and hello.py scripts. Transfer them to the cluster by whatever means you wish.  Modify the Slurm script to use your own allocation group name.

## Common Slurm Directives

The most commonly used Slurm directives are listed in the table below.  Many options have two versions, one with a single hyphen `-` followed by one letter, or two hyphens `--` followed by an equals sign `=` and a word.  Some commands have no single-letter equivalent.

Angle brackets `< >` indicate a value to be specified, and are not typed.

| Single-hyphen Option | Double-Hyphen Option           | Action                                                                                                                                                                                                                                        |
|----------------------|--------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| -a \<list\>          | -\-array=\<list\>              | This is a job array with parameters in \<list\>                                                                                                                                                                                               |
| -c \<ncpus\>         | -\-cpus-per-task=\<ncpus\>     | Number of cpus (cores) to be assigned to each task.  For threaded code. Ensures all cores are on the same node.                                                                                                                               |
| -C \<list\>          | -\-constraint=\<list\>         | Specify certain resource constraints                                                                                                                                                                                                          |
| -D \<directory\>     | -\-chdir=\<directory\>         | Change to \<directory\> before starting the job. Default is directory from which job is started.                                                                                                                                              |
| -e \<name\>          | -\-error=\<filename\>          | Separate standard error from standard output and print to file \<name\>                                                                                                                                                                       |
| None                 | -\-export=\<vars\>             | Specify which environment variables are to be exported. Other options are  ALL (the default) or  NONE                                                                                                                                         |
|                      | -\-gres=\<list\>               | Specify "generic consumable resources." For example  `--gres=gpu:2`                                                                                                                                                                           |
| -J \<jobname\>       | -\-job-name=\<jobname\>        | 	Specify a name of your choosing for the job rather than the default script name.                                                                                                                                                             |
| None                 | -\-mail-type=\<type\>          | Notify me by email upon certain events. Options are NONE (default) BEGIN (job begins),  END (job ends), FAIL (job fails) , REQUEUE (job is requeued), or ALL                                                                                  |
| None                 | -\-mail-user=\<email\>         | 	Specify the email for notifications.                                                                                                                                                                                                         |
| None                 | -\-mem=\<size[units]\>         | 	Specify the total memory request per node, over the entire node. The default unit is megabytes.                                                                                                                                              |
| None                 | -\-mem-per-cpu=\<size[units]\> | Memory request per allocated core. Default unit is megabytes.                                                                                                                                                                                 |
| -n \<number\>        | -\-ntasks=\<number\>           | Request a total number of tasks over all nodes allocated.                                                                                                                                                                                     |
| None                 | -\-ntasks-per-node=\<ntasks\>  | Request that a minimum of `ntasks` be assigned to each node.                                                                                                                                                                                  |
| -N \<nnodes\>        | -\-nodes=\<nnodes\>            | Request `nnodes` nodes. Should be used only with MPI or other protocols able to use them.                                                                                                                                                     |
| -o \<filename\>      | -\-output=\<filename\>         | Specify a name of your choosing for the standard output file rather than the default of `slurm`\<jobid\>`.out`.                                                                                                                               | 
| -p \<name\>          | -\-partition=\<names\>         | Specify the partition to run the job.                                                                                                                                                                                                         |
| -t \<time\>          | -\-time=\<time\>               | Set the upper limit of the runtime. Format can be `M` (a number of minutes), `MM:SS` (minutes:seconds), `HH:MM:SS` (hours:minutes:seconds), `D-H` (days-hours), `D-HH:MM` (days-hours:minutes), or `D-HH:MM:SS` (days-hours:minutes:seconds). |

See also our [documentation](https://www.rc.virginia.edu/userinfo/rivanna/slurm/) for many more examples.



## Modules

Any application software that you want to use will need to be loaded with the `module load` command.  

For example:

```
$ module load matlab
$ module load anaconda
$ module load goolf R
```
Modules need to be loaded any time that a new shell is created to set up the same working environment. This includes every time that you log out and back in, and every time that you run a batch job on a compute node.

### Module Details

`module avail` – Lists all available modules and versions.

`module spider` – Shows all available modules

`module key keyword` – Shows modules with the keyword in the description

`module list` – Lists modules loaded in your environment.

`module load mymod` – Loads the default module to set up the environment for some software.

`module load mymod/N.M` – Loads a specific version N.M of software mymod.
module load compiler mpi mymod – For compiler- and MPI- specific modules, loads the modules in the appropriate order and, optionally, the version.

`module purge` – Clears all modules.

### Learning more about a Module

To locate a python module, try the following:

```
$ module avail python
$ module spider python
$ module key python
```

To find bioinformatics software packages, try this:

```
$ module key bio
```

The available software is also listed on our [website](https://www.rc.virginia.edu/userinfo/rivanna/software/complete-list/)

**Question:**

Why does the command `module load R` give an error?


## Working with Files and Folders

When using Slurm in terminal mode, you will probably want to create your own folders to organize your Slurm scripts, any input files, and the output.  You will need to be able to move around from one folder to another at the terminal.

By default, Slurm will start your job from the folder in which it was launched. You can change that with the `-D` option (directory) but many users simply navigate to the folder and type commands.

### Creating Files and Folders

There are several options to create, rename, and move your files and folders. Note that folders are usually called "directories" in Unix.

#### FastX

Use the Caja file manager.  This shows up as a filing-cabinet icon in the upper-left corner of the ribbon of the MATE Desktop.  It can also be started from the menu Applications&rarr;System Tools&rarr;Caja. Caja's layout is very similar in appearance and behavior to Windows Explorer and similar tools.

#### Open OnDemand

Use the File Manager to create, rename, or move your folders.

#### Command Line

If you are familiar with the command line, you can use that. If you wish to learn it, you can go through our [Unix Tutorials for Beginners](https://learning.rc.virginia.edu/tutorials/unix-tutorial/), especially Tutorials 1--3.  You can also go through our [HPC from the Terminal](https://learning.rc.virginia.edu/tutorials/rivanna-command-line/) tutorial if you have not already done so.

#### Changing into a Directory

If you do not wish to learn the full command-line navigation, you will need to learn the `cd` command to get to your folder for launching your job.

Log into a terminal in FastX, or open a terminal through the Clusters tab in Open OnDemand.

The `cd` command stands for "change directory." It is followed by a **path** to that directory. In the examples below, `mst3k` is a generic user ID. Substitute your own.

```bash
$cd myworkdir
$cd /scratch/mstk3/myprojectdir
$cd
```
The `cd` command with no options returns you to the top level of your home directory.  

You may also wish to learn `pwd` for "print working directory" so you can find out where you are.

```bash
$cd shakespeare
$pwd
/home/mst3k/shakespeare
```

**Exercise 2**

Use FastX or Open OnDemand or the command line to create a new folder under your scratch directory. Practice changing into and out of it.

Use FastX and Caja to navigate to your `/scratch` directory. To get there, click `Go` in the Caja menu.  A textbox will open. Be sure that "search for files" is unchecked.  Erase whatever is in the textbox and type `/scratch/mst3k` (substituting your own user ID).  Still in FastX, open a terminal (the black box, or in the System Tools menu) and navigate to your new scratch folder.


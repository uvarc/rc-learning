---
title: Working with Files and Folders
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1000
menu: 
    slurm-from-cli:
---

When using Slurm in terminal mode, you will probably want to create your own folders to organize your Slurm scripts, any input files, and the output.  You will need to be able to move around from one folder to another at the terminal.

By default, Slurm will start your job from the folder in which it was launched. You can change that with the `-D` option (directory) but many users simply navigate to the folder and type commands.

## Creating Files and Folders

There are several options to create, rename, and move your files and folders. Note that folders are usually called "directories" in Unix.

### FastX

Use the Caja file manager.  This shows up as a filing-cabinet icon in the upper-left corner of the ribbon of the MATE Desktop.  It can also be started from the menu Applications&rarr;System Tools&rarr;Caja. Caja's layout is very similar in appearance and behavior to Windows Explorer and similar tools.

### Open OnDemand

Use the File Manager to create, rename, or move your folders.

### Command Line

If you are familiar with the command line, you can use that. If you wish to learn it, you can go through our [Unix Tutorials for Beginners](https://learning.rc.virginia.edu/notes/unix-tutorial/), especially Tutorials 1--3.  You can also go through our [HPC from the Terminal](https://learning.rc.virginia.edu/tutorials/rivanna-command-line/) tutorial if you have not already done so.

### Changing into a Directory

If you do not wish to learn the full command-line navigation, you will need to learn the `cd` command to get to your folder for launching your job.

Log into a terminal in FastX, or open a terminal through the Clusters tab in Open OnDemand.

#### Change Directory

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

Use FastX or Open OnDemand or the command line to create a new folder under your home directory. Practice changing into and out of it.

Use FastX and Caja to navigate to your `/scratch` directory. To get there, click `Go` in the Caja menu.  A textbox will open. Be sure that "search for files" is unchecked.  Erase whatever is in the textbox and type `/scratch/mst3k` (substituting your own user ID).  Still in FastX, open a terminal (the black box, or in the System Tools menu) and navigate to your new scratch folder.

---
title: Stream Output in Slurm
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1700
menu: 
    slurm-from-cli:
---

When running a program interactively, any output to the Unix [standard streams](https://learning.rc.virginia.edu/notes/unix-tutorial/unix_tutorial_3/) will be printed directly to the user's console window.  However, programs running under the control of Slurm will not have a console attached. 

By default, SLURM redirects both standard output and standard error to a file called `slurm-<jobid>.out`.

You can change the name of this file with the `-o` or `--output` option in your script.

```bash
#SBATCH --output=<filename>
```
or
```bash
#SBATCH -o <filename>
```

You can also separate standard-error output. Even if your program does not use standard error (not many do), Slurm uses it, so you may wish to keep that output distinct.
```bash
#SBATCH --error=<filename>
```
or
```bash
#SBATCH -e <filename>
```

Text from standard input must be redirected from a file in your command line in the script.
```bash
./myexec < myinput.txt
```

As an alternative to the Slurm options, you can also redirect standard output in the usual Unix manner if you prefer.
```bash
./myexec < myinput.txt > myoutput.dat
```

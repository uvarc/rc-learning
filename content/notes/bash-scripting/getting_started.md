---
title: "Getting Started"
type: docs
toc: true
# date: 2015-07-23T21:13:14-05:00
draft: false
weight: 200

menu:
   bash-scripting:
---

A shell script is a text file so should be created using a text editor.  Do not use a word-processing program such as LibreOffice Write. Shell scripts should always be created on the operating system (Linux, Mac OS) on which they will run.

The first line should be
```bash
#!/bin/bash
```
The `#!` is often called a _shebang_.

After the shebang, we can add some shell commands.  

**Example**

Here is a simple example of a script that clears the screen, pauses for three seconds, then displays a quote from a Website.

{{< code-download file="/notes/bash-scripting/scripts/qod.sh" lang="bash" >}}

If you have downloaded the file onto a Linux system, you can run it directly.  Otherwise, either transfer it to your intended system or copy and paste it into a plain text file named `qod.sh`. 

Linux shells are indifferent to file suffixes. The `sh` is a convention, but other suffixes may be used; for example, when writing a shell script for a resource manager such as Slurm, we may wish to use a `.slurm` suffix.

To run the script, type
```bash
bash qod.sh
```
This invokes `bash` to execute your script. Alternatively, you can modify your script to make it executable and run it:
```bash
chmod 755 myscript.sh
./quod.sh
```



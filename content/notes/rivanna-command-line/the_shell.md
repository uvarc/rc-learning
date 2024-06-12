---
title: The Shell
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 250
menu: 
    rivanna-command-line:
---
In all version of Unix, the __shell__ is a program that interprets commands and acts as an interface between the user and the kernel.

Multiple shells are available. In Linux systems, the default is the `bash` shell.  MacOS formerly defaulted to bash as well, but has recently switch to `zsh`.

The shell displays a __prompt__, which indicates that it is ready to accept commands.  In this tutorial, we will utilize the dollar sign `$` as the prompt; yours may be different, and later in this tutorial we will learn how to customize it to your preferences.

To determine your current shell, at the prompt type
```bash
echo $SHELL
```
It is important to keep in mind that Unix in general and the shell in particular is  __case-sensitive__.  The commands `LS` and `ls` would not be the same.


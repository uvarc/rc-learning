---
title: Section1- The Unix Operating System
date: 2023-12-11-14:11:14Z
type: docs 
weight: 200
toc: true
menu: 
    rivanna-command-line-edited:
---

__UNIX__ : is a text-oriented operating system (OS) originally developed at Bell Labs during the 1960s. Two versions of this OS are dominant today, _Linux_ and _Mac OS_. 

Strictly speaking, "Linux" refers just to the _kernel_, which is the part of an operating system that manages the hardware interfaces.  On top of the kernel sits a number of utilities that enable users and applications to interact with the kernel. 

Linux is the operating system most widely used at HPC facilities, internet servers, and the majority of financial trading system worldwide. A version of Linux powers Android systems.  

Mac OS is based on a slightly different version of Unix.

## Shell

In all version of Unix, the __shell__ is a program that interprets commands and acts as an interface between the user and the kernel.

Multiple shells are available. In Linux systems, the default is the `bash` shell.  MacOS formerly defaulted to bash as well, but has recently switch to `zsh`.

The shell displays a __prompt__, which indicates that it is ready to accept commands.  In this tutorial, we will utilize the dollar sign `$` as the prompt; yours may be different, and later in this tutorial we will learn how to customize it to your preferences.

To determine your current shell, at the prompt type
```bash
echo $SHELL
```
It is important to keep in mind that Unix in general and the shell in particular is  __case-sensitive__.  The commands `LS` and `ls` would not be the same.

## Logging In

Logging into a remote UNIX based system requires a program generally called a _client_. The options for the client depends on your OS.

## SSH

Command line access through a terminal on your computer is based on the `ssh` or Secure Shell protocol.  It is
  * Encrypted
  * Available on most UNIX systems

Your ssh client communicates with the ssh _server_ program running on the remote system.  Once established, the connection is secure even if the network is insecure.   

**Example**

If your computer is a MacOS or Linux system, you log in with
```bash
ssh -Y mst3k@virginia.edu
```
Throughout this tutorial we will use `mst3k` as our example user ID. You should substitute your own.  The option `-Y` allows access to graphical applications and requires that an _X11 server_ application must be installed on your computer.  This should be the default for Linux, but MacOS users must install [XQuartz](https://xquartz.org) before this command-line option will be useful.

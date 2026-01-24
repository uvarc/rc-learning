---
title: I - Intro to  Unix and Bash Shell
date: 2023-12-11T00:00:00-05:00
type: docs 
weight: 10
toc: true
menu: 
    hpc-from-terminal:
---

## UNIX

UNIX is a text-oriented operating system (OS) originally developed at Bell Labs during the 1960s. Two versions of this OS are dominant today, _Linux_ and _macOS_. 

Strictly speaking, "Linux" refers just to the _kernel_, which is the part of an operating system that manages the hardware interfaces.  On top of the kernel sits a number of utilities that enable users and applications to interact with the kernel. 

Linux is the operating system most widely used at HPC facilities, internet servers, and the majority of financial trading system worldwide. A version of Linux powers Android systems.  

macOS is based on a slightly different version of Unix.


## Shell

In all version of Unix, the __shell__ is a program that interprets commands and acts as an interface between the user and the kernel.

Multiple shells are available. In Linux systems, the default is the `bash` shell.  MacOS formerly defaulted to bash as well, but has recently switched to `zsh`.

The shell displays a __prompt__, which indicates that it is ready to accept commands.  In this tutorial, we will utilize the dollar sign `$` as the prompt; yours may be different, and later in this tutorial we will learn how to customize it to your preferences.

To determine your current shell, at the prompt type

```bash
$echo $SHELL
```

It is important to keep in mind that Unix in general and the shell in particular is  __case-sensitive__.  The commands `LS` and `ls` would not be the same.

## Logging In

Logging into a remote UNIX based system requires a program generally called a _client_. The options for the client depends on your OS.

### SSH

Command line access through a terminal on your computer is based on the `ssh` or Secure Shell protocol.  It is
  * Encrypted
  * Available on most UNIX systems

Your ssh client communicates with the ssh _server_ program running on the remote system.  Once established, the connection is secure even if the network is insecure.   

**Example**

If your computer is a macOS or Linux system, you log in with
```bash
ssh -Y mst3k@login.hpc.virginia.edu
```
Throughout this tutorial we will use `mst3k` as our example user ID. You should substitute your own.  The option `-Y` allows access to graphical applications and requires that an _X11 server_ application must be installed on your computer.  This should be the default for Linux, but macOS users must install [XQuartz](https://xquartz.org "The official XQuartz website") before this command-line option will be useful.

### Graphical Applications

The command-line secure shell is not the only option for accessing the HPC system. Windows users in particular may wish to use other methods, since although ssh is available for it, Windows is not particularly friendly to command lines.

#### Open OnDemand (OOD)

Open OnDemand is a Web-based interface to the system. It provides a graphical file-management interface and access to several popular applications running on the compute nodes. A simple terminal that opens on a loginnode is also provided.
See the introduction in our basic [tutorial](/notes/hpc-intro/connecting_to_the_system/connecting_ood/ "The Intro to High Performance Computing tutorial's Open OnDemand page").

#### FastX

FastX is a Web-based graphical interface to a loginnode. It is also covered in the [introduction](/notes/hpc-intro/connecting_to_the_system/connecting_fastx/ "The Intro to High Performance Computing tutorial's FastX page").

#### MobaXterm (Windows)

MobaXterm combines an ssh client, a _sftp_ client for file transfers, and an X11 server into a single bundle. More details are available at our [website](https://www.rc.virginia.edu/userinfo/rivanna/logintools/mobaxterm/ "The Research Computing website's MobaXterm page") or in the [introduction](/notes/hpc-intro/files/file_moba/ "The Intro to High Performance Computing tutorial's MobaXterm page").

## Running Shell Commands

The syntax of Unix commands is not completely standardized but in general follow the pattern of a two or three-letter abbreviation followed by _command-line options_, which are single-letter options preceded by one hyphen or multiple-letter options with two hyphens. Many commands also take _arguments_.

```bash
$cmd -o  --option  argument
```

**Example**

Invoke the utility `rm` to delete a file.
```bash
$rm myfile
```
In this example, the shell issues a request to the kernel to delete `myfile`.  The kernel then communicates with the software that manages file storage to execute the operation.

When it is complete the shell then returns the UNIX prompt to the user, indicating that it is waiting for further commands.

## Running Our First Command

Let’s run our first command. Into a terminal type
```bash
$pwd
/home/mst3k
```
This command stands for *p*rint *w*orking *d*irectory.  It prints the name of the folder in which the shell is currently working.

## Navigating the Bash Shell

Modern shells provide useful "hotkey" commands that can save considerable typing. The "control-" notation means that the `Ctrl` (control) key and the following key should be depressed at the same time.

{{< table >}}

| Function |  Key |
|-------|-----|
|Tab completion: expand typed string to as far as it has a unique name. | tab |
|Search for earlier command | control-r <cmd> |
|Move to the beginning of the line | control-a |
|Move to the end of the line | control-e |
|Clear the screen | `clear` or control-l |
{{< /table >}}

## Bash History

When using bash you may use its built-in history mechanism to save yourself some keystrokes.

{{< table >}}
| Function |  Key |
|-------|-----|
|scroll through the previous commands you have typed | up arrow|
|if already scrolled back, scroll to more recent commands | down arrow |
|edit text on a line | right or left arrow |
{{< /table >}}

## Logging Out

To log out of a shell, type `exit`
```bash
$exit
```

This logs you out of your current terminal window. If that is a login window, for example your `ssh` shell, it will log you out of the system entirely.


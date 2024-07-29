---
date: "2023-12-11"
title: "Introduction to Unix"
weight: 10
---

## Typographical Conventions

In what follows, we shall use the following typographical conventions:

* Characters written in color-coded typewriter font are commands to be typed into the computer as they stand.
* Characters written in standard typewriter font indicate non-specific file or directory names.
* Words inserted within square brackets, e.g. [Ctrl], indicate keys to be pressed.

So, for example:

```
% ls anydirectory [Enter]
```

means "at the Unix prompt `%`, type `ls` followed by the name of some directory, then press the key marked [Enter]." Don't forget to press the [Enter] key: commands are not sent to the computer until this is done.

Note: Unix is case-sensitive, so "LS" is not the same as `ls`. The same applies to filenames, so `myfile.txt`, `MyFile.txt` and `MYFILE.TXT` are three separate files. Beware if copying files to a PC, since DOS and Windows do not make this distinction.

## Introduction to the Unix Operating System

An _operating system_ is the suite of programs that make the computer work. Historically, Unix had two "flavors," System V from AT&T's Bell Labs, and BSD (for Berkeley Software Distribution).  Today that distinction is less relevant but the dominant versions of Unix still have different roots.  _Linux_, used for some workstations and many high-performance computing clusters, arose from System V.  Mac OS was derived from a version of BSD.  Android is a heavily-modified Linux.

The "windowing" (graphical) system on Linux is called X11.  Modern Linux systems all provide at least one "desktop" environment, similar to Windows or the Mac OS desktop. However, working at the command line is still often the most efficient use of teh system, so users can benefit by learning their way around a terminal.

A Unix operating system is made up of three parts: the kernel, the shell and the programs (applications or "apps").

#### The Kernel
The kernel of Unix is the hub of the operating system: it allocates time and memory to programs and handles the filesystem and communications in response to system calls. 

#### The Shell
The shell acts as an interface between the user and the kernel. When a user logs in, the login program checks the username and password, and then starts another program called the shell. The shell is a command line interpreter (CLI). It interprets the commands the user types in and arranges for them to be carried out. The commands are themselves programs: when they terminate, the shell gives the user another prompt. The adept user can customise his/her own shell, and users can use different shells on the same machine. The `bash` shell is the default on Linux.  Mac OS formerly used bash, but on newer releases the `zsh` shell is the default.  Most of the basic commands are the same.

As an illustration of the way that the shell and the kernel work together, suppose a user types `rm myfile` to delete the file `myfile`. The shell searches the filesystem, first for the file containing the program `rm`, and directs the kernel, through system calls, to _execute_ the program `rm` on myfile. When the process `rm myfile` has finished, the shell then returns the prompt to the user, indicating that it is ready for further commands.

##### Some Features
* Filename Tab Completion: By typing part of the name of a command, filename or directory and pressing the [Tab] key, the shell will complete the rest of the name automatically. If the shell finds more than one name beginning with those letters you have typed, it will halt, prompting you to type a few more letters before pressing the [Tab] key again.

    * Both bash and zsh support tab completion, but zsh has a somewhat more sophisticated set of features.  

* History: The shell keeps a list of the commands you have typed in. If you need to repeat a command, use the cursor keys to scroll up and down the list or type`history` for a list of previous commands.

   * Several discussions of the differences between bash and zsh are available online, such as [here](https://www.fosslinux.com/58416/bash-vs-zsh-differences.htm).  The rest of these tutorials are targeted to bash.

#### Prompt

The prompt is a character or string of characters that indicates the shell is ready for a command.  It varies by shell and system, and can be customized by more advanced users.  Throughout these tutorials we will us the percent sign `%` as  the prompt.

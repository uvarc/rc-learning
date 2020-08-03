---
title: "UNIX Tutorials for Beginners"
type: docs
toc: true
menu:
  unix-tutorials:
    name: Introduction
    weight: 1

---

## Introduction

These tutorials are derived from the excellent tutorials from the University of Surrey, UK, with some minor modifications for our site. The originals can be found [here](http://www.ee.surrey.ac.uk/Teaching/Unix/).

- - -

## Typographical Conventions

In what follows, we shall use the following typographical conventions:

* Characters written in bold typewriter font are commands to be typed into the computer as they stand.
* Characters written in italic typewriter font indicate non-specific file or directory names.
* Words inserted within square brackets, e.g. [Ctrl], indicate keys to be pressed.

So, for example:

```
% ls anydirectory [Enter]
```

means "at the UNIX prompt `%`, type `ls` followed by the name of some directory, then press the key marked [Enter]." Don't forget to press the [Enter] key: commands are not sent to the computer until this is done.

Note: UNIX is case-sensitive, so "LS" is not the same as `ls`. The same applies to filenames, so `myfile.txt`, `MyFile.txt` and `MYFILE.TXT` are three separate files. Beware if copying files to a PC, since DOS and Windows do not make this distinction.

- - -

## Introduction to the UNIX Operating System
This session concerns UNIX, which is a common operating system. By operating system, we mean the suite of programs which make the computer work. UNIX is used by the workstations and multi-user servers within the school. On X terminals and the workstations, X Windows provide a graphical interface between the user and UNIX. However, knowledge of UNIX is required for operations which aren't covered by a graphical program, or for when there is no X windows system, for example, in a telnet session.

### The UNIX operating system
The UNIX operating system is made up of three parts; the kernel, the shell and the programs.

#### The Kernel
The kernel of UNIX is the hub of the operating system: it allocates time and memory to programs and handles the filestore and communications in response to system calls. As an illustration of the way that the shell and the kernel work together, suppose a user types rm myfile (which has the effect of removing the file myfile). The shell searches the filestore for the file containing the program rm, and then requests the kernel, through system calls, to execute the program rm on myfile. When the process rm myfile has finished running, the shell then returns the UNIX prompt % to the user, indicating that it is waiting for further commands.

#### The Shell
The shell acts as an interface between the user and the kernel. When a user logs in, the login program checks the username and password, and then starts another program called the shell. The shell is a command line interpreter (CLI). It interprets the commands the user types in and arranges for them to be carried out. The commands are themselves programs: when they terminate, the shell gives the user another prompt (% on our systems). The adept user can customise his/her own shell, and users can use different shells on the same machine. The bash shell is now the default. The bash shell has certain features to help the user inputting commands, for example:

* Filename Tab Completion: By typing part of the name of a command, filename or directory and pressing the [Tab] key, the bash shell will complete the rest of the name automatically. If the shell finds more than one name beginning with those letters you have typed, it will beep, prompting you to type a few more letters before pressing the [Tab] key again.
* History: The shell keeps a list of the commands you have typed in. If you need to repeat a command, use the cursor keys to scroll up and down the list or type history for a list of previous commands.

[<button class="btn btn-warning">Try the Command Linx</button>](https://linuxcontainers.org/lxd/try-it/)


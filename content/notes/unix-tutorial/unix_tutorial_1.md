---
title: "Files in Unix"
linktitle: "Tutorial 1: Files in Unix"
draft: false
highlight_style: "github"
toc: true
type: docs
weight: 20

menu:
    unix-tutorials:
---

## Files and Processes

Everything in Unix is either a file or a process. A process is an executing program identified by a unique PID (process identifier). A file is a collection of data. They are created by users using text editors, running compilers etc. Examples of files:

* a document (report, essay etc.)
* the text of a program written in some high-level programming language
* instructions comprehensible directly to the machine and incomprehensible to a casual user, for example, a collection of binary digits (an executable or binary file)
* a directory, containing information about its contents, which may be a mixture of other directories (subdirectories) and ordinary files

### The Directory Structure

In Unix, folders are generally called _directories_.  Directories are arranged in a hierarchical structure, like an inverted tree. The top of the hierarchy is traditionally called `root`.

## Listing Files and Directories

### `ls` (list)

When you first log in, your current working directory is your _home directory_. Your home directory has the same name as your username, for example, `mst3k`, and it is where your personal files and subdirectories are saved. To find out what is in your home directory, type
```
% ls
``` 
This is short for "list."  Most Unix commands are two to four letters, sometimes with nonintuitive meanings.

The `ls` command lists the contents of your current working directory. There may be no files visible in your home directory, in which case the prompt will be returned. Alternatively, there may already be some files or folders created when your account was set up. 

In most Unix systems, `ls` does not report _hidden files_ by default.  Files or directories with names beginning with a dot (.) are hidden and usually contain important program configuration information. They are hidden because you should not change them unless you understand what you are doing.  To list all files in your home directory including those whose names begin with a dot, type
```
% ls -a
```
The -a is an example of a _command-line option_. The options change the behavior of the command. There are online manual pages that tell you which options a particular command can take, and how each option modifies the behavior of the command. 

## Making Directories

### `mkdir` (make directory)

We will now make a subdirectory in your home directory to hold the files you will be creating and using in the course of this tutorial. To make a subdirectory called unixstuff in your current working directory type
```
% mkdir unixstuff
```
To see the directory you have just created, type
```
% ls
```

## Changing to a Different Directory

### `cd` (change directory)

The command `cd _directory_` means change the current working directory to "directory". The current working directory may be thought of as the directory you are in, i.e. your current position in the directory tree. To change to the directory you have just made, type
```
% cd unixstuff
```

Type `ls` to see the directory's contents (it should be empty).

### Exercise 1A

Make another directory inside the unixstuff directory called backups.

## The Directories `.` and `..`

While still in the unixstuff directory, type
```
% ls -a
```
The unixstuff directory (and in all other directories) contains two special directories called (.) and (..). In Unix, (.) means the current directory, so typing
```
% cd .
```
means stay where you are (the unixstuff directory). This may not seem very useful at first, but using (.) as the name of the current directory will save a lot of typing, as we shall see later in the tutorial. (..) means the parent of the current directory, so typing
```
% cd ..
```
will take you one directory up the hierarchy. Try it now.

Note: there is a space between cd and the dot or double dot. Also note: typing cd with no argument always returns you to your home directory. This is very useful if you are lost in the file system.

## Pathnames

### `pwd` (print working directory)

Pathnames enable you to work out where you are in relation to the whole filesystem. For example, to find out the absolute pathname of your home directory, type `cd` to get back to your home directory and then type
```bash
% pwd
```
Most of the time you should see `/home/mst3k`.  On a multiuser central system like Rivanna, /home may be a _symbolic link_, i.e. an alias, for something else.  To see it, type
```bash
% pwd -P
```
The full pathname may look something like this: /sfs/qumulo/qhome/mst3k. 

### Exercise 1B

Use the commands `ls`, `pwd` and `cd` to explore the file system. (Remember, if you get lost, type cd with no argument to return to your home directory.)

## More About Home Directories and Pathnames

### Understanding Paths

First type cd to get back to your home directory, then type
```bash
% ls unixstuff
```
to list the contents of your unixstuff directory. Now type
```bash
% ls backups
```
You will get a message like this:
```
backups: No such file or directory
```
The reason is that "backups" is not in your current working directory. To use a command on a file (or directory) not in the current working directory (the directory you are currently in), you must either cd to the correct directory, or specify its full _pathname_. To list the contents of your backups directory, you must type
```bash
% ls unixstuff/backups
```
This path starts from the current location.  A path may be _absolute_ or _relative_.  An absolute path starts from the root location.  The absolute path is
```bash
ls /home/mst3k/unixstuff/backups
```

A relative path starts from the current working directory.  The special symbols `.` and `..` are often used for relative paths.    

### `~` (your home directory)

Home directories can also be referenced by the tilde `~` character. It can be used to specify paths starting at your home directory. So typing
```bash
% ls ~/unixstuff
```
will list the contents of your unixstuff directory, no matter where you currently are in the file system. What do you think
```bash 
% ls ~
```
would list? What do you think
```
% ls ~/..
```
would list?

## Getting Help

### Online Manuals

There are online manuals which give information about most commands. The manual pages tell you which options a particular command can take, and how each option modifies the behaviour of the command. Type `man` command to read the manual page for a particular command. For example, to find out more about the wc (word count) command, type
```bash
% man wc
```
In newer Linux systems, most commands have a `--help` option
```bash
% wc --help
```
However, `man` sends output through a [pager](/notes/unix-tutorials/unix_tutorial_2), whereas --help prints directly to the console.

Another useful command,
```
% whatis wc
```
gives a one-line description of the command, but omits any information about options, etc.

## Summary

| Command | Operation |
|---|---|
| `ls` | list files and directories |
| `ls -a` | list all files and directories |
| `mkdir` | make a directory |
| `cd directory` | change to directory |
| `cd` | change to home directory |
| `cd ~` | change to home directory |
| `cd ..` | change to parent directory |
| `pwd` | display the path of the current directory |
| `man` | display the manual pages for the specified command |
| `whatis` | display a description of the specified command |

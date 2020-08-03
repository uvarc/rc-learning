---
title: "Files in Unix"
linktitle: "Files in Unix"
draft: false
highlight_style: "github"
toc: true
type: docs
menu:
    unix-tutorials:
       parent: Unix Tutorial 1
       weight: 2
---

### Files and Processes
Everything in UNIX is either a file or a process. A process is an executing program identified by a unique PID (process identifier). A file is a collection of data. They are created by users using text editors, running compilers etc. Examples of files:

* a document (report, essay etc.)
* the text of a program written in some high-level programming language
* instructions comprehensible directly to the machine and incomprehensible to a casual user, for example, a collection of binary digits (an executable or binary file)
* a directory, containing information about its contents, which may be a mixture of other directories (subdirectories) and ordinary files

### The Directory Structure
All the files are grouped together in the directory structure. The file-system is arranged in a hierarchical structure, like an inverted tree. The top of the hierarchy is traditionally called `root`.

### 1.1 Listing Files and Directories

#### `ls` (list)

When you first login, your current working directory is your home directory.Your home directory has the same name as your user-name, for example, ee91ab, and it is where your personal files and subdirectories are saved. To find out what is in your home directory, type

```
% ls (short for list)
```

The ls command lists the contents of your current working directory. There may be no files visible in your home directory, in which case, the UNIX prompt will be returned. Alternatively, there may already be some files inserted by the System Administrator when your account was created. ls does not, in fact, cause all the files in your home directory to be listed, but only those ones whose name does not begin with a dot (.). Files beginning with a dot (.) are known as hidden files and usually contain important program configuration information. They are hidden because you should not change them unless you are very familiar with UNIX! To list all files in your home directory including those whose names begin with a dot, type

```
% ls -a
```

ls is an example of a command which can take options: -a is an example of an option. The options change the behaviour of the command. There are online manual pages that tell you which options a particular command can take, and how each option modifies the behaviour of the command. (See later in this tutorial for more on manual pages.)

### 1.2 Making Directories

#### `mkdir` (make directory)

We will now make a subdirectory in your home directory to hold the files you will be creating and using in the course of this tutorial. To make a subdirectory called unixstuff in your current working directory type

```
% mkdir unixstuff
```
To see the directory you have just created, type

```
% ls
```

### 1.3 Changing to a Different Directory

#### `cd` (change directory)

The command cd directory means change the current working directory to 'directory'. The current working directory may be thought of as the directory you are in, i.e. your current position in the file-system tree. To change to the directory you have just made, type

```
% cd unixstuff
```

Type ls to see the directory's contents (it should be empty).

#### Exercise 1A

Make another directory inside the unixstuff directory called backups.

### 1.4 The Directories `.` and `..`

While still in the unixstuff directory, type

```
% ls -a
```

As you can see, in the unixstuff directory (and in all other directories), there are two special directories called (.) and (..). In UNIX, (.) means the current directory, so typing

```
% cd .
```

means stay where you are (the unixstuff directory). This may not seem very useful at first, but using (.) as the name of the current directory will save a lot of typing, as we shall see later in the tutorial. (..) means the parent of the current directory, so typing

```
% cd ..
```

will take you one directory up the hierarchy (back to your home directory). Try it now.

Note: there is a space between cd and the dot or double dot. Also note: typing cd with no argument always returns you to your home directory. This is very useful if you are lost in the file system.

### 1.5 Pathnames

#### `pwd` (print working directory)

Pathnames enable you to work out where you are in relation to the whole filesystem. For example, to find out the absolute pathname of your home-directory, type cd to get back to your home-directory and then type

```
% pwd
```

The full pathname will look something like this: /a/fservb/fservb/fservb22/eebeng99/ee91ab which means that ee91ab (your home directory) is in the directory eebeng99 (the group directory),which is located on the fservb fileserver. Note: /a/fservb/fservb/fservb22/eebeng99/ee91ab can be shortened to /user/eebeng99/ee91ab

#### Exercise 1B

Use the commands `ls`, `pwd` and `cd` to explore the file system. (Remember, if you get lost, type cd by itself to return to your home directory.)

### 1.6 More About Home Directories and Pathnames

#### Understanding Pathnames

First type cd to get back to your home directory, then type

```
% ls unixstuff
```

to list the contents of your unixstuff directory. Now type

```
% ls backups
```

You will get a message like this:
```
backups: No such file or directory
```

The reason is, backups is not in your current working directory. To use a command on a file (or directory) not in the current working directory (the directory you are currently in), you must either cd to the correct directory, or specify its full pathname. To list the contents of your backups directory, you must type

```
% ls unixstuff/backups
```

#### `~` (your home directory)

Home directories can also be referred to by the tilde ~ character. It can be used to specify paths starting at your home directory. So typing

```
% ls ~/unixstuff
```

will list the contents of your unixstuff directory, no matter where you currently are in the file system. What do you think

```
% ls ~
```

would list? What do you think

```
% ls ~/..
```

would list?

### Summary
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


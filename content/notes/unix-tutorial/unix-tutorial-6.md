---
title: "Useful Commands"
linktitle: "Useful Commands"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
menu:
    unix-tutorials:
       parent: Unix Tutorial 6
       weight: 7
---

### Other Useful UNIX Commands

#### `exit`
Exit the current shell. If it is the login shell, this command logs the user off.

#### `which`
The `which` command indicates the path to the executable specified.

```
% which myexec
```

The which command returns the location of the executable according to the rules used to search paths. The shell always searches from left to right in the list contained in the PATH environment variable; the first executable of the specified name is the one that is used.

#### `wc`
The `wc` command returns the number of lines, words, and characters in an ASCII file. A word is defined as a non-zero length string surrounded by whitespace.

```
% wc myfile.txt
```

To print only the number of lines, use

```
% wc –l &lt myfile.txt &gt
```

#### `diff`
`diff` shows the differences between two ASCII files on a per-line basis.

```
% diff file1 file2
```

A few systems offer `xxdiff`. This is a graphical, and for human use far more useful, version of diff. It does require the ability to display X11 windows. It is available on the ITS cluster frontend.

#### `find`
`find` is an extremely powerful command with many options. The simplest and most common use of it is to search for a file of a given name or with a name containing a pattern.

```
% find . -name myscript.sh
```

This starts from current directory and searches for myscript.sh. The period is optional under Linux (but not under Mac OSX). To search for a name with a pattern it must typically be enclosed in quotes

```
% find . -name "*.sh"
```

See the manpage or examples online for more usage patterns of this command.

#### `du`
The `du` command outputs the number of kilobyes used by each subdirectory. Useful if you have gone over quota and you want to find out which directory has the most files. Some options make it more useful; in particular, -s summarizes directories and -h prints it in human-readable format. In your home directory, type

```
% du -s -h *
```

#### `gzip`
This reduces the size of a file, thus freeing valuable disk space. For example, type

```
% ls -l science.txt
```

and note the size of the file. Then to compress `science.txt`, type

```
% gzip science.txt
```
```

This will compress the file and place it in a file called science.txt.gz. To see the change in size, type ls -l again. To uncompress the file, use the gunzip command.

```
% gunzip science.txt.gz
```

#### `file`
`file` classifies the named files according to the type of data they contain, for example ASCII (text), pictures, compressed data, etc. To report on all files in your home directory, type

```
% file *
```

#### `cut`
The cut command extracts selected portions of a line, based on fields separated by a delimiter

```
% cut ­?d delim ­?fC1,C2,C3
```

Example:

```
% cut -d ' ' ?f1 /etc/resolve.conf
```
```
% cat myfile | cut -c 80
```

#### `sort`
This command sorts lines of a text file, based on command-­line options. The default is to sort alphabetically, based on lexigraphical ordering (in which e.g. 100 comes before 2).

```
% sort mylist.txt
```

#### `uniq`
Removes duplicate lines (file must be sorted first since it only compares lines pairwise).

```
% uniq mylist.txt
```

A frequent pattern is to pipe the output of sort into `uniq`

```
% sort animals | uniq
```

#### `history`
The bash shell keeps an ordered list of all the commands that you have entered. Each command is given a number according to the order it was entered.

```
% history (show command history list)
```

If you are using the `bash` or `tcsh` shell, you can use the exclamation character (`!`) to recall commands easily.


```
% !! (recall last command)

% !-3 (recall third most recent command)

% !5 (recall 5th command in list)

% !grep (recall last command starting with grep)
```

You can increase the size of the history buffer by typing

```
% set history=100
```

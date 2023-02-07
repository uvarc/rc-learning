---
title: "Other Useful Commands"
linktitle: "Tutorial 6: Other Useful Commands"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
weight: 70

menu:
    unix-tutorials:
---

### `exit`
Exit the current shell. If it is the login shell, this command logs the user off.

### `which`
The `which` command indicates the path to the executable specified.
```bash
% which myexec
```
The which command returns the location of the executable according to the rules used to search paths. The shell always searches from left to right in the list contained in the PATH environment variable; the first executable of the specified name is the one that is used.

### `wc`
The `wc` command returns the number of lines, words, and characters in an ASCII file. A word is defined as a non-zero length string surrounded by whitespace.
```
% wc myfile.txt
```

To print only the number of lines, use
```bash
% wc –l  myfile.txt 
```

### `diff`
`diff` shows the differences between two ASCII files on a per-line basis.
```bash
% diff file1 file2
```

### `find`
`find` is an extremely powerful command with many options. The simplest and most common use of it is to search for a file of a given name or with a name containing a pattern.
```bash
% find . -name myscript.sh
```
This starts from current directory (`.`) and searches for myscript.sh. The period is optional under Linux (but not under Mac OSX). To search for a name with a pattern it must typically be enclosed in quotes
```bash
% find . -name "*.sh"
```

See the manpage or examples online for more usage patterns of this command.

### `du`
The `du` command outputs the number of kilobyes used by each subdirectory. Useful if you have gone over quota and you want to find out which directory has the most files. Some options make it more useful; in particular, -s summarizes directories and -h prints it in human-readable format. In your home directory, type
```bash
% du -s -h *
```

### `gzip` and `zip`

This reduces the size of a file, thus freeing valuable disk space. For example, type
```bash
% ls -l science.txt
```
and note the size of the file. Then to compress `science.txt`, type
```bash
% gzip science.txt
```

This will compress the file and place it in a file called science.txt.gz. To see the change in size, type ls -l again. To uncompress the file, use the gunzip command.
```
% gunzip science.txt.gz
```

Most Linux systems also provide the standard `zip` and `unzip` commands.
```bash
% zip science.txt.zip
% unzip science.txt.zip
```

### tar (tape archive)

The standard archive format in Linux is `tar`.  Tar is usually used to bundle directories.  (Zip can also be used for this purpose.)  Typically the output file is compressed with gzip.
```bash
tar czf mydir.tar.gz mydir
```
The `c` option creates the tarfile (also called a "tarball").  The `f` option is for file, and this form of the command must be followed by the name of the file to contain the archive.  The `z` option gzips the file. 

To extract the files
```bash
tar xf mydir.tar.gz
```
Newer versions of `tar` can detect that the archive is zipped, so a `z` is not necessary for extraction.  The `x` option extracts.  This will create the directory `mydir` if it does not exist.  If it does, the contents will be replaced by the contents of mydir.tar.gz.

### `file`

`file` classifies the named files according to the type of data they contain, for example ASCII (text), pictures, compressed data, etc. To report on all files in your home directory, type
```bash
% file *
```

### `cut`

The cut command extracts selected portions of a line, based on fields separated by a delimiter
```
% cut­?d delim ­?fC1,C2,C3
```

Examples:
```bash
% cut -d ' ' ?f1 /etc/resolve.conf
% cat myfile | cut -c 80
```

### `sort`
This command sorts lines of a text file, based on command-­line options. The default is to sort alphabetically, based on lexigraphical ordering (in which e.g. 100 comes before 2).
```bash
% sort mylist.txt
```

### `uniq`

Removes duplicate lines (file must be sorted first since it only compares lines pairwise).
```bash
% uniq mylist.txt
```

A frequent pattern is to pipe the output of sort into `uniq`
```bash
% sort animals | uniq
```

### `history`

The bash shell keeps an ordered list of all the commands that you have entered. Each command is given a number according to the order it was entered.
```bash
% history (show command history list)
```
If you are using the `bash` or `tcsh` shell, you can use the exclamation character (`!`) to recall commands easily.

```bash
% !! (recall last command)
% !-3 (recall third most recent command)
% !5 (recall 5th command in list)
% !grep (recall last command starting with grep)
```
You can increase the size of the history buffer by typing
```
% set history=100
```

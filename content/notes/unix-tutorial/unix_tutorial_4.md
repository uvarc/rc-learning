---
title: "Wildcards and File Access"
linktitle: "Tutorial 4: Wildcards and File Access"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
weight: 50
date: 2023-12-11-14T00:11:14Z
menu:
    unix-tutorials:
---

## Wildcards

### The Characters `*` and `?`

The character `*` is called a wildcard, and will match against none or more character(s) in a file (or directory) name. For example, in your `unixstuff` directory, type

```bash
% ls list*
```
This will list all files in the current directory starting with list. Try typing
```
% ls *list
```
This will list all files in the current directory ending with list. The character ? will match exactly one character. So `ls` ?ouse will match files like house and mouse, but not grouse. Try typing
```bash
% ls ?list
```

## Filename Conventions

Unix regards a directory as merely a special type of file. So the rules and conventions for naming files apply also to directories. In naming files, characters with special meanings, such as `/` `*` & `%`, should be avoided. Also avoid using spaces within names. The safest way to name a file is to use only alphanumeric characters, that is, letters and numbers, together with `_` (underscore) and `.` (dot). File names conventionally start with a lower-case letter, and may end with a dot followed by a group of letters indicating the contents of the file. For example, all files consisting of C code may be named with the ending `.c`, for example, `prog1.c`. Then in order to list all files containing C code in your home directory, you need only type `ls *.c` in that directory.

Beware: some applications give the same name to all the output files they generate. For example, some compilers, unless given the appropriate option, produce compiled files named a.out. Should you forget to use that option, you are advised to rename the compiled file immediately, otherwise the next such file will overwrite it and the original file will be lost.

The Unix system itself ignores file suffixes.  You could name your C++ program `mycode.py` and Linux will not care.  However, many applications do care about file extensions.  A C++ compiler will expect the file to end in `.cpp` or `.cxx`. It will not recognize a file ending in `.py`, but a Python interpreter would.

## Filesystem Security (Access Rights)

In your unixstuff directory, type
```
% ls -l (l for long listing!)
```
You will see that you now get lots of details about the contents of your directory, similar to the example below.

{{< code file="/notes/unix-tutorial/snippets/ls-l.txt" lang="bash" >}}

Each file (and directory) has associated access rights, which may be found by typing ls -l.

In the left-hand column is a 10-symbol string consisting of the symbols `d`, `r`, `w`, `x`, `-`, and, occasionally, `s` or `S`. If d is present, it will be at the left hand end of the string and indicates a directory; otherwise - will be the starting symbol of the string. The nine remaining symbols indicate the permissions, or access rights, and are taken as three groups of three.

* the leftmost group of 3 gives the file permissions for the user that owns the file (or directory) (mst3k in the above example);
* the middle group gives the permissions for the group of people to whom the file (or directory) belongs (users in the above example);
* the rightmost group gives the permissions for all others.
The symbols r, w, etc., have slightly different meanings depending on whether they refer to a simple file or to a directory.

### Access Rights on Files
* `r` (or `-`), indicates read permission, that is, the presence or absence of permission to read and copy the file
* `w` (or `-`), indicates write permission, that is, the permission (or otherwise) to change a file
* `x` (or `-`), indicates execution permission, that is, the permission to execute a file, where appropriate

###  Access Rights on Directories
* `r` allows users to list files in the directory
* `w` means that users may delete files from the directory or move files into it
* `x` means the right to access files in the directory or to cd into it.

So, in order to read a file, you must have execute permission on the directory containing that file, and hence on any directory containing that directory as a subdirectory, and so on, up the tree.

####  Some Examples
| Permissions | Meaning |
|---|---|
| `-rwxrwxrwx` | a file that everyone can read, write and execute (and delete) |
| `-rw-------` | a file that only the owner can read and write: no one else can read or write and no one has execution rights (e.g., yourmailbox file) |

## Changing Access Rights

### `chmod` (changing a file mode)

Only the owner of a file can use chmod to change the permissions of a file. The options of chmod are as follows:

| Symbol | Meaning |
|---|---|
| `u` | user |
| `g` | group |
| `o` | other |
| `a` | all |
| `r` | read |
| `w` | write (and delete) |
| `x` | execute (and access directory) |
| `+` | add permission |
| `-` | take away permission |
For example, to remove read write and execute permissions on the file biglist for the group and others, type
```
% chmod go-rwx biglist
```
This will leave the other permissions unaffected. To give read and write permissions on the file biglist to all,
```
% chmod a+rw biglist
```

#### Exercise 4A
Try changing access permissions on the file science.txt and on the directory backups. Use ls -l to check that the permissions have changed.

### Shared Systems

Some multiuser systems, such as high-performance clusters, may not permit `chmod` on certain directories, or may automatically revert to the default set of permissions (called the _umask_).  This is for data protection and privacy for users.

### Summary

| Command | Operation |
|---|---|
| * | match any number of characters |
| ? | match one character |
| chmod | change file or directory permissions |

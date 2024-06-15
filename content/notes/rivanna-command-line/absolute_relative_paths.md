---
title: Absolute and Relative Paths
date: "2023-12-11T00:00:00"
type: docs 
weight: 760
menu: 
    rivanna-command-line:
---

Paths may be _absolute_ or _relative_.

An __absolute path__ * is path to a file or folder starting at the root. On Unix it will begins with `/`, to designate the root.

An absolute path is guaranteed to get you to the location you want.

A __relative path__ is the path to a file starting at the current location.  We use the notation `.` (a single period) for the current working directory, and `..` (two periods) for the parent of the current working directory.

**Examples**

Absolute paths:
```no-highlight
/home/mst3k/file.txt 
/home/mst3k/files/file.txt
/home/mst3k/projects/project1
/home/mst3k/projects/project1/output.txt
```
Note that `/home/mst3k/file.txt` and `/home/mst3k/files/file.txt` are _different files_ unless you explictly _link_ them.

Relative paths.  Suppose we are in the `/home/mst3k/files` folder.
```no-highlight
./file.txt		
../files/file.txt
```
The relative path to `file.txt` in `files` from the `project1` folder is
```no-highlight
../../files/file.txt
```

## Tilde Notation

In Unix the tilde `~` stands for the user's home directory, e.g. `/home/mst3k`.
```no-hightlight
ls ~
ls ~/files
```

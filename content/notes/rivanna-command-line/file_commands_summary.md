---
title: File Commands
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 800
menu: 
    rivanna-command-line:
---

## ls

`ls` list files in a directory.  
```bash
ls 
```
With no argument, listing the entire contents of the current working directory is assumed.

Like most Unix commands, it has many options.  They may be combined.

{{< table >}}
|  Option  |  Purpose |
|-------|-----|
|-l  | long listing, includes file date, size, and permissions |
|-a  | displays all files including hidden (dotfiles) |
|-h  | show file sizes in human readable terms |
|-C  | lay out listing in columns |
|-1  | (digit one) list one file per line, no header |
|-t  | show the newest files first |
|-r  | reverse the order |
|-F  |append a symbol to indicate the type of file (ordinary, executable, directory |
{{< /table >}}

```bash
ls -ltr ./projects
```

## cp

`cp` to copy a file.
```bash
cp file1.txt file2.txt
cp mymod.py ../projects/python_code
```

Commonly-used options:

{{< table >}}
|  Option  |  Purpose |
|-------|-----|
| -i    |  ask for confirmation before overwriting an existing file |
| -r    |  copy recursively all subdirectories|
| -n    |  "noclobber"; do not overwrite an existing file|
| -f    |  force an overwrite of an existing file|
{{< /table >}}

```bash
cp -r /share/resources/tutorials/rivanna-cl ~
```

## mv

`mv` to rename or _move_ a file.
```bash
mv file1.txt file2.txt
mv mymod.py ../projects/python_code
```

Options for `mv` are similar to `cp`.

## rm

`rm` to remove a file or directory

```bash
rm file1.txt 
rm file1.txt data.csv
```
Once the file is removed, it is gone and can no longer be accessed.

Options for `rm` are similar to `cp`.

{{< warning >}}
By default, the rm command _does not_ ask for confirmation before deleting a file! Use the `-i` option if you are unsure.
{{< /warning >}}

---
title: Directory Commands
date: "2023-12-11T00:00:00"
type: docs 
weight: 820
menu: 
    rivanna-command-line:
---

## cd

`cd` change directory
```bash
cd 
```
With no argument, `cd` moves to the user's home directory.  If you are hopelessly lost, you can always type `cd` and start over.

```bash
cd ../projects/project1
```

## mkdir

`mkdir` to create a directory.  Works relative to current working directory.
```bash
mkdir new_project
```
or
```bash
mkdir new_project
mkdir ~/projects/new_project/src
```

## rmdir

`rmdir` to remove an _empty_ directory.  Does not affect directories with content.
```bash
rmdir ~/projects/new_project/src
```

## rm -rf

`rm -rf` to force the removal of a directory and all its contents, including subdirectories.

```bash
rm -rf ~/projects/project1/src
```

## cp -r

Directories are copied with the `cp -r` command previously discussed.

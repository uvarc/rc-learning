---
title:  II - Files and Directories
date: 2023-12-11-14:11:14Z
type: docs
toc : true 
weight: 20
menu: 
    hpc-from-terminal:
---

## Files and Directories

**Files** store some sort of information: data, source code for scripts or programs, images, video, and so forth.

There are two basic types of files:
  * Text (documents, code)
  * Binary (images, executables)

The Unix shells ignore file extensions, but software might require certain extensions.  This includes compilers (`.cxx`, `.f90` and so forth), interpreters (`.py`, `.R`, etc.), image and video tools (`.png`, `.jpg`, `.mp4`). Since the format of the file extension depends on the expectations of software or on user preference, there is no rule limiting the number of characters, but most consist of one to three characters.

**Directories** are collections of files and other directories. Often they are called _folders_, but directory is generally the preferred (and historical) term in Unix.

Directories that are stored within another directory are _subdirectories_ of the parent.

Both files and directories have “metadata” associated with them such as name, timestamps, and permissions.

The names of Unix files and directories should contain _no spaces_. 
```bash
$ls 

'10. directories.md'
'15. streams.md'
```
The quotes indicate that a space is part of the file name.  While most modern Unix tools can handle spaces, the shell does not always do so, and special precautions must be taken to avoid suprises. For that reason, underscores or hyphens are preferred instead of spaces.

## Paths

In most operating systems, all files and directories are located with a **path**.  The “path” is the "full name" of every file & directory.

In a Unix-based operating system, the files are organized into a _tree_ structure (the method to store and organize the files is called **filesystem**, e.g. ext3,ext4, NTFS, etc).  The tree is "upside down" because the **root directory** is at the top, and directories branch off from there.  The root directory is indicated with a forward slash `/`.

{{< diagram >}}
graph TD
    C["/"] --> etc["etc"]
    C["/"] --> opt["opt"]
    C["/"] --> lib["lib"]
    C --> usr["usr"]
    C --> home["home"]
    C --> var["var"]
    
    home --> mst3k["mst3k"]
    home --> im4u["im4u"]

    mst3k-->myfile["myfile"]
    mst3k-->mydir["mydir"]

    mydir-->file1["file1"]
    mydir-->file2["file2"]

    usr--> bin["bin"]
    usr--> share["share"]
    usr--> local["local"]

    bin--> ls["ls"]
    bin--> pwd["pwd"]
    bin--> cd["cd"]
{{< /diagram >}}

The **home** directory is the usual name on Linux and similar Unix-based operating systems for the folder that holds user directories and files.  On MacOS it is called User.  On both, the _separator_ between branches of the tree is the forward slash.

{{< hl >}}
/home/mst3k/myfile
{{< /hl >}}
<br>
{{< hl >}}
/Users/Misty Tea/Documents/Homework.pages
{{< /hl >}}

Windows files and folders also have paths.  In Windows, drive letters or _volumes_ are the top-level folders, and usually there is more than one.  User files are in Users, similar to MacOS.

{{< hl >}}
C:\Users\Misty Tea\Documents\Homework.docx
{{< /hl >}}

## Absolute and Relative Paths

Paths may be _absolute_ or _relative_.

An __absolute path__ is path to a file or folder starting at the root. On Unix it will begins with `/`, to designate the root.

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

### Tilde Notation

In Unix the tilde `~` stands for the user's home directory, e.g. `/home/mst3k`.
```no-hightlight
ls ~
ls ~/files
```

## File Commands

### ls

`ls` lists files in a directory.  
```bash
$ls 
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
|-F  |append a symbol to indicate the type of file (ordinary, executable, directory) |
{{< /table >}}

```bash
$ls -ltr ./projects
```

### cp

`cp` to copy a file.
```bash
$cp file1.txt file2.txt
$cp mymod.py ../projects/python_code
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
$cp -r /share/resources/tutorials/rivanna-cl ~
```

### mv

`mv` to rename or _move_ a file.
```bash
$mv file1.txt file2.txt
$mv mymod.py ../projects/python_code
```

Options for `mv` are similar to `cp`.

### rm

`rm` to remove a file or directory

```bash
$rm file1.txt 
$rm file1.txt data.csv
```
Once the file is removed, it is gone and can no longer be accessed.

Options for `rm` are similar to `cp`.

{{< warning >}}
By default, the rm command _does not_ ask for confirmation before deleting a file! Use the `-i` option if you are unsure.
{{< /warning >}}

## Directory Commands

### cd

`cd` change directory
```bash
$cd 
```
With no argument, `cd` moves to the user's home directory.  If you are hopelessly lost, you can always type `cd` and start over.

```bash
$cd ../projects/project1
```

### mkdir

`mkdir` to create a directory.  Works relative to current working directory.
```bash
$mkdir new_project
```
or
```bash
$mkdir new_project
$mkdir ~/projects/new_project/src
```

### rmdir

`rmdir` to remove an _empty_ directory.  Does not affect directories with content.
```bash
$rmdir ~/projects/new_project/src
```

### rm -rf

`rm -rf` to force the removal of a directory and all its contents, including subdirectories.

```bash
$rm -rf ~/projects/project1/src
```

### cp -r

Directories are copied with the `cp -r` command previously discussed.

## Exercise

Start a terminal on a loginnode by whatever means you prefer. Type (the `$` is the prompt and is not typed):

```bash
$echo $SHELL
$pwd
$mkdir test_dir
$cd test_dir
$ls ..
$mkdir sub_dir
$ls
```

Q1: What is the full path of your home directory?
<details><summary>Solution</summary>

```bash
$cd ~
$pwd
```
</details>


Q2: Delete the `sub_dir` folder.
<details><summary>Solution</summary>

```bash
$rmdir ~/test_dir/sub_dir
```
</details>

Q3: Copy the directory
`/share/resources/tutorials/rivanna-cli/shakespeare` into your home directory.
<details><summary>Solution</summary>

```bash
$cp -r /share/resources/tutorials/rivanna-cli/shakespeare ~
```
</details>


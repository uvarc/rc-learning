---
title: File Permissions
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 860
menu: 
    rivanna-command-line:
---

Each file and directory has a set of _permissions_ associated with it. Traditionally, Unix has assigned permissions to the _owner_ (the user), the _group_ (a set of users), _others_ (users not the owner nor a member of the group that may have permissions on the file), and _all_ (all users on the system). These are generally denoted by `ugoa`.

Within each category, a user can have permission to _read_, _write_, or _execute_ the file.  These are denoted by `rwx`.

The command
```bash
ls -l
```
shows the permissions associated with each file.  Since in Unix a directory is a special type of file, the permissions are the same.

```bash
$ls -l
total 8
drwxr-xr-x. 2 mst3k mst3k 4096 Dec 18 10:39 data
drwxr-xr-x. 2 mst3k mst3k 4096 Dec 18 10:22 shakespeare

```
The output of `ls -l` is

{{< table >}}
| Permissions | Number of "Hard Links"  |  Owner |  Group | Size | Date and Time Last Modified | Name|
| ---- | ---- | -------- | ------ | ---- | ----------- | ---- |
| drwxr-xr-x | 2 | mst3k | mst3k | 4096 | Dec 18 10:39 | data |
{{< /table >}}

Permissions not granted to a user are indicated with a hyphen `-`.

The permissions layout in the first columns should be read as

{{< table >}}
| Type |  Owner   |  Group Owner |  Others |
| ---- | -------- | ------ | ---- |
|  d   |  rwx     |  r-x   |  r-x |
{{< /table >}}

In the above example, the `d` indicates the listing is a directory. A directory must have "execute" `x` permission in order for a user to enter it.

Listing the files in a directory will result in output such as
```bash
$ls -l shakespeare
-rwxr-xr-x. 1 mst3k mst3k     91 Dec 18 10:22 2col.txt
-rwxr-xr-x. 1 mst3k mst3k 173940 Dec 18 10:22 Hamlet.txt
-rwxr-xr-x. 1 mst3k mst3k 162563 Dec 18 10:22 HamletWords.txt
-rwxr-xr-x. 1 mst3k mst3k     34 Dec 18 10:22 k2sort.txt
-rwxr-xr-x. 1 mst3k mst3k 180857 Dec 18 10:22 Lear.txt
-rwxr-xr-x. 1 mst3k mst3k 154520 Dec 18 10:22 Othello.txt
-rwxr-xr-x. 1 mst3k mst3k  25628 Dec 18 10:22 uniques
-rwxr-xr-x. 1 mst3k mst3k  25628 Dec 18 10:22 uniques.txt
-rwxr-xr-x. 1 mst3k mst3k     72 Dec 18 10:22 wcdemo.txt
-rwxr-xr-x. 1 mst3k mst3k     26 Dec 18 10:22 words_and_num.txt
```
The hyphen as Type indicates an ordinary file.

## Changing Permissions

The owner of a file or directory may change the permissions with the `chmod` command.  Two formats are supported: a three- or four-digit code (in octal, i.e. base 8) indicating permissions, or the add/remove symbolic format.  The digit format is advanced so we will not discuss it; a reference is available [here](https://www.adminschoice.com/chmod-quick-referance-with-examples).  The symbolic format is more intuitive and mnemonic.  

**Examples** 

Add execute permission to a file for its owner. This is frequently used with shell scripts.
```bash
$chmod u+x ascript.sh
```

Add execute permissions for the owner and group members
```bash
$chmod ug+x ascript.sh
```

Allow others to read and write a file
```bash
$chmod o+wr myfile.txt
```

Please note that on multiuser, central systems such as an HPC cluster, the administrators may not allow individual users to change the permissions on certain file sets such as `home` and `scratch` directories, for reasons of data security and privacy.




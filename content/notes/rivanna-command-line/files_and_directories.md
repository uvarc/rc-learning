---
title: Files and Directories
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 700
menu: 
    rivanna-command-line:
---

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

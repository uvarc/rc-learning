---
title: Common Task File Archiving
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2250
menu: 
    bash-scripting:
---


Unix/Linux offers the tar command which can be used to bundle and compress many input files into a single archive file, a .tar file.

__Create .tar  file__

tar -czvf file.tar inputfile1 inputfile2

-c 	create archive

-f	archive is a file

-v	verbose output

-z	compress

__Unpack .tar file__

tar –xzvf file.tar

-x	extract/unpack archive

Open your terminal window

Change current directory to  ~/scripts

Execute tar czvf script_archive.tar *.sh

Execute ls, verify that there is a new file archive.tar


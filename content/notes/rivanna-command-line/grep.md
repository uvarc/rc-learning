---
title: Finding and Searching Files
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1360
menu: 
    rivanna-command-line:
---

## Searching with grep

The `grep` command  is commonly used in UNIX to filter a file or input, line by line, against a pattern.  Patterns can be complex and use _regular expressions_, but most of the time wildcards are sufficient.

```no-highlight
grep [OPTIONS] PATTERN FILENAME
```
**Example**

The `-i` option stands for "ignore case."
```bash
$grep -i Unix intro_basic-unix.txt
```

Grep is frequently used with wildcards and pipes.
```bash
$grep -i write *f90
$grep weight: *md | grep 100
```

**Example** 

How many sequences are in a FASTA-formatted file? Each sequence record in a FASTA file has one line of description that always starts with `>`, followed by multiple lines of the sequence itself. Each sequence record ends when the next line starts with `>`.

```bash
$grep -c '>' sequences.fasta
```
The `-c` option returns the number of lines containing the pattern. Please be sure to include the quotes around the `>` or the shell will interpret it as a redirection.  

**A Handy Trick**

To find all occurrences of a pattern in all files in a directory, use `grep -r`.
```bash
$grep -r "print" python_programs
```
Be careful with the pattern for a recursive search, or the output can be excessive.

## Finding Files by Name

The `find` command can locate a file if you cannot remember its directory.  It can take wildcards, in which case it is best to use quotes around the name pattern.

```bash
$find . -name 2col.txt
./shakespeare/2col.txt
$find . -name "people*"
./data/people.txt
```
The period `.` tells find to start at the current working directory.

Find has many options to locate files by name, type, date, and others.  See [here](https://www.tecmint.com/35-practical-examples-of-linux-find-command/) for examples.


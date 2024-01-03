---
title: Standard Stream Redirection
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1300
menu: 
    rivanna-command-line:
---

The standard streams are normally associated with the console. The command will print to the terminal, or will read input typed into the terminal.  Stdin and stout can also be _redirected_ to write to or read from a file.

Redirect standard input with `<`
```bash
$./mycode < params.txt
```

Redirect standard output with `>`
```bash
$ls –l > filelist.txt 
```

If the file exists, `>` will overwrite it.  Append with `>>`.
```bash
$cat file1 >> bigfile.csv
```

Redirection of standard error depends on the shell and is needed for only a few commands.

For bash
```bash
$make >& make.out
```
redirects both stdout and stderr from the `make` command to `make.out`.


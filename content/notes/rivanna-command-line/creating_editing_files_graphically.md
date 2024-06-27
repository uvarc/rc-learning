---
title: Creating and Editing Files
date: 2023-12-11T00:00:00
type: docs 
weight: 900
menu: 
    rivanna-command-line:
---

To create files we use a _text editor_.  Do not use a word processor such as LibreOffice.

## Graphical Options

Graphical editors must be used from within a graphical environment.  On Linux the standard graphical windowing system is called **X11**.  Newer Linux versions provide some form of "desktop" environment similar to Windows or MacOS on top of X11.  On our system we provide the [MATE](https://mate-desktop.org/) Desktop Environment.  It can be accessed from [FastX](https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/) on a frontend.  It can also be started on a compute node from the [Open OnDemand](https://www.rc.virginia.edu/userinfo/rivanna/ood/overview/) Desktop Interactive Application.

### gedit/pluma

Modern Linux systems provide at least one graphical text editor. One option is `gedit`.  On the MATE Desktop used on our system, the equivalent is `pluma`, but the `gedit` command starts pluma.

Gedit/pluma is very similar to Notepad on Windows.

### VS Code

This is accessed by a [module](/notes/rivanna-command-line/modules).
```bash
$module load code-server
```

Then open a browser (Firefox is the default on MATE) and go to the URL `http://127.0.0.1:8080`.


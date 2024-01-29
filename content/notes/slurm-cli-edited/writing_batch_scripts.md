---
title: Writing Batch Scripts
date: 2023-12-11-14:11:14Z
type: docs 
weight: 600
menu: 
    slurm-from-cli:
---

Batch scripts should be written on a cluster frontend.  Please do not use your local computer to write them, as they may not work.  You must also use a _text editor_ and not a word-processing program.

Several options are available to prepare batch scripts.  

### Graphical Editors

You can log in to a [FastX](https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/), which provides a [MATE](https://mate-desktop.org/) desktop environment. One of the tools is a graphical editor very similar to Notepad.  It is called `pluma` by MATE, but we have made it available as `gedit` if started from a terminal.  If you wish to start it from a menu, it is available from Applications&rarr;Accessories.

You can also use [Open OnDemand's](https://www.rc.virginia.edu/userinfo/rivanna/ood/overview/) built-in file manager and editor.  Create a new file from the Files menu.  Select the file and choose `Edit` from the three-dot dropdown menu to the right of the file name.  This will open a very basic text editor.

### Command-Line Editors

Editors available at the command line are [nano](https://www.nano-editor.org/), [vim](https://www.vim.org/), and [emacs](https://www.gnu.org/software/emacs/).  Nano is a simple text-only editor.  Vim is also available text-only from a command line, but a graphical version called `gvim` can be invoked from a MATE Desktop through the Applications&rarr;Accessories menu. Emacs can also be started from the Accessories menu but, if a graphical environment, will start a graphical user interface.  If invoked within a text-only environment, it will fall back to a text interface.

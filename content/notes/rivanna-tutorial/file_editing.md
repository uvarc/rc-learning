---
title: Editing Files
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 465

menu:
  rivanna-tutorial:
    parent: Working with Files
---

Once we have our files on Rivanna, we may need to edit them.  It is a good idea to edit your files directly on Rivanna, rather than editing on your local computer and then transferring them back and forth.

You can create files by the same process as editing an existing one; just select `New` if there is a menu.

You can use:
* The built-in editor in Open OnDemand. Click on Files on the Dashboard, highlight the file that you want to edit. From the dropdown menu next to the file name, select Edit.  A simple editor will open. To create a file, navigate to the desired location, click the New File button, then edit that file.
* If logged in through FastX, you can use the `pluma` editor, which is accessible through the Applications->Accessories menu.  You can also start it from a terminal with either its name `pluma` or as `gedit` (those are the same program).
* The MATE desktop in FastX also provides the semi-graphical editors `Emacs` and `GVim` in the same menu.
* In FastX, you can also use a programmer's interface such as VSCode, Spyder, or Rstudio.  For extensive editing or running programs through environments such as VSCode, use the Open OnDemand [interactive app](notes/rivanna-tutorial/interactive).
* When in a Terminal, through ssh or FastX, use a text editor such as vi or nano.
    * vi uses keyboard commands and may take a while to learn.
    * nano is a very basic editor.  

To launch vi or nano, type
```bash
vi myfile.txt
```
or
```bash
nano myfile.txt
```
{{< info >}}
Nano is fairly self-explanatory but documentation is [here](https://www.nano-editor.org/).  On Rivanna `vi` is equated to `vim` which has documentation [here](https://www.vim.org/).  GVim is an editor built on top of vim with some friendlier features, such as easier navigation.  Vi can be used from a text-based terminal, including the Open OnDemand terminal app.
{{< /info >}}

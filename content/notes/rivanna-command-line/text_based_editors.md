---
title: Text-Based File Editors
date: 2023-12-11-14:11:14Z
type: docs 
weight: 910
menu: 
    rivanna-command-line:
---

Regular Linux users are advised to learn at least one text-based editor, in case a graphical interface is not available or is too slow.

### vi (vim)

The oldest and most widely available text-based editor on Unix is _vi_ for "visual." Modern versions are nearly all based on [vim](https://vim.org) ("vi improved).  On Rivanna we generally _alias_ the `vi` command to `vim`.

Vim/vi is used entirely through keyboard commands which must be memorized.  The mouse is not utilized.  Only a few commands are needed to be able to do basic editing and they are not difficult to learn.  A beginner tutorial is [here](https://www.tutorialspoint.com/vim/index.htm). One stumbling block for new users is that vim has _command mode_ and _insert mode_ and it must be toggled between the two.

Basics:
  * To enter the insert mode press  `i`
  * To enter the command mode press  `ESC`
  * To save the file enter  `:w`
  * To save under a new name  `:w filename`
  * To exit `:q`

MATE also provides a graphical interface called `gvim`. It can be accessed through the Applications->Accessories menu as Vi IMproved.  GVim combines the keyboard-oriented commands of vim with some mouse-based capabilities.

### nano

Nano is simple text editor that is fairly self-explanatory and simple to learn. It is always in insert mode. A tutorial similar to that for vim above is [here](https://www.tutorialspoint.com/how-to-use-nano-text-editor).

Basics:
  * Immediately start typing
  * To exit:  __control+X__
  * Type the filename and press  __Enter__

### Emacs

The Emacs editor is another long-time editor on Unix. Similar to gvim, it combines a graphical interface, when available, with a keyboard-based command system.  If the graphical interface is not available, such as from an Open OnDemand terminal, it will fall back to a text-only interface. Emacs is powerful and complex.  The documentation is [here](https://www.gnu.org/software/emacs/tour/).

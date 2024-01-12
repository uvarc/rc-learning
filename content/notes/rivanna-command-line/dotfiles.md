---
title: Dotfiles
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1590
menu: 
    rivanna-command-line:
---

“Dotfiles” are files that describe resources to programs that look for them.  They begin with a period or “dot” (hence the name).  In general, they are used for configuration of various software packages.  

Dotfiles are hidden from `ls`, but `ls -a` shows them.  Sometimes `ls` is _aliased_ to ls –a.

Bash has configuration dotfiles: `.bash_profile` and `.bashrc`.
  * if no .bash_profile is present it will read .profile
  * .bash_profile is sourced only for login shell
  * the default .bash_profile on our HPC system incorporates the .bashrc; otherwise on a general Linux system, .bashrc is not run for a login shell.

Dot "files" may also be, and often are, directories.

```bash
$ls -a
.   .bash_logout   .bashrc  .lesshst  shakespeare  .Xauthority
..  .bash_profile  data     .mozilla  .ssh
```

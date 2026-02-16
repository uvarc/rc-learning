---
title: "Environment Variables"
linktitle: "Tutorial 7: Environment Variables"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
weight: 80
date: 2023-12-11T00:00:00Z
menu:
    unix-tutorials:
---

Variables are a way of passing information from the shell to programs when you run them. Programs look "in the environment" for particular variables and if they are found will use the values stored. Some are set by the system, others by you, yet others by the shell, or any program that loads another program. Standard Unix variables are split into two categories, _environment variables_ and _shell variables_. In broad terms, shell variables apply only to the current instance of the shell and are used to set short-term working conditions. Environment variables are exported and have a farther reaching significance; those set at login are valid for the duration of the session. By convention, environment variables are written in UPPERCASE while shell variables usually have lowercase names.

## Environment Variables

An example of an environment variable is the `$SHELL` variable. The value of this is the current shell you are using. Type
```bash
% echo $SHELL
```

More examples of environment variables are
```bash
$USER (your login name)
$HOME (the path name of your home directory)
$PWD (current working directory)
$DISPLAY (the name of the computer screen to display X windows; only set if X is enabled)
$PATH (the directories the shell should search to find a command)
```

## Using and Setting Variables

Environment variables are set using the `export` command (`bash` or `zsh`) or the `setenv` command (`tcsh` or `csh`), displayed using the `printenv` (`bash`, `tcsh`) or `env` (`bash`, `zsh`) commands, and unset using the `unsetenv` command. To show all values of these variables, type

```bash
% printenv | more
```

To set a value of an environment variable, type (for `bash`)

```bash
% export VAR=value
```

## Sourcing

A group of shell commands can be placed into a file and then _sourced_.  When a file is sourced, the commands are executed as if they had been typed at the command line in the current shell.  For example, if several environment variables needed to be set over and over again, they could be collected into a file such as this simple script called `envs.sh`:

{{< code-download file="/notes/unix-tutorial/snippets/envs.sh" lang="bash" >}}

## Exercise 6A

Download the `envs.sh` file to the Unix system you are using.  Run the command
```bash
source envs.sh
```

Print the values of the environment variables in the file.  To print the value of the shell variable ncpus, type
```bash
echo $ncpus
```

## Dotfiles

Each time you log in to a Unix host, the system looks in your home directory for initialization files. Information in these files is used to set up your working environment. The first initialization file sourced is the _login_ initialization. It is sourced only in the login shell.  Note: modern "desktop" user interfaces tend to "swallow" the login setup file, and it may be difficult to determine what is happening in these cases if there is an error.

At login the bash shell first sources .bash_profile or .profile (if .bash_profile exists .profile will be ignored). Child shells source .bashrc.  The `zsh` sources `.zprofile` and child shells source `.zshrc`. Two older shells, csh (C shell) and tcsh, read `.login` for login shells and `.cshrc` or `.tcshrc` for all other shells. 

Note that all these file names begin with periods or "dots"; hence they are called _dotfiles_.  As we have learned, dotfiles are hidden and will only be visible with `ls -a`. 

The `.bash_profile`, `.profile`, or `.login` is to set conditions which will apply to the whole session and/or to perform actions that are relevant only at login.  The `.bashrc`, `.zshrc`, or `.tcshrc` file is used to set conditions and perform actions specific to the shell and to each invocation of it. The rc stands for resource; many Unix dotfiles use this convention to set resources.

If you wish for your login shell to source the .bashrc also, add the lines
```bash
if [ -f ~/.bashrc ];
  then . ~/.bashrc
fi
```
to the `.bash_profile` script. 

Warning: NEVER put commands that run graphical displays (e.g. web browsers) in your dotfiles. If you change your `.bashrc` you can force the shell to reread it by using the shell source command.

```
% source ~/.bashrc
```

## Setting the Path

When you type a command, your path (or `$PATH`) variable defines in which directories the shell will look to find the command you typed. If the system returns a message saying "`command: Command not found`", this indicates that either the command doesn't exist at all on the system or it is simply not in your path. 

For example, suppose you have installed a program called "units" into your home directory in a folder called `units174`.  Units is a simple utility that can convert Imperial to metric and vice versa, from SI to cgi, and so forth.  This folder contains a `bin` subdirectory in which the executable is located.  To run units, you must either directly specify the units path (`~/units174/bin/units`), or you need to have the directory `~/units174/bin` in your path. You can add it to the end of your existing path (the `$PATH` represents this) by issuing the command:
```bash
% export PATH=$PATH:$HOME/units174/bin
```

If you have `units` installed you can test that this worked by trying to run units in any directory other than where `units` is actually located.
```
% cd; units
```
Hint: You can run multiple commands on one line by separating them with a semicolon. 

To add this path permanently, add the `export` line to your `.bashrc` file after the list of other commands. Make sure that you include the `$PATH` when you reset it, or you will lose access to basic system commands!

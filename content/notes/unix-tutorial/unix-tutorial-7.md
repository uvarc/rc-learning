---
title: "Environment Variables"
linktitle: "Environment Variables"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
menu:
    unix-tutorials:
       parent: Unix Tutorial 7
       weight: 8
---

### 7.1 UNIX Variables
Variables are a way of passing information from the shell to programs when you run them. Programs look "in the environment" for particular variables and if they are found will use the values stored. Some are set by the system, others by you, yet others by the shell, or any program that loads another program. Standard UNIX variables are split into two categories, environment variables and shell variables. In broad terms, shell variables apply only to the current instance of the shell and are used to set short-term working conditions; environment variables are exported and have a farther reaching significance; those set at login are valid for the duration of the session. By convention, environment variables are written in UPPERCASE while shell variables usually have lowercase names.

### 7.2 Environment Variables
An example of an environment variable is the `$SHELL` variable. The value of this is the current shell you are using. Type

```
% echo $SHELL
```

More examples of environment variables are

```
$USER (your login name)
$HOME (the path name of your home directory)
$PWD (current working directory)
$DISPLAY (the name of the computer screen to display X windows; only set if X is enabled)
$PATH (the directories the shell should search to find a command)
```

#### Finding Out the Current Values of These Variables

Environment variables are set using the `export` command (`bash` or `ksh`) or the `setenv` command (`tcsh` or `csh`), displayed using the `printenv` (`bash`, `tcsh`) or `env` (`bash`, `ksh`) commands, and unset using the `unsetenv` command. To show all values of these variables, type

```
% printenv | less
```

To set a value of an environment variable, type (for `bash`)

```
% export VAR=value
```

### 7.3 Using and Setting Variables
#### Dotfiles
Each time you login to a UNIX host, the system looks in your home directory for initialisation files. Information in these files is used to set up your working environment. The bash shell uses files called .profile, .bash_profile, and .bashrc, whereas ksh uses .profile and .kshrc, and the the C and TC shells use files called .login and .cshrc or .tcshrc (note that all these file names begin with periods or dots; hence they are called dotfiles). At login the bash shell first sources .bash_profile or .profile (if .bash_profile exists .profile will be ignored). Child shells source .bashrc.  If you wish for your login shell to source the .bashrc also, add the lines

```
if [ -f ~/.bashrc ];
  then . ~/.bashrc
fi
```

to the `.bash_profile` script. The `ksh` reads `.profile` and child shells source `.kshrc`. The C shell reads `.login` for login shells and `.tcshrc` for all other shells. The `.bash_profile`, `.profile`, or `.login` is to set conditions which will apply to the whole session and/or to perform actions that are relevant only at login.  The `.bashrc`, `.kshrc`, or `.cshrc` file is used to set conditions and perform actions specific to the shell and to each invocation of it. The rc stands for resource; many UNIX dotfiles use this convention to set resources.

Warning: NEVER put commands that run graphical displays (e.g. web browsers) in your dotfiles. If you change your `.bashrc` you can force the shell to reread it by using the shell source command.

```
% source ~/.bashrc
```

### 7.4 Setting the Path
When you type a command, your path (or `$PATH`) variable defines in which directories the shell will look to find the command you typed. If the system returns a message saying "`command: Command not found`", this indicates that either the command doesn't exist at all on the system or it is simply not in your path. For example, to run units, you either need to directly specify the units path (`~/units174/bin/units`), or you need to have the directory `~/units174/bin` in your path. You can add it to the end of your existing path (the `$PATH` represents this) by issuing the command:
```
% export PATH=$PATH:$HOME/units174/bin
```

If you have `units` installed you can test that this worked by trying to run units in any directory other than where units is actually located.

```
% cd; units
```

Hint: You can run multiple commands on one line by separating them with a semicolon. To add this path permanently, add the line to your `.bashrc` file after the list of other commands. Make sure that you include the `$PATH` when you reset it, or you will lose access to basic system commands!

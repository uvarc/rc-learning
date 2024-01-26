---
title: IV - I/O, Pipes and Processes
date: 2023-12-11-14:11:14Z
type: docs
toc: true 
weight: 40
menu: 
    hpc-from-terminal:
---

## Wildcards

**Wildcards** are special characters that can stand for strings.  Wildcards enable you to work with groups of files without needing to type multiple files with similar names. 

The asterisk `*` can replace zero to unlimited characters except for a leading period.

The question mark `?` replaces exactly one character.  

**Examples**
```bash
$ls *.py
$cd array_test
$ls input?.dat
$ls input??.dat
$ls input*.dat
$rm list?.sh
```

{{< warning >}}
BE CAREFUL when using wildcards with rm! Gone is gone! On some systems there may be backups, or there may not be, and on your personal system you would need to set up backups and learn to retrieve files. It is advisable to first run an `ls` with the pattern you plan to use with `rm`.
{{< /warning >}}


## Standard Streams

Each executable has associated with it three input/output streams:  __standard input__ ,  __standard error__ , and  __standard output__.  Normally these streams come from or go to your console (i.e. your terminal).

Most Unix commands read from standard input and/or write to standard output.

These I/O streams are often represented as  __stdin__,  __stderr__, and  __stdout__.

The Unix commands we have studied so far all write to standard output.

```bash
$ls -l
```
produces output to the terminal.

## Standard Stream Redirection

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

## Pipes

One of the most powerful properties of Unix is that you can  __pipe__  the standard output of one command into the standard input of another.

The pipe symbol `|` is above the backslash on most US keyboards.  Pipes can be chained indefinitely, though most uses need just one.
```no-highlight
cmd1 | cmd2 | cmd3
```

**Example**

Commands such as `ls` have lengthy manpages that will tend to scroll off our terminal window.
```bash 
$man ls | more
```
Now we can page through the listing.

## Finding and Searching Files

### Searching with grep

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

### Finding Files by Name

The `find` command can locate a file if you cannot remember its directory.  It can take wildcards, in which case it is best to use quotes around the name pattern.

```bash
$find . -name 2col.txt
./shakespeare/2col.txt
$find . -name "people*"
./data/people.txt
```
The period `.` tells find to start at the current working directory.

Find has many options to locate files by name, type, date, and others.  See [here](https://www.tecmint.com/35-practical-examples-of-linux-find-command/) for examples.

## Running Executables

Executables are often also called _binaries_. The terms are synonymous in most cases.

The shell has a predefined _search path_. It will look in a sequence of directories for an executable with the specified name, and will invoke the first one it encounters.  If the executable is in this search path, you can simply type its name at the prompt.

```bash
$gedit hello_world.sh
```

here `gedit` is the name of the executable.  Its actual location is /usr/bin/gedit, but /usr/bin is in the default search path.

If the location of the binary is not in your search path, you must type the path to the executable (it can be absolute or relative)

```bash
$./hello_world.sh
```

For security reasons the current directory is not in your default search path. Hence you must explicitly provide the `./` path to execute a binary in the current directory.  If you wish to add it, type (for bash)
```bash
$export PATH=$PATH:.
```
`PATH` is called an _environment variable_. It holds the list of paths to be searched by the shell.  In this example it is essential to add the first `$PATH` or you will lose the default path set by the system.

If you are unsure of the path to the binary you wish to run, the `which` command will tell you the path to the binary the shell will start.
```bash
$which g++
/apps/software/standard/core/gcc/11.4.0/bin/g++
```
## Process Control

A running executable is a _process_ to the Unix operating system. When it is run at a command line, a process can be running in the  _foreground_, which suppresses a prompt, or in the  _background_, which returns a prompt to the shell.  

To start in the background, add an ampersand `&` at the end of the command.
```bash
$./myexec -o myopt myfile&
```

### Managing Processes

The `jobs` command lists your running jobs (processes) with their job index.

The key combination control-z (ctrl-z or ^z) suspends the foreground job. To resume the job in the background, type `bg`.

This can be combined with output from `jobs`
```bash
$bg %1	# place the job number 1 into the background
$fg %4	# place the job number 4 back to the foreground
```

For more general information about processes, use `ps` (process status) The `-u` option limits it to processes owned by user `mst3k`.
```bash
$ps -u mst3k
   PID TTY          TIME CMD
498571 ?        00:00:00 systemd
498575 ?        00:00:00 (sd-pam)
498581 ?        00:00:00 pulseaudio
498582 ?        00:00:00 sshd
498593 pts/3    00:00:00 bash
498665 ?        00:00:00 dbus-daemon
498670 ?        00:00:00 dbus-daemon
498672 ?        00:00:00 dbus-kill-proce
498677 ?        00:00:00 gio
498685 ?        00:00:00 gvfsd
498691 ?        00:00:00 gvfsd-fuse
517189 pts/3    00:00:00 ps
```

The `pid` is the _process id_.  


### Killing Processes

You have accidentally started a production job on a loginnode node.  What to do?

You can kill your forground process with `Crtl c`. 
```bash
#oops, I was supposed to run this through Slurm
$./myexe  
^c
```

If you need to kill a background process, you can use `jobs` to locate and foreground it.  You may also have processes that don't appear with `jobs`.  Use `ps` to find the PID, then
```bash
$kill -9 <pid>
```
Do not type the angle brackets, just the number. Many processes will ignore the kill command without the `-9` option so we routinely include it.

To kill by executable name
```bash
$killall -9 <executable name> 
```
The kill command with -9 immediately kills the process without allowing the process to clean up or save data. The killall command can be used to kill all of the processes that match a specific name or pattern.

If you find yourself in a jam and do not know what is wrong and you must start over, 
```bash
$kill -9 -1
```
kills **all** your processes, including your login.

## Dotfiles

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

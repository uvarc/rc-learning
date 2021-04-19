---
title: "Using Rivanna from the Command Line"
authors: [uvarc]
slides:
  theme: white
  highlight_style: github
---

# Unix

__UNIX__ is an operating system originally developed at Bell Laboratories in the 1960s. Linux is a version of Unix; MacOS is derived from another version. 

- Linux is the dominate operating system for high-performance computing.

- Graphical User Interfaces have been developed for Unix
   - Linux and others use the _X11 Window System_.
   - MacOS calls its native window system _Quartz_. [XQuartz](https://xquartz.org) is a version of X11 for Macs.

---

# Rivanna

_Rivanna_ is the university's primary resource for high-performance computation. It provides a platform for computationally-intensive research across disciplines.

---

# Logging In

- Logging into a remote UNIX based system requires a client
- Your options for the client depends on your operating system.
- Command line access is based on the “SSH” or Secure Shell protocol
  - Encrypted
  - Used on most UNIX systems
  - Variety of clients for all platforms

- When off Grounds, you must be connected to the VPN

<img src="img/UsingRivannaCommandLine4.png" width=228px />

---

# OOD Shell Access

[https://www.rc.virginia.edu/userinfo/rivanna/ood/overview/](https://www.rc.virginia.edu/userinfo/rivanna/ood/overview/)

- Access through Netbadge (does not require VPN) at the [portal](https://rivanna-portal.hpc.virginia.edu)

- From the Dashboard go to the menu Clusters->Rivanna

---

# FastX

FastX is a remote desktop application.

- Provides a [MATE](https://mate-desktop.org) desktop environment.

- Available from a [Web browser](https://rivanna-desktop.hpc.virginia.edu).

- Menu 
   

---

# Windows

- We recommend [MobaXterm](https://mobaxterm.mobatek.net/).  Download the Home Edition, Installer Edition.

- From the Sessions menu, start an _SSH_ session. Using one of the particular names for a rivanna frontend (rivanna1.hpc.virginia.edu, etc.). as the host name.

- If you prefer to type an ssh command, start a local terminal session and enter the same command as is used in the Mac OS or Linux terminal (the -Y option is the default).

- MobaXterm includes an X server for graphics, a simple editor, and other useful tools in addition to ssh and sftp/scp functions.

---

# Mac and Linux

- Mac OSX and Linux ship with a Terminal app.
    - Mac users should also install [XQuartz](https://www.xquartz.org/).

- Open Finder and Go to Applications

<img src="img/UsingRivannaCommandLine14.png" width=500px />

   - Utilities > Terminal app

<img src="img/UsingRivannaCommandLine15.png" width=500px />

Connect using
```
ssh -Y mst3k@rivanna.hpc.virginia.edu
```
Replace `mst3k` with your user ID.

---

# The Shell

- Direct interaction with Unix is through a _shell_.  

- The shell is a _program that interprets commands_ issued to the operating system.
   - The default shell on Linux is `bash` (the Bourne Again Shell).
   - MacOS formerly used bash but has switched to `zsh` as its default.

- You will have a __prompt__ which indicates that the shell is ready to accept commands.
   - A default prompt is set by the system
      - On rivanna the default is `bash-4.2$` (the number is the version)
   - The user can change the individual prompt
      - In our examples we will use `$`

---

# Shell Commands

- To determine your shell, at the prompt type
```bash
echo $shell
```
- Unix in general and the shell in particular is __case-sensitive__ .
   - `ls` and `LS` are different.
- The syntax of commands is not completely standardized but in general is
  - `cmd -o --opts <filename>`
- The pattern is a two or three-letter abbreviation, single-letter options preceded by one hyphen, multiple-letter options with two hyphens, and arguments at the end.
  - Angle brackets indicate an _option_ and are not typed.

---

# Navigating in the Shell

Let’s run our first command

- Assume a prompt symbol before each command
```
pwd
```
_p_ rint  _w_ orking  _d_ irectory
prints working (current) directory

```
/home/mst3k
```

Know where you are!

---

# Bash History Mechanism

* When using bash you may use arrow keys to save yourself some keystrokes.
  * Up arrow: scroll through the previous commands you have typed
  * Down arrow: if scrolled back, scroll to more recent commands
  * Left/right arrows: edit text on a line

---

# More Bash Goodies

* Tab completion
  * `string<tab>` causes bash to expand `string` as far as it has a unique name.
* Search for earlier command
  * control-r  <text>
* Move to the beginning of the line
  * control-a
* Move to the end of the line
  * control-e
* Clear the screen
  * `clear` or control-l

---

# Files and Directories

- A file stores some form of information.
  - There are two basic types of files:
     - Text (documents, code)
     - Binary (images, executables)
  - Unix does not pay attention to file extensions, but an application might.

- Directories are collections of files 
  - Usually called a "folder" in other environments 
  - Directories beneath another one are _subdirectories_ of the parent.

- Both files and directories contain metadata
  - Name, time of last access, permissions
  - Often the file type (text, png)

---

# Paths

In UNIX all files and directories have a “path”
- At the top is the __root directory__ symbolized by `/`.
- The path is the "full name" of every file and directory."

---

# Absolute Paths

A path that starts with `/` is an _absolute_ path.
Examples:
```
/
/home/mst3k  
/home/mst3k/rivanna-cl_01302  
/home/mst3k/rivanna-cl_013020/basic_commands
```
An absolute path will always lead to the location.
A tilde `~` stands for `/home` so alternatives to the above are
```
~mst3k
~mst3k/rivanna-cl_01302
```

---

# Relative Paths

The current directory can be represented by a period (.) Thus if we are in
`/home/mst3k/rivanna-cl/basic_commands` we can type
```
gedit ./hello_world.sh
./hello_world.sh
```

The parent directory is symbolized by two periods (..).

```
gedit ../scripts/script.sh
```

Relative paths are defined by their relationship to the _current_ directory.
Be sure you are starting in the correct location!

---

# Changing Directories

- `cd  somedir 
  _c_hange  _d_irectory_ : move from one directory to another. Paths must be correct.
   - cd
     Change to home directory
   - cd ..
      Change to parent directory from current

---

# Listing Files

- `ls` 
   list files in current directory

- Options.  `ls` has many.
   - l
     long listing, includes file date and size
   -a
     displays all files.
   -h 
     show file sizes as a more human readable number
   -t 
     show the newest files first
   -r
     reverse
   -1 (digit one)
     list one file per line.  Convenient in scripts.

---

# Copying Files

- `cp file1 file2 `
   Copy `file1` into `file2`
   - Destructive if `file2` exists
- Options
  - `-r` copy files and subdirectories recursively.
  - `-n` "noclobber".  Do not overwrite an existing file.
  - `-i` ask for confirmation of overwriting.
  - `-p` preserve metadata.
  - If both `-i` and `-n` are present, the last one is used.

---

# Copy Files for This Tutorial

```
cp -r /share/resources/tutorials/rivanna-cl ./
```

Be sure to include a space between `rivanna-cl` and `./`.

---

# Renaming a File

- `mv file1 file2`
   - Destructive if file2 exists
   - Creates file2 if it does not exist
   - Deletes file1
   - `-i` ask for confirmation before overwriting 
   - `-n` noclobber
   - `-u` move only if file1 is newer than file2

---

# Moving a File

- `mv` "moves" a file.
   - Without paths, renames the file in the current directory
   - With a path, relocates the file to another directory

```
$pwd
$/home/mst3k/rivanna-cl/basic_commands
$mv SP_R1.list  list_of_reads.txt
```

---

# Creating Files

If not already in your basic_commands subdirectory, navigate there. Type after your prompt
```
cat >mynewfile
# This is a file.
```
Use an editor of your choice and type another line or two of text.
```
more mynewfile
ls
mv mynewfile the_file
cp the_file old_file
ls -l
```

---

# Deleting a File

- `rm file` 
   (r_ e _m_ ove) 
   - deletes a file.  With no options, **does not ask for permission**!
   - `-r` remove files and subdirectories recursively.
   - `-i` ask for confirmation
   - `-f` do not ask (overrides -i if it comes after it).

---

# Tutorial Directory

Change to a tutorial directory.  List the files that are in it.

```
cd rivanna-cl/basic_commands
ls -lh
```

You should see something like

```
total 245M
-rw-r--r-- 1 mst3k users 2.1K Jan 29 13:20intro_basic-unix.txt
-rwxr-xr-x 1 mst3k users 197M Jan 29 13:20list_of_reads.txt
-rw-r--r-- 1 mst3k users  46M Jan 29 13:20sequences.fasta
-rw-r--r-- 1 mst3k users 1.6M Jan 29 13:20 SP_R1.fastq
-rw-r--r-- 1 mst3k users 1.6M Jan 29 13:20 SP_R2.fastq
```

# Creating Directories

- `mkdir newdir`  
   _m_ ake _d_ irectory
  - Path may be absolute or relative

---

# Creating Directories Example

```bash
cd rivanna-cl/basic_commands
mkdir newdir
$ ls -lhtr
```

# Creating Directories: Paths

You can use absolute paths or relative paths for the new directory.
Let’s do some examples! Please type these into your shell directly.

```
mkdir newcode
mkdir ~/newcode/build
cd newcode
mkdir ../oldcode
mkdir src
mkdir ../oldcode/src
```

---

# Deleting Directories

- `rm` can also remove a directory
   - `rm -d somedir` remove the directory (must be empty)
   - `rm -rf somedir` remove directory, files, and subdirectories. Don't ask for permission for each entry.
- `rmdir somedir`
   - synonym for `rm -d somedir`

---

# Exercise

* Start a terminal
* Create a new directory called `testdir` in your home directory.
* Change into the new directory
* List the contents
* What is the full path of your home directory?

---

# Using More to View File Contents

`more filename`

Displays file contents on the screen with paging.
- Enter key: advance one page (fill screen)
- space bar: advance one line
- b or control-b: skip back one page
- arrow up/down: page up or down
- q or Q: exit

In most implementations you can search a page in the forward direction with
```
/<pattern>
```
Search for next occurrence of <pattern>
```
n
```
---

# Wildcards

Strings of characters may be matched with wildcards.

- The asterisk (\*) can match zero to unlimited characters, except a leading period.
- The question mark (?) matches exactly _one_ character.  
- Wildcards enable you to work with files without needing to type multiple files with similar names.
  - ls \*py
  - rm list?.sh

BE CAREFUL when using wildcards with `rm`! Gone is gone! On some systems there may be backups, or maybe not, and if you are using a personal system you would need to set up backups and learn to retrieve files.

---

# Searching Files

`grep` is used to filter a file or group of files, line by line, against a pattern (e.g., to print each line of a file which contains a match for <pattern>).

- grep [OPTIONS] PATTERN FILENAME
   - `-i` ignore the case of letters
   - `-v` invert; show non-matching lines
   - `-r` search recursively through current directory (omit filename)
   - `-c` count lines

---

# Extended Grep

The _PATTERN_ that grep matches may be a simple string, or it may be a special type of pattern called a _regular expression_.  Regular expressions (or "regexes") are beyond our scope here, but to use one with `grep` add the `-e` option, for extended, and enclose the regex in quotes.

A very common regex pattern is `^` which stands for "beginning of the line."  So we could search for the letter `C` at the beginning of the line with
```
grep -e "^C" *.f
```

See [this site](https://regexone.com/) for an introduction to regular expressions.

---

# Exercise

* By definition, each DNA sequence record in a FASTA file has one line of description that always starts with >, followed by multiple lines of sequence itself. Each sequence record ends when the next line starting with > appears.  For the file
```
~/rivanna-cl/basic_commands/sequences.fasta
```
how would you count the number of sequences it contains?

---

# Your Best Friend

The basic documentation for a command can be read from the shell with theman(manual) command

man ls

---

# Standard Streams

Each executable has associated with it three I/O streams: __standard input__ , __standard error__ , and __standard output__ .
They are often represented as __stdin__ , __stderr__ , and __stdout__ .

Normally these streams are attached to your console (your interactive shell).
Most Unix commands read from standard input and/or write to standard output.

# Stream Redirection

You can redirect standard input with <
```
mycode < params.txt
```
Redirect standard output with >
```
ls –l > filelist
```
Redirection overwrites anything already in the target file.  Append with >>
```
cat file1 >>bigfile
```
Redirection of standard error depends on the shell

Bash:
```
make >& make.out
```
Redirects both stdout and stderr to make.out

---

# Pipes

- One of the most powerful properties of Unix is that you can __pipe__ the standard output of one command into the standard input of another.
- The pipe symbol `|` is above the backslash on most US keyboards.

Example
```
grep "@H-148:116" SP_R1.fastq | head
```
grep searches for the pattern in the file and `head` displays first 10 lines of the output.

---

# Shell Variables

- Bash is a scripting language and as such has variables.  Shell variables can be defined and then referenced with a leading `$`.  
- Important: when setting shell variables with `=`, do _not_ put any spaces around the `=` symbol.

- The value of a variable can be printed with `echo`.

- Some variables are preset by the system.

Examples
```
echo $shell
```

---

# Environment Variables

- An **environment variable** is a shell variable that describes some aspect of the user's environment.  These variables are conventionally written in all capitals.  

- Several environment variables are predefined.  

- Environment variables are still shell variables and can be printed with `echo`.  But a special command `printenv` is available.  It does not use the leading `$`.
```
echo $SHELL
printenv SHELL
printenv HOME
printenv USER
ls $HOME
```

---

# The PATH Variable

- A particularly important environment variable is PATH.  This represents the _search path_ for executables.  It is read from left to right and if two versions of an executable are in different paths, it uses the first one it finds. Each element of PATH must be a directory. 
```
printenv PATH
```

- The PATH variable can be modified by appending or prepending.  This is a major part of what modules do.
```
printenv PATH
module load gcc/9.2.0
printenv PATH
```

# Modifying Your PATH

Environment variables can be set with the `export` command.  This differs from `=` in that `export` sets the variable for the current shell and any "child" shells started by the current shell.  

Example:
In most cases, current working directory (.) is not in your default search path.  To add it, type (for bash)
```
export PATH=$PATH:.
```
In this case it is essential to add the first $PATH or you will lose the default path set by the system.

---

# System Utilities Paths

If you forget to include the base $PATH when you modify it, you can find most Linux utilities in /usr/bin or /usr.
```
export PATH=.
ls 
```
OOPS!
```
/usr/bin/ls
```
Any changes to PATH will disappear when you exit the shell in which you made them.

---

# Running Executables

You will run some form of executable on the system.  Executables are often called binaries, especially by Unix types and computer programmers.  The terms are synonymous in most cases.

If the executable is in your _search path_ you can simply type its name at the prompt.

```bash
gedit hello_world.sh
```
here `gedit` is the name of the binary.  Its actual location is `/usr/bin/gedit`,but `/usr/bin` is in the default search path.

If it is not in your search path you must type the path to the executable (can be absolute or relative)

./hello_world.sh

Usually the current directory is not in your default search path for security reasons.

---

# SLURM

You now have the tools to manage your jobs from the command line and write your own scripts for your SLURM jobs.  Please see our SLURM tutorial for more information.

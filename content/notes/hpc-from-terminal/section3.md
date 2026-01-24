---
title:  III - File Manipulation
date: 2023-12-11T00:00:00-05:00
type: docs
toc : true 
weight: 30
menu: 
    hpc-from-terminal:
---

## File Permissions

Each file and directory has a set of _permissions_ associated with it. Traditionally, Unix has assigned permissions to the _owner_ (the user), the _group_ (a set of users), _others_ (users not the owner nor a member of the group that may have permissions on the file), and _all_ (all users on the system). These are generally denoted by `ugoa`.

Within each category, a user can have permission to _read_, _write_, or _execute_ the file.  These are denoted by `rwx`.

The command
```bash
$ls -l
```
shows the permissions associated with each file.  Since in Unix a directory is a special type of file, the permissions are the same.

```bash
$ls -l
total 8
drwxr-xr-x. 2 mst3k mst3k 4096 Dec 18 10:39 data
drwxr-xr-x. 2 mst3k mst3k 4096 Dec 18 10:22 shakespeare

```
The output of `ls -l` is

{{< table >}}
| Permissions | Number of "Hard Links"  |  Owner |  Group | Size | Date and Time Last Modified | Name|
| ---- | ---- | -------- | ------ | ---- | ----------- | ---- |
| drwxr-xr-x | 2 | mst3k | mst3k | 4096 | Dec 18 10:39 | data |
{{< /table >}}

Permissions not granted to a user are indicated with a hyphen `-`.

The permissions layout in the first columns should be read as

{{< table >}}
| Type |  Owner   |  Group Owner |  Others |
| ---- | -------- | ------ | ---- |
|  d   |  rwx     |  r-x   |  r-x |
{{< /table >}}

In the above example, the `d` indicates the listing is a directory. A directory must have "execute" `x` permission in order for a user to enter it.

Listing the files in a directory will result in output such as
```bash
$ls -l shakespeare
-rwxr-xr-x. 1 mst3k mst3k     91 Dec 18 10:22 2col.txt
-rwxr-xr-x. 1 mst3k mst3k 173940 Dec 18 10:22 Hamlet.txt
-rwxr-xr-x. 1 mst3k mst3k 162563 Dec 18 10:22 HamletWords.txt
-rwxr-xr-x. 1 mst3k mst3k     34 Dec 18 10:22 k2sort.txt
-rwxr-xr-x. 1 mst3k mst3k 180857 Dec 18 10:22 Lear.txt
-rwxr-xr-x. 1 mst3k mst3k 154520 Dec 18 10:22 Othello.txt
-rwxr-xr-x. 1 mst3k mst3k  25628 Dec 18 10:22 uniques
-rwxr-xr-x. 1 mst3k mst3k  25628 Dec 18 10:22 uniques.txt
-rwxr-xr-x. 1 mst3k mst3k     72 Dec 18 10:22 wcdemo.txt
-rwxr-xr-x. 1 mst3k mst3k     26 Dec 18 10:22 words_and_num.txt
```
The hyphen as Type indicates an ordinary file.

## Changing Permissions

The owner of a file or directory may change the permissions with the `chmod` command.  Two formats are supported: a three- or four-digit code (in octal, i.e. base 8) indicating permissions, or the add/remove symbolic format.  The digit format is advanced so we will not discuss it; a reference is available [here](https://www.adminschoice.com/chmod-quick-referance-with-examples "chmod Quick Reference with Examples").  The symbolic format is more intuitive and mnemonic.  

**Examples** 

Add execute permission to a file for its owner. This is frequently used with shell scripts.
```bash
$chmod u+x ascript.sh
```

Add execute permissions for the owner and group members
```bash
$chmod ug+x ascript.sh
```

Allow others to read and write a file
```bash
$chmod o+wr myfile.txt
```

Please note that on multiuser, central systems such as an HPC cluster, the administrators may not allow individual users to change the permissions on certain file sets such as `home` and `scratch` directories, for reasons of data security and privacy.

## Creating and Editing Files


To create files we use a _text editor_.  Do not use a word processor such as LibreOffice.

### Graphical Options

Graphical editors must be used from within a graphical environment.  On Linux the standard graphical windowing system is called **X11**.  Newer Linux versions provide some form of "desktop" environment similar to Windows or macOS on top of X11.  On our system we provide the [MATE](https://mate-desktop.org/ "The official MATE website") Desktop Environment.  It can be accessed from [FastX](https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/ "The Research Computing website's FastX info page") on a loginnode.  It can also be started on a compute node from the [Open OnDemand](https://www.rc.virginia.edu/userinfo/rivanna/ood/overview/ "The Research Computing website's Open OnDemand info page") Desktop Interactive Application.

#### 1. gedit/pluma

Modern Linux systems provide at least one graphical text editor. One option is `gedit`.  On the MATE Desktop, the equivalent is `pluma`, but the `gedit` command starts pluma.

Gedit/pluma is very similar to Notepad on Windows.

#### 2. VS Code

This is accessed by a [module](https://www.rc.virginia.edu/userinfo/hpc/software/modules/ "The Research Computing website's Software Modules page").
```bash
$module load code-server
```

Then open a browser (Firefox is the default on MATE) and go to the URL `http://127.0.0.1:8080`. First set up would need a password that can be extracted from the file `~/.config/code-server/config.yaml`.

### Text-Based File Editors

Regular Linux users are advised to learn at least one text-based editor, in case a graphical interface is not available or is too slow.

#### 1. vi (vim)

The oldest and most widely available text-based editor on Unix is _vi_ for "visual." Modern versions are nearly all based on [vim](https://vim.org "The official Vim website") ("vi improved").  On our system we generally _alias_ the `vi` command to `vim`.

Vim/vi is used entirely through keyboard commands which must be memorized.  The mouse is not utilized.  Only a few commands are needed to be able to do basic editing and they are not difficult to learn.  A beginner tutorial is [here](https://www.tutorialspoint.com/vim/index.htm "Vim tutorial"). One stumbling block for new users is that vim has _command mode_ and _insert mode_ and it must be toggled between the two.

Basics:
  * To enter the insert mode press  `i`
  * To enter the command mode press  `ESC`
  * To save the file enter  `:w`
  * To save under a new name  `:w filename`
  * To exit `:q`

MATE also provides a graphical interface called `gvim`. It can be accessed through the Applications->Accessories menu as Vi IMproved.  GVim combines the keyboard-oriented commands of vim with some mouse-based capabilities.

#### 2. nano

Nano is simple text editor that is fairly self-explanatory and simple to learn. It is always in insert mode. A tutorial similar to that for vim above is [here](https://www.tutorialspoint.com/how-to-use-nano-text-editor "How To Use Nano Text Editor").

Basics:
  * Immediately start typing
  * To exit:  __control+X__
  * Type the filename and press  __Enter__

#### 3. Emacs

The Emacs editor is another long-time editor on Unix. Similar to gvim, it combines a graphical interface, when available, with a keyboard-based command system.  If the graphical interface is not available, such as from an Open OnDemand terminal, it will fall back to a text-only interface. Emacs is powerful and complex.  The documentation is [here](https://www.gnu.org/software/emacs/tour/ "Emacs guided tour").

## Viewing Files

### Pagers

The most widely used way to view files quickly without editing is to use a _pager_.  A pager prints to the terminal window the amount of text that fits in that window.  The default pager on Linux is `more` (also called `less` because "less is more").

```bash
$more filename
```
This displays the contents of a file on the screen with line scrolling. To scroll you can use ‘arrow’ keys. To advance one line, press the Enter key.  To advance a full page, press the space bar. Press `q` to exit.

```bash
$more ~/rivanna-cl/shakespeare/Lear.txt
```

To page upward within the text, press `b` (back).

#### Searching

You can search in the forward direction with `/<pattern>`, where pattern is a combination of characters you wish to find.

```bash
$more ~/rivanna-cl/shakespeare/Lear.text
 /serpent
```
<pre>
 ...skipping
     Turn all her mother's pains and benefits
     To laughter and contempt, that she may feel
     How sharper than a serpent's tooth it is
     To have a thankless child! Away, away!                Exit.
</pre>

Search stops at the first occurrence. To locate the next one, type `n`.

## Printing a file to the console

The `cat` (concatenate) command prints the contents of a file to the screen.
```bash
$cat myfile
```
It does not fit the output to the terminal windows size; it simply keeps printing to the end.

It can also be used to create or join files (hence its name). We will learn more about its behavior here when we look at standard streams.

```bash
#create a file
$cat > newfile
Now I can enter
some text
It will be copied exactly into the file
^d
```
The `^d` notation means hold down `Ctrl` and `d` together. It is the end-of-file marker for bash.

```bash
#append a file to another
$cat otherfile >> newfile
```
{{< warning >}}
If used single `>` in place of the double `>>` in the above, `cat` will overwrite `newfile` with `otherfile`.
{{< /warning >}}

## Displaying Parts of a File

### head and tail

`head` filename

Displays only the starting lines of a file. The default is first ten lines. Use “-n” to specify the number of lines.
```bash
$head ~/rivanna-cl/shakespeare/Lear.text
```
<pre>
This Etext file is presented by Project Gutenberg, in
cooperation with World Library, Inc., from their Library of the
Future and Shakespeare CDROMS.  Project Gutenberg often releases
Etexts that are NOT placed in the Public Domain!!

*This Etext has certain copyright implications you should read!*

&lt&ltTHIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM
SHAKESPEARE IS COPYRIGHT 1990-1993 BY WORLD LIBRARY, INC., AND IS
PROVIDED BY PROJECT GUTENBERG WITH PERMISSION.  ELECTRONIC AND
</pre>

`tail` filename

Displays the last 10 lines.
```bash
$tail 30 ~/rivanna-cl/shakespeare/Lear.text
```
<pre>
     The cup of their deservings.- O, see, see!
  Lear. And my poor fool is hang'd! No, no, no life!
     Why should a dog, a horse, a rat, have life,
     And thou no breath at all? Thou'lt come no more,
     Never, never, never, never, never!
     Pray you undo this button. Thank you, sir.
     Do you see this? Look on her! look! her lips!
     Look there, look there!                            He dies.
  Edg. He faints! My lord, my lord!
  Kent. Break, heart; I prithee break!
  Edg. Look up, my lord.
  Kent. Vex not his ghost. O, let him pass! He hates him
     That would upon the rack of this tough world
     Stretch him out longer.
  Edg. He is gone indeed.
  Kent. The wonder is, he hath endur'd so long.
     He but usurp'd his life.
  Alb. Bear them from hence. Our present business
     Is general woe. [To Kent and Edgar] Friends of my soul, you
        twain
     Rule in this realm, and the gor'd state sustain.
  Kent. I have a journey, sir, shortly to go.
     My master calls me; I must not say no.
  Alb. The weight of this sad time we must obey,
     Speak what we feel, not what we ought to say.
     The oldest have borne most; we that are young
     Shall never see so much, nor live so long.
                                       Exeunt with a dead march.

THE END
</pre>

## Transferring Files Using the Command Line

Files can be transferred by [graphical clients](https://www.rc.virginia.edu/userinfo/rivanna/logintools/graphical-sftp "The Research Computing website's Graphical Transfer Tools page") such as MobaXterm and Filezilla, or through [Globus](/tutorials/globus-data-transfer "Globus tutorial").  If you are using a terminal from your local computer, you can also use some command-line tools.

### scp and sftp

The secure shell protocol includes two file-transfer command-line tools; `scp` and `sftp`.  Sftp is scp with a slightly different interface.

```bash
$scp LOCAL_FILE mst3k@login.hpc.virginia.edu:REMOTE_PLACE
$scp mst3k@login.hpc.virginia.edu:REMOTE_FILE LOCAL_PLACE
```
`REMOTE_PLACE` and `LOCAL_PLACE` refer to the location on the appropriate host where you want the file to be written.  `REMOTE_PLACE` can be omitted and the file will be transferred to your home directory under the same name.  To change the name or specify a directory on the other system, use a different name or path for `REMOTE_PLACE`.

`LOCAL_PLACE` must be present but can be `.` for the directory where the `scp` was run.

The colon `:` is required.

To copy a directory, use `scp -r` similarly to `cp -r`.

**Examples**
```bash
$scp myfile.txt mst3k@login.hpc.virginia.edu:
$scp myscript.py mst3k@login.hpc.virginia.edu:project1
$scp myscript.py mst3k@login.hpc.virginia.edu:script.py
$scp myfile.csv mst3k@login.hpc.virginia.edu:/scratch/mst3k
$scp mst3k@login.hpc.virginia.edu:/scratch/mst3k/run11/output.txt .
```

Scp resembles cp.  Sftp is an implementation over scp of the interface of a popular, but insecure, protocol widely used in the past called `ftp` (file transfer protocol).

```bash
$sftp mst3k@login.hpc.virginia.edu
sftp> put local_file
sftp> get remote_file
sftp> quit
```
The sftp client permits other commands. `ls` lists files on the remote system.  `lls` lists local files.

### rsync

The rsync command is used to _synchronize_ files and folders.  It has many options and some attention must be paid to whether a trailing slash is needed or not.

```bash
$rsync -av ldir/ mst3k@login.hpc.virginia.edu:rdir
$rsync my_file mst3k@login.hpc.virginia.edu:/scratch/$USER
```
By default `rsync` does not transfer files that are older than the equivalent on the target. This can increase the transfer speed significantly. Rsync also resumes a transfer that was interrupted.  Scp always starts again from the beginning.

Rsync is very powerful but has many options and can be confusing. For more details see our [documentation](https://www.rc.virginia.edu/userinfo/rivanna/logintools/cl-data-transfer "The Research Computing website's Command Line Data Transfer page"). Several online resources with examples are also available, such as [this](https://www.digitalocean.com/community/tutorials/how-to-use-rsync-to-sync-local-and-remote-directories "How To Use Rsync to Sync Local and Remote Directories").

### Setting up Passwordless ssh

If you will frequently use ssh, scp, sftp, or rsync to a remote system, it becomes inconvenient to repeatedly enter your password.  Follow the instructions given [here](https://www.rc.virginia.edu/userinfo/howtos/general/sshkeys/ "The Research Computing website's SSH Keys page") to generate a _key_ to log in without a password.

## Finding Information About a Command

Basic documentation for a command can be read from the shell with the  `man`  (manual) command.
```bash
$man ls
```

Many commands also provide a `--help` option which usually prints the same information.
```bash
$ls --help
```

The output of man is often called the _manpage_ of that command.

## Exercise

Q1: Create a directory called `newdir`.  Navigate to it.  Make the new file `mynewfile` and save it with `^d`. Use nano or another editor of your preference and type a line or two of text.

<details><summary>Solution</summary>

```bash
$mkdir newdir
$cd newdir
$cat > mynewfile
nano mynefile
```
</details>

Q2: View the content of the `mynewfile` using `more`. Rename `mynewfile` to `the_file`. Copy `the_file` to `old_file`. List the files in long format.

<details><summary>Solution</summary>

```bash
$more mynewfile
$mv mynewfile the_file
$cp the_file old_file
$ls -l
```

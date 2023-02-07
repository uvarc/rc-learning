---
title: "Working with Files"
linktitle: "Tutorial 2: Working with Files"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
weight: 30

menu:
    unix-tutorials:
---

## Copying Files

### `cp` (copy)

`cp` file1 file2 is the command which makes a copy of file1 in the current working directory and calls it file2. 

For our example we will create a file _science.txt_.  Click the down-arrow icon to download the file.  Use whatever method you know to place this file into your home directory.  

{{< code-download file="/notes/unix-tutorial/snippets/science.txt" lang="no-highlight" >}}

```bash
% cd ~/unixstuff
```

Then at the Unix prompt, type,

```bash
% cp ../science.txt .
```

Note: Don't forget the dot (.) at the end. Remember, in UNIX, the dot means the current directory. 

The above command means copy the file science.txt from the parent directory to the current directory, keeping the name the same.  To change the name, use
```bash
% cp ../science.txt ./newname
```

### Exercise 2A

Create a backup of your science.txt file by copying it to a file called science.bak.

## Moving Files

### `mv` (move)

`mv file1 file2` moves (or renames) `file1` to `file2`. To move a file from one place to another, use the `mv` command. This has the effect of moving rather than copying the file, so you end up with only one file rather than two. It can also be used to rename a file, by moving the file to the same directory, but giving it a different name. We are now going to move the file science.bak to your backup directory. First, change directories to your unixstuff directory (can you remember how?). Then, inside the unixstuff directory, type

```bash
% mv science.bak backups/.
```

Type `ls` and `ls backups` to see if it has worked.

## Removing Files and Directories

## `rm` (remove), `rmdir` (remove directory)

To delete (remove) a file, use the `rm` command. As an example, we are going to create a copy of the science.txt file then delete it. Inside your unixstuff directory, type

```bash 
% cp science.txt tempfile.txt
```
Confirm the file was created:
```bash
% ls
```
Now delete it:
```bash
% rm tempfile.txt
% ls
```

You can use the `rmdir` command to remove a directory, but only if it is empty. Try to remove the backups directory. You will not be able to since Unix will not let you remove a non-empty directory.

To remove a non-empty directory use 
```bash
rm -rf directory
```

{{< warning >}}
The above command will remove the directory without confirming anything!  Be extremely careful with it!
{{< /warning >}}

You can request confirmation with
```bash
% rm -if directory
```
though this may be tedious.  The `-i` option (inquire) also works for `rm`
```bash
% rm -i myfile
```

## Exercise 2B

Create a directory called tempstuff using mkdir, then remove it using the rmdir command.

## Displaying the Contents of a File on the Screen

### `cat` (concatenate)

The `cat` command can show a text file's contents
```bash
% cat science.txt
```
Be sure to use the correct path to the file. Cat can also join two text files, hence its name.
```bash
% cat file1 file2 > file3
```
THe `>` sign is a _redirection_, which we will discuss later.

### `clear` (clear screen)

Clear the screen.

### `more`

The command `more` prints the contents of a file onto the screen a page at a time. Type

```bash
% more science.txt
```
Press the [spacebar] if you want to see another page. Type [q] if you want to quit reading. 

The `more` command is an example of a **pager**, a program that "pages" through a text file.

### `head`

The head command writes the first ten lines of a file to the screen.
```bash
% head science.txt
```
Now type
```bash
% head -5 science.txt
```
What difference did the `-5` make to the `head` command?

### `tail`

The `tail` command writes the last ten lines of a file to the screen. Clear the screen and type
```bash
% tail science.txt
```
How can you view the last 15 lines of the file?

## Searching the Contents of a File

### Simple Searching Using `more`
Using `more`, you can search through a text file for a keyword (pattern). For example, to search through science.txt for the word 'science', type

```bash
% more science.txt
```
then, still in `more` (i.e. don't press [q] to quit), type a forward slash [`/`] followed by the word to search
```no-highlight
/science
```
The `more` command finds and highlights the keyword. Type [n] to search for the next occurrence of the word.

### `grep` (don't ask why it is called grep)

`grep` is one of many standard Unix utilities. It searches files for specified words or patterns. First clear the screen, then type

```bash
% grep science science.txt
```
As you can see, grep has printed out each line containg the word science... or has it? Try typing

```bash
% grep Science science.txt
```
The `grep` command is case sensitive; it distinguishes between Science and science. To ignore upper/lower case distinctions, use the -i option, i.e. type

```bash
% grep -i science science.txt
```

To search for a phrase or pattern, you must enclose it in single quotes (the apostrophe symbol). For example to search for spinning top, type

```bash
% grep -i 'spinning top' science.txt
```

Some of the other options of `grep` are: `-v` (display those lines that do NOT match); `-n` (precede each maching line with the line number); and `-c` (print only the total count of matched lines). Try some of them and see the different results. Don't forget, you can use more than one option at a time, for example, the number of lines without the words `science` or `Science` is

```bash
% grep -ivc science science.txt
```

#### `wc` (word count)

A handy little utility is the wc command, short for word count. To do a word count on `science.txt`, type
```bash
% wc -w science.txt
```
To find out how many lines the file has, type
```bash
% wc -l science.txt
```

## Summary

| Command | Operation |
|---|---|
| cp file1 file2 | copy file1 and call it file2 |
| mv file1 file2 | move or rename file1 to file2 |
| rm file | remove file |
| rmdir directory | remove directory |
| cat file | display file |
| more file | display file a page at a time |
| head file | display the first few lines of a file |
| tail file | display the last few lines of file |
| grep 'keyword' file | search file for keywords |
| wc file` | count number of lines/words/characters in file |

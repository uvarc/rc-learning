---
title: "More About Files"
linktitle: "Tutorial 3: More About Files"
date: 2019-04-29T11:06:47-04:00
draft: false
highlight_style: "github"
toc: true
type: docs
weight: 40
date: 2023-12-11T00:00:00Z
menu:
    unix-tutorials:
---


##  Redirection

Most processes initiated by Unix commands write to the _standard output_ (that is, they write to the terminal screen), and many take their input from the _standard input_ (that is, they read it from the keyboard). There is also the _standard error_, where processes write their error messages, by default to the terminal screen. Type `cat` without specifying a file to read
```bash
% cat
```
Then type a few words on the keyboard and press the [Return] key. Finally hold the [CTRL] key down and press `[d]` (written as `^D` for short) to end the input. What has happened? If you run the cat command without specifying a file to read, it reads the standard input (the keyboard), and on receiving the 'end of file' (`^D`), copies it to the standard output (the screen). In UNIX, we can redirect both the input and the output of commands.

## Redirecting the Output

We use the `>` symbol to redirect the output of a command. For example, to create a file called list1 containing a list of fruit, type
```bash
% cat > list1
```
Then type in the names of some fruit as follows. Press [Return] after each one. Terminate with the end-of-file marker control-d (`^D`).
```no-highlight
pear 
banana 
apple 
^D
```
The `cat` command reads the standard input (the keyboard) and the > redirects the output, which normally goes to the screen, into a file called list1. To read the contents of the file, type
```bash
% cat list1
```

## Exercise 3A
Using the above method, create another file called list2 containing the following fruit: orange, plum, mango, grapefruit. The form >> _appends_ standard output to a file. So to add more items to the file list1, type
```bash
% cat >> list1
```
Then type in the names of more fruit
```no-highlight
peach 
grape 
strawberry
^D
```
To read the contents of the file, type
```bash
% cat list1
```

You should now have two files. One contains six fruit names, the other contains four fruits. We will now use the cat command to join (concatenate) list1 and list2 into a new file called biglist. Type

```bash
% cat list1 list2 > biglist
```
This reads the contents of list1 and list2 in turn, then outputs the text to the file biglist. To read the contents of the new file, type
```bash
% cat biglist
```

## Redirecting the Input

We use the `<` symbol to redirect the input of a command. The command sort alphabetically or numerically sorts a list. Type
```bash
% sort
```
Using `<` you can redirect the input to come from a file rather than the keyboard. For example, to sort the list of fruit, type
```bash
% sort < biglist
```
and the sorted list will be output to the screen. To output the sorted list to a file, type
```bash
% sort < biglist > slist
```
Use cat to read the contents of the file slist.

## Pipes
To see who is on the system with you, type
```bash
% who
```
One method to get a sorted list of names is to type
```bash
% who > names.txt
% sort < names.txt
```
This is a bit slow and you have to remember to remove the temporary file called names.txt when you have finished. What you really want to do is connect the output of the `who` command directly to the input of the `sort` command. This is exactly what pipes do. The symbol for a pipe is the vertical bar | which, on a US keyboard, is above Enter on the right, with the backslash. For example, typing
```bash
% who | sort
```
will give the same result as above, but quicker and cleaner. To find out how many users are logged on, use wc (word count) with the option -l (ell) for number of lines only:
```bash
% who | wc -l
```

## Exercise 3B
Using pipes, print all lines of list1 and list2 containing the letter 'p', sort the result, and print to a file sorted_plist.txt.  Hint: from `grep --help` find an option to print only the line, omitting the file name.

{{< spoiler text="Solution" >}}
{{< code file="/notes/unix-tutorial/snippets/ex3b.txt" lang="bash" >}}
{{< /spoiler >}}

### Summary
| Command | Operation |
|---|---|
| `command >` file | redirect standard output to a file |
| `command >>` file | append standard output to a file |
| `command <` file | redirect standard input from a file |
| <code>command1 | command2</code> | pipe the output of command1 to the input of command2 |
| `cat file1 file2 >` file0 | concatenate file1 and file2 to file0 |
| `sort` | sort data |
| `who` | list users currently logged in |



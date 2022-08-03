---
title: Files
toc: true
type: docs
draft: false
weight: 72

menu:
    python_introduction:
        parent: IO and Exceptions
        weight: 72
---

Files are the main ingredients of our programs.  Our scripts are files; we may have input files; we will usually want some kind of output file.  We can manipulate files in Python without having to go through the operating system's user interface.

## Files, Folders, and Paths

Three major operating systems are in use today; Windows, Mac OS, and Linux. Each one does things a little differently.
The `os` module provides access to some basic operating-system functionality, particularly those related to files, in a uniform interface.

### Paths and Platforms

The location of a file is specified by its _path_.  The exact format of the path varies somewhat by operating system.  Python uses _forward slashes_ to separate folders, even on Windows where the backslash (`\`) is "native."

Python tends to be rooted in the Unix operating system so some of the vocabulary comes from there.  "Folders" in Windows and Mac OS are called "directories" in Unix.  The full path to a file is the tree of all folders/directories that must be traversed to reach it. 

```python
#Windows
filename="C:/Users/You/Desktop/Python Programs/myscript.py"
#Mac OS
filename="/Users/You/Desktop/Python Programs/myscript.py"
#Linux
filename="/home/you/python_programs/myscript.py"
```

We can use the `os` module to make our scripts a little more platform-independent.  The `expanduser` function will get the home directory of the user running the script.  For this we must use the `path` submodule of os.

```python
import os
home_dir=os.path.expanduser('~')
```
The tilde `~` stands for the home directory in any operating system.

We can use `join` to concatenate folders and paths into complete file names. On Windows, `join` understands both forward and backward slashes.

```python
#Windows
folder="Desktop\Python Programs"
#Mac OS
folder="Desktop/Python Programs"
#Linux
folder="python_programs"
#All
script="myscript.py"
filename=os.path.join(home_dir,folder,script)
f=open(filenam)
```
The `os` module can perform basic file and folder manipulations in a way that is appropriate for each operating system.  

### Changing and Creating Directories

When we run a script, the path from which it is run is the _current working directory_.  Note that Jupyterlab and IDEs may set the current working directory their own way.  We can get it through the `os` module and we can change it.

```python
mycwd=os.getcwd()
newpath="/home/you/somefolder"
os.chdir(newpath)
```

To create a new folder/directory, use `mkdir`
```python
os.mkdir(new_folder) #relative to CWD
os.mkdir(fullpath)   #full path
```

### Listing Files

We can list the files in a directory with `listdir` from `os`.  It returns a list.
```python
files=os.listdir() #current working directory
input_directory=os.path.listdir(input_path) #using a full path
```
We can check whether an item is a directory with `os.path.isdir()`, which can also check whether it exists.  Similar for `os.path.isfile()`.
```python
#Starting from CWD
for file in os.listdir():
    if os.path.isdir(file):
        print("{} is a directory".format(file))
    elif os.path.isfile(file):
        print("{} is a file".format(file))
    else:
        print("{} is neither a file nor a directory".format(file))
```

If we do not need a list of the files but only an iterator, we can use `scandir`.  Scandir returns an object, not a string, so we must extract the parts we need.  The advantage to scandir is that it can be faster if we need to test any attributes of the file.
{{< code file="/courses/python_introduction/scripts/scandir.py" lang="python" >}}

### Copying and Moving Files

Another module, [shutil](https://docs.python.org/3/library/shutil.html), allows us to move and copy files and perform other basic operations on them.

```python
import shutil
shutil.copy(source,destination)
shutil.move(source,destination)
```
In the above, both `source` and `destination` should be either strings or should be paths created by some function such as `os.path.join`.

For more details about the os module, see its [documentation](https://docs.python.org/3/library/os.html).

## Opening a File

Before anything can be done with a file we must _open_ it.  This attaches the file name to the program through some form of _file descriptor_, which is like an identifier for the file.  Once opened we do not refer to the file by its name anymore, but only via the ID.

We open a file and associate an identifier with it by means of the `open` statement.

```python
fp=open("filename")
fin=open("filename","r")
fout=open("filename","w")
```

The default is read-only.  Adding the `r` makes this explicit.  To open for writing only, add the `w`.  This overwrites the file if it already exists.  To append to an existing file, use `a` in place of `w`.  Both `w` and `a` will create the file if it does not exist.  To open for both reading and writing, use `r+`; in this case the file must exist.  To open it for reading and writing even if it does not exist, use `w+`.

## Closing Files

When you have completed all operations on a file you should close it.

```python
fin.close()
fout.close()
```

Files will be automatically closed when your script terminates, but best practice is to close them all yourself as soon as you are done with it.  You _must_ close a file if you intend to open it later in a different mode.  You cannot reopen a file using an active file descriptor.  You must first close it.

**Exercise**

All three major operating systems have a Documents folder by default in their desktop environments.  Write a script to 
 1. change the working directory to the Documents directory.  Use a general way to construct the path.
 2. List all the files in the directory.  Use a string function to check whether any end in ".txt."  OK to use listdir.
 3. Make a new folder "MyTestFolder" in the Documents directory.  Check first whether it already exists.
 4. Open a file "new_file.txt" for writing.
 5. Use the following line to write a little text into it.  Replace "f" with your choice of file identifier.
    f.write("Here are some words for this file.\n")
 6. Close the file.
 7. Move the file into "MyTestFolder."

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/exercises/file_fiddling.py" lang="python" >}}
{{< /spoiler >}}

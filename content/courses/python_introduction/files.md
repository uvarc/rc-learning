---
title: Files
toc: true
type: book
draft: false
weight: 71
---

Most of the time we read or write from a file rather than from the console.  File input/output can be complicated in Python and we will later see some built-in means of reading particular types of files, particularly comma-separated-values (CSV) text files.

Before anything can be done with a file we must _open_ it.  This attaches the file name to the program through some form of _file descriptor_, which is like an identifier for the file.  Once opened we do not refer to the file by its name anymore, but only via the ID.

## Opening a File

We open a file and associate an identifier with it by means of the `open` statement.

```python
fp=open("filename")
fin=open("filename","r")
fout=open("filename","w")
```

The default is read-only.  Adding the `r` makes this explicit.  To open for writing only, add the `w`.  This overwrites the file if it already exists.  To append to an existing file, use `a` in place of `w`.  Both `w` and `a` will create the file if it does not exist.  To open for both reading and writing, use `r+`; in this case the file must exist.  To open it for reading and writing even if it does not exist, use `w+`.

## Reading from a File

The standard read commands in Python read _only strings_.  Even if the input values are intended to be numbers, they are read in as strings.  Any conversions must be done by the programmer.

Reading commands

* f.read()
  * Reads the entire file identified by `f` into one large string
* f.readline()
  * Reads a single line (including the newline character) from the file `f`
* f.readlines() 
  * Reads all the lines into a list, one line per list element.  Newlines are included.

We have several options to handle the strings when reading directly from a file.  One option is to use read, then split all the lines on the `\n` character:

```python
fin=open("filename","r")
the_file=fin.read()
file_list=the_file.split("\r\n")
```

We can now process through the list using a loop.  Note that we use `\r\n` to make sure we accommodate Windows, Mac OSX, and Linux operating systems.  

A way to obtain a list of strings, with each element one line of the file, is to employ `map` with `readlines`:

```python
fin=open("filename","r")
lines=[line.strip('\r\n') for line in fin.readlines()]
for line in lines:
    #do whatever with each line, for example
    data=list(map(float,line.split(',')))  #will expect CSV file
    #do something with the data list corresponding to each line
```

Yet another option is to use a Python idiom in which we iterate through the file using the file identifier.  Suppose we have a file with comma-separated values.  We can read the data with

```python
fin=open("filename")
for line in fin:
    data=line.rstrip("\r\n").split(",")
```

In the examples above, we have not done anything to store the values of the data, so each time through the `data` variable will be overwritten and the previous values lost.  How we handle this will depend on the file and what should be done to its contents.  One common approach is to declare lists to hold the values in advance, then append as we go.

```python
fin=open("filename")
x=[]
y=[]
fin.readline()
for line in fin:
    data=line.rstrip("\r\n").split(",")
    x.append(float(data[0]))
    y.append(float(data[2]))
```

When we have completed this loop x will contain the values from the first column and y will contain values from the third column.  In this example we are not interested in the other columns.  We also convert on the fly to float.  This example assumes that our data file has one line of text header, which we use `readline` to skip.  Since we are not interested in the contents of the header line, we discard it and only use `readline` to move past it.

**Example**

Read the following file and store the values into two lists x and y.  You may choose any name for the file but make the file extension be `.csv`.  Print the values for the 4th row after reading in the data.
{{< code-snippet >}}
x,y
11.3,14.6
9.2,7.56
10.9,8.1
4.8,12.8
15.7,9.9
{{< /code-snippet >}}
Make sure there is no blank line at the beginning of the data file.

{{< code-download file="/courses/python_introduction/scripts/read_file_ex.py" lang="python" >}}

<details>
<summary>Exercise 17</summary>

Use readlines rather than a loop to read the data file.  Eliminate the header appropriately.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/readlines_demo.py" lang="python" >}}
{{< /spoiler >}}

We will not go into more detail about reading files since we will later cover packages that offer more convenient ways to read the most common formats.  

## Writing Files

To write a file it must have been opened appropriately.  We can use print with the addition of a `file=` argument.

```python
f=open("myfile","w")
print("Format string {:f} {:f}".format(x,y),file=f)
```

Corresponding to `read` there is a `write`.  It writes _one_ string to the specified file 

```python
fout=open("outfile","w")
fout.write(s)
```

The string can contain newline markers `\n` but `write` will not insert them.  They must be positioned explicitly.

Corresponding to `readlines` there is a `writelines`:

```python
fout=open("outfile","w")
fout.writelines(seq)
```

here `seq` is a sequence, usually a list of strings.  The `writelines` will not add any end-of-line markers so as with write, they must be appended to every string in the sequence where a linebreak is desired.

## Closing Files

When you have completed all operations on a file you should close it.

```python
fin.close()
fout.close()
```

Files will be automatically closed when your script terminates, but best practice is to close them all yourself as soon as you are done with it.  You _must_ close a file if you intend to open it later in a different mode.  You cannot reopen a file using an active file descriptor.  You must first close it.

<details>
<summary>Exercise 18</summary>

Open a file 

```python
data.txt 
```

in write mode.  Write to the file three columns of numbers separated by commas.  These columns should be

```python
n n**2 n**3
```

for n going from 1 to 20.  
Read back the file.  Store each variable into a list.  Use these values to compute and print 

$$ a+bn+cn^2+dn^3 $$

for 

```python
a=1.; b=2.4; c=5.8; d=0.7
```

Print the results for each n you have.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/polynomial.py" lang="python" >}}
{{< /spoiler >}}

</details>

## Resources


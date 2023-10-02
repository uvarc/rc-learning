---
title: Reading and Writing Files
toc: true
type: docs
draft: false
weight: 74

menu:
    python-introduction:
        parent: IO and Exceptions
---

Most of the time we read or write from a file rather than from the console.  File input/output can be complicated in Python and we will later see some built-in means of reading particular types of files, particularly comma-separated-values (CSV) text files.

## Reading from a File

The standard read commands in Python read _only strings_.  Even if the input values are intended to be numbers, they are read in as strings.  Any conversions must be done by the programmer.

*  Read the entire file identified by `f` into one large string:
  * `f.read()`
* Read a single line (including the newline character) from the file `f`
  * `f.readline()`
* Read all the lines into a list, one line per list element.  Newlines are included.
  * `f.readlines()`

We have several options to handle the strings when reading directly from a file.  One option is to use read, then split all the lines on the `\n` character:

```python
fin=open("filename","r")
the_file=fin.read()
file_list=the_file.split("\r\n")
```

We can now process through the list using a loop.  Note that we use `\r\n` to make sure we accommodate Windows, MacOS, and Linux operating systems.  

If we have numeric data, one way to obtain a list of floats for each line of the file is to employ `map` with `readlines`:
```python
fin=open("filename","r")
lines=[line.strip('\r\n') for line in fin.readlines()]
all_data=[]
for line in lines:
    #do whatever with each line, for example
    data=list(map(float,line.split(',')))  #will expect CSV file
    #do something with the data list, for example
    all_data.append(data)
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

**Exercises**

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

{{< code-download file="/courses/python-introduction/scripts/read_file_ex.py" lang="python" >}}

Use readlines rather than a loop to read the data file.  Eliminate the header appropriately.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/readlines_demo.py" lang="python" >}}
{{< /spoiler >}}

We will not go into more detail about reading files since we will later cover packages that offer more convenient ways to read the most common formats.  

## Writing Files

To write a file it must have been opened appropriately.  We can use `print` with the addition of a `file=` argument.
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
Here `seq` is a sequence, usually a list of strings.  The `writelines` will not add any end-of-line markers so as with `write`, they must be appended to every string in the sequence where a linebreak is desired.

## Closing Files

When you have completed all operations on a file you should close it.
```python
fin.close()
fout.close()
```

**Exercise**

Open a file 

```no-highlight
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
{{< code-download file="/courses/python-introduction/exercises/polynomial.py" lang="python" >}}
{{< /spoiler >}}


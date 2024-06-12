---
date : "2021-06-23T00:00:00"
title: "Advanced Input/Output"
toc: true
type: book
weight: 63

---

Simple stream input/output covers much of what we need for basic programs, but as we move to more sophisticated codes we may find that we need more control, especially for reading files.

## Stringstreams

An input/output stream is a sequence of characters.  A string is also a sequence of characters.  With the right definitions and methods, we can thus treat a string as a stream.  C++ does this with the `stringstream` class.
In particular, the insertion and extraction operators are defined on the stringstream object.

Stringstreams are very commonly used to convert from a string to a different type, often a numerical type, and vice versa.  As long as `>>` and `<<` know how to do the conversion for the target type, as they must for an ordinary istream or ostream objects, the operators can perform the same conversion for a stringstream.  Since the `>>` operator breaks on whitespace, it can also be used to split a string.

To convert _from_ string _to_ numeric, we create a new stringstream from the string.  We now have a _string buffer_ holding the string.  We now read from that buffer into the numeric variable using the extraction operator.
```c++
   std::stringstream ss(num_str) 
   float num;
   ss>>num;
```
To go the other direction we _write_ the number into an empty stringstream buffer.  We must then extract the string using a built-in method.
```c++
   std::stringstream st;
   float num=10.4;
   st<<num;
   std::string num_str=st.str();
```
Putting this together gives us this example:
{{< code file="/courses/cpp-introduction/codes/str_stream.cxx" lang="c++" >}}

## Reading from the Command Line

Command-line options are strings that follow the name of the executable.
```no-highlight
./myexec first second 10
```
The command line is contained in a two-dimensional character array (one dimension for the characters, the other for multiple character groups).  It is called `argv`.  The element `argv[0]` is the name of the executable.  The integer `argc` is the length of `argv`.  These variables must be specified as arguments to `main` if you wish to read command-line arguments.

We can read strings only.  You must convert if necessary to a numerical type using stringstreams.
{{< code-download file="/courses/cpp-introduction/codes/cl.cxx" lang="c++" >}}

### Getline

The `getline` function reads an entire line at once, as a single string.  This means that we will need to handle the input ourselves, converting as appropriate.
{{< code file="/courses/cpp-introduction/codes/getline_read.cxx" lang="c++" >}}

Getline's name is a little misleading.
Getline actually reads to a _delimiter_.  The default delimiter is newline      `\n.`
```c++
   getline(istream,string)  // reads to newline
   getline(istream,string,delim) // reads to delimiter character
```
The delimiter character is discarded from the string.

Getline can also be used for standard input.
Example:
```c++
  cout<<"Enter your name:";
  getline(cin,name);
```

### Reading a CSV file

* We often need to read files where each line contains several fields separated by a comma or other delimiter.  For example: read four values from each line for 200 lines, ignoring the second column values.

{{< code-download file="/courses/cpp-introduction/codes/read_csv.cxx" lang="c++" >}}

Getline is used twice, once to read the line as a string and again to split the line on commas.  In this case we know that we have four fields in each line so we declare an array of strings.  More generally, we could use a vector and `push_back` after getline reads the next chunk to the delimiter.  To read the subunits of the line, we declare a stringstream and use that as the stream buffer, rather than a file descriptor.

**Exercises**

1. Download the file [cpi.csv](/data/cpi.csv).  Examine the file.  Write a program that will read the file.  Store the first column in a vector `year` and the second column in another vector `cpi`.  Be sure to skip the header line.  It is not necessary to read any data from the header.  

As an alternative to converting with a stringstream, you can convert a C++ string to a C-style string with the `c_str()` method.
```c++
mystr.c_str()
```
You can then use the atoi and atof functions that operate on C strings to convert to integer and float respectively.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/read_cpi.cxx" lang="c++" >}}
{{< /spoiler >}}

2. Write a program that creates a file mydata.txt containing four rows consisting of
```
1, 2, 3
4, 5, 6
7, 8, 9
10, 11, 12
```
Rewind the file and read the data back.  Write a loop to add 1 to each value and print each row to the console.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/read_write_csv.cxx" lang="c++" >}}
{{< /spoiler >}}


---
title: "File Input/Output"
toc: true
type: book
weight: 62

---


## File Streams

Standard streams are automatically opened.  Other files must be opened explicitly.
Files can be input streams (ifstream), output streams (ofstream), or either/both (fstream).
```c++
#include <fstream>
```
or
```c++
#include <ofstream>
#include <ifstream>
```
as needed.

## Open

First a stream object must be declared.
```c++
  ifstream input;
```
Then the stream can be attached to a named file
```c++
  input.open(inFileName);
```
This assumes the file exists and is opened for reading only.

For output use
```c++
  ofstream output;
  output.open(outFileName);
```
This file will be emptied if it exists or created if it does not exist, and will be opened in write-only mode.

To open read/write use
```c++
   fstream myfile;
   myfile.open(myfileName);
```

## Modifiers

We can control the characteristics of the file with modifiers

* `ios::in`      open for input (read). Default for ifstream.
* `ios::out`     open for output (write). Default for ofstream.
* `ios::binary`  open as binary (not text)
* `ios::app`     append
* `ios::trunc`   if file exists, overwrite (default for ofstream)

Use a pipe (|) to combine them
```c++
   ofstream myfile;
   myfile.open("myfile.dat",ios::binary | ios::app);
```

## Inquiring

All inquiry methods return a `bool` (Boolean).  To check whether a file is open
```c++
   myfile.is_open()
```
To check whether a file opened for reading is at the end
```c++
   myfile.eof()
```

To test for any error condition
```c++
   myfile.good()
```

## Close

Much of the time, it is not necessary to close a file explicitly.  Files are automatically closed when execution terminates.
If many files are opened, it is good practice to close them before the end of the run.
```c++
   myfile.close();
```

## Rewind

An open unit can be rewound.  This places the _file pointer_ back to the beginning of the file.
The default is to rewind a file automatically when it is closed.

These are C-style functions and are in `<cstdio>`.
```c++
   rewind(mystream)
```
You can also seek to position 0
```c++
   fseek(mystream,0,SEEK_SET)
```
where `rewind` clears the end-of-file and error indicators, whereas `fseek` does not.

# Writing to a File

We write to a file much like to a standard stream.  In this example, we assume that var1, var2, and var3 are arrays or vectors of length `nlines`.
```c++
  ofstream out("outfile.txt");
  out<<"column1,column2,column3\n";
  for (int i=0;i<nlines;++i) {
      out<<var1[i]<<","<<var2[i]<<","<<var3[i]<<"\n';
  }
```

## Reading from a File

The extraction operator works on file objects as well as on cin:
{{< code file="/courses/cpp-introduction/codes/simple_read.cxx" lang="c++" >}}

Although this method works, it has a number of drawbacks.  As we learned for [console IO](/courses/cpp-introduction/console_io), the extraction operator assumes the separator is whitespace. It can be thrown off when attempting to convert a string into a numerical type.  It also is inflexible. We will discuss more advanced methods in the [next](/courses/cpp-introduction/advanced_io) chapter.

**Exercise**

Write a program that creates a file mydata.txt containing four rows consisting of
```no-highlight
1 2 3
4 5 6
7 8 9
10 11 12
```
Either rewind or reopen the file and read the data back.  Write a loop to add 1 to each value and print each row to the console.  Note that if you choose to rewind, the file will have to be opened read/write.  If you close it and reopen it, it will have to be reopened in write mode.  You may use a statically-sized array for the data.  In the next chapter we will learn a more flexible method of reading lines of files.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/read_write_file.cxx" lang="c++" >}}
{{< /spoiler >}}

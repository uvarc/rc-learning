---
title: Exceptions
toc: true
type: docs
draft: false
weight: 85
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: IO and Exceptions
---

Many situations can result in an error.  

* An attempt to read a file that isn't present.
* A attempt to cast a string to a number when the string does not represent a number.
* An index out of bounds.
* A key requested is not in the dictionary.

All these conditions (and many more) are called __exceptions__.  If you do not handle them, the interpreter will, and it will just stop with some error message.  We can prepare for failure by _catching_ exceptions ourselves.

### Catching Exceptions

The interpreter _throws_ an exception so we can _catch_ it.  We can do this with a `try except` block.  

```python
try:
    fin=open("datafile.csv","r")
except IOError:
    print("Unable to open file.")
```

This stops on the named exception `IOError`.  Any other exception will not be caught. 

It is not necessary to specify exceptions if you are unsure of the correct name; the following will be executed for any exception.
```python
try: 
    something
except: 
    print("Error")
```
More processing than we have shown is usually required, of course.  For example, if a file cannot be opened, we may choose to exit the script.
```python
try: 
    fin=open("datafile.csv","r")
except IOError:
    print("Unable to open file.")
    exit()
```

The interpreter provides a large number of built-in exceptions.  (See the [documentation](https://docs.python.org/3/library/exceptions.html) for a complete list.)  Some of the more commonly encountered are 
* EOFError
* IOError
* KeyboardInterrupt
* IndexError
* IKeyError
* NameError
* NotImplementedError
* TypeError
* ValueError
* ZeroDivisionError

You can provide more than one `except` for the same `try`

```python
try:
   input=int(input("Please enter an integer:"))
except EOFError:
   print("You did not enter anything")
except ValueError:
   print("You did not enter an integer")
```

If you want your script to perform some action upon passing the `try`, use `else`
```python
try:
    fout=open("outputfile.csv","w")
except IOError:
    print("Unable to open file")
    exit()
else:
    print("File successfully opened")
```

If you have code that should be executed regardless of whether the `try` passes or fails, use `finally`:
```python
try:
   fin=datafile.read()
except IOError:
   print("Unable to read ",datafile)
finally:
   fin.close()
```
You cannot use an `else` in any `try` clause that contains a `finally` block.

#### With/As

In many case, especially those having to do with file operations, the `with` and `as` operators can replace the `try`/`except`/`finally` block. The following will close the file whether the operation was successful or not.
```python
with open('myfile','w') as f:
    for line in f:
        print(line)
```

**Exercise**

Download the file [numbers.csv](/data/numbers.csv). Write a program to read it and extract the columns as x,y.  It has a one-line header.  What problem did you encounter?  Use an exception to exclude the line with the error.  

Optionally, add exception handling to the file operations (opening, reading, etc.)

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/exceptions_example.py" lang="python" >}}
{{< /spoiler >}}


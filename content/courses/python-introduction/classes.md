---
title: Classes
toc: true
type: docs
draft: false
weight: 201
date: "2020-11-17T00:00:00"

menu:
    python-introduction:
        parent: Object-Oriented Programming
---

A __class__ is a generalized, programmer-defined data structure that can contain multiple variables under one name, as well as functions that work on the data represented by those variables. These programmer-defined types are the foundation of _object-oriented programming_.  Python is an object-oriented language even though most programmers mostly use classes written by others, so it is useful to understand the basics.  

This "object" is a concept; the class is its most common representation as code.  Strictly speaking, an object must contain both data (variables) and procedures (functions) and it must be possible to declare variables of its type.  Declaring a variable of a class is called _instantiation_ and each variable is called an _instance_ of the object/class.  This is a key difference between a class and module; as we have seen, a module also contains variables and functions, but no variables can be declared of its type.

Variables and functions defined in a class are collectively called __members__ of the class.  The variables are usually called __attributes__ while the functions are called __methods__.  The methods describe the _interface_ to the class.  Attributes and methods are accessed through the instances of the class.  As long as the interface is not changed, we can make whatever changes to the class that may be required, and other code that uses the class will not be affected. 

Classes may have two special methods called __constructors__ and __destructors__.  Constructors are called when a new instance is created and perform whatever setup work is needed.  If the language supports a destructor, it removes an instance and releases the memory it occupies.

### Classes in Python

We have already been using classes in our Python program, since all data types (including the apparently "primitive" types) are actually classes. In Python syntax, an instance is separated from its methods by a period.  So when we invoke
```python
L=[1,2,3,4]
```
we have invoked a constructor (in an unusual format) for the list class.  When we subsequently type

```python
L.append(5)
```

we are calling the `append` _method_ on the _instance_ `L` of the _class_ `list` to add an element at the end of the list represented by `L`.

### Syntax

Classes are always defined in a module.  Frequently only the class is in the module, though this is not required.  The keyword is `class`.  The class body, including its docstring, is indented starting one level from the margin.  Classes without methods are allowed in Python, but normally the class will at least contain a constructor.  Instances are defined with

```python
new_inst=MyClass()
```

Calling the class by name in the form of a function automatically invokes the constructor.  If there is no constructor, this creates an empty instance.

As for modules, any variables defined outside of and above the methods are _global_ to the class. All methods can see them.

Example:  

{{< code-download file="/courses/python-introduction/scripts/mymodule.py" lang="python" >}}

The first line defines the class name.  The next line is the docstring.  After that we define and initialize a variable that is global to the class.  The first method is the constructor.  The ultimate class constructor is always named `__init__` (two underscores).  The first argument to init, and to all methods in Python, is the _instance variable_.  The next two arguments to the constructor are actually passed from the caller.  They are used to initialize the two attributes `x` and `y`.  Notice that `self.x` and `x` are completely different variables; similarly for `self.y` and `y`.

Next is a method that acts only upon the instance.  Note that it does _not_ return the instance.  Instance variables are never returned from a method of their own class.  Traditionally None is returned (explicitly or by default).  Finally, we have a function that performs a computation using a class attribute, and returns the result to the caller.  The global variable `i` is referenced with the class name, not with self, because it is a _class variable_ and not an attribute of an instance.

### Self

The first argument to all class methods _must_ be the instance variable, which is a placeholder for the instance on which the method was invoked. This variable is always called `self`.  Unlike some other object-oriented languages such as C++, the instance variable must always be explicitly present in the argument list. It is not used when we invoke the method, but is understood.

```python
thing=MyClass(x,y)
thing.reset(w,z)
```

### Constructor

The constructor `__init__` is invoked when a new instance is declared.  It need not take any arguments from the caller at all, though it will always include `self` in its parameter list.  Any attributes to be declared should be set to some default value in the constructor.

**Exercise**

Type in the example class.  Save it into a file called `mymodule.py`.  Run the file or cell.  In your interpreter window or a new cell, type

```python
from mymodule import MyClass
mc=MyClass(12,14)
x=12
print(x,mc.x)
mc.reset(19,11)
print(x,mc.x)
a=mc.addit(13)
print(a)
```

In the interpreter you can create new attributes dynamically; they will not, however, be preserved in your class module.

```python
x=MyClass(11.,13.)
x.counter=1
print(x.counter,x.i,x.reset(12,15))
```

## Class Methods

Class methods access the _class_ variables rather than the _instance_ variables.  This may seem obscure, but class methods are fairly widely used to create _alternate constructors_.  For example, suppose we wish to load values into an instance, but in some cases we want to read those from a file.  We are now faced with something of a chicken-or-egg situation; we need the constructor to create the instance, but if we don't already have the values we can't call the constructor.  We do not wish to write a function outside the class since we want to keep everything encapsulated.  A class method solves this problem.  

To mark a class method we use `@classmethod` ahead of the `def` statement.  Statements beginning with the `@` symbol are called __decorators__ and have other uses.

Here is a code snippet for our example:

```python
class pData:
    def __init__(self,x,y,z,t):
        set_values

    @classmethod
    def fromfile(cls,filename):
        read_values_from_filename
        return cls(x,y,z,t)

aDataPoint=pData.fromfile(inputfile)
```

The alternate constructor must return the new instance in order to invoke the constructor.

## Pickling

Objects may have a more complicated state than the simple variables we have encountered so far.  Saving it correctly could be tedious and error-prone.  Python provides an operation called "pickling" that coverts the state of an instance into a linear stream of bytes that can be stored or read back.

```python
import pickle
aclass=MyClass(11)
f=open("myresults.dat","w")
pickle.dump(aclass,f)
```

To restore, use 

```python
aclass=pickle.load(f)
```

### Dill

The pickle module is built into Python but it is limited.  It cannot handle several built-in types.  The dill package overcomes these limitations.  For consistency it is even possible to import it as the pickle namespace.

```python
import dill as pickle
```
Dill is not included in the base Anaconda but can be installed through [conda](/courses/python-introduction/package_managers).

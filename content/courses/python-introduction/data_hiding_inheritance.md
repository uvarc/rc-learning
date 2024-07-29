---
date: "2020-11-17"
title: "Data Hiding and Inheritance"
weight: 210
---

In object-oriented languages that are stricter than Python, class data may be _public_ or _private_.  Public members are directly accessible from an instance.  Private attributes can only be accessed through methods; the data is said to be _encapsulated_ within the object.  Private methods can only be accessed by other methods.  This is to prevent outside code from changing the attributes, or the results of methods, without a "message" to the class instance being sent.  

### Data Hiding in Python

Python does not enforce this but does have a mechanism for "hiding" some members.
All symbols beginning, but not ending, in two underscores are not accessible through an instance.  They can only be utilized through a method.  Symbols beginning with a single underscore are understood to be part of the _implementation_ and not the interface.  Outside code must not rely on them and should rarely to never use them directly.

Example: 

{{< code-download file="/courses/python-introduction/scripts/mymodule_private.py" lang="python" >}}

This makes a difference. 

```python
ac=MyClass(19.,20.)
ac.addit(30.)
ac._i
ac.__redo()
```

The last line will result in an AttributeError: MyClass instance has no attribute '\_\_redo'.

However, it is not absolute:

```python
ac._MyClass__redo()   #impolite!
print(ac.x,ac.y)
```

### Accessors and Mutators

To handle "private" or even just "implementation" variables we use methods.  Accessors ("getters") get the value and return it.  Mutators ("setters") change the value.

{{< code-download file="/courses/python-introduction/scripts/privateclass.py" lang="python" >}}

## Inheritance: New Classes from Old

One of the cornerstones of Object-Oriented Programming is _inheritance_.  New
classes can be _derived_ from existing classes; the lower-level class is called the _base class_.  The derived class inherits the attributes and methods from its parent. The new class is said to be a _subclass_ of its parent.

Inheritance facilitates code reuse and extension of code functionality while minimizing the modifications required to the original code.  The new class may add members, both attributes and methods, along with the inherited ones.  It can also _override_ the methods of the base class, adapting them to the requirements of the new class. No changes to the base class are required.

The relationship between the base class and the derived class can be understood as "Child IS_A Parent."

**Examples**
```no-highlight
A Sparrow IS_A Bird
An Employee IS_A Person
```

Let us consider a more detailed example. An important object in a forest model is a tree.  An individual tree will have particular attributes depending on the species, and they may have additional behaviors in some cases, but they will have a number of attributes in common.  We can define a class Species which might contain attributes such as
 - genus
 - species
 - wood_density
 - max_life_expectancy
 - max_height
The methods would include behaviors such as
 - sprout
 - grow
 - die

A Tree would add members specific to an individual tree, such as
 - diameter
 - height
 - branch

In Python, we indicate the parent with parentheses
```python
class Child(Parent):
```

**Example**

Suppose we wish to develop code for an employee class when we already have a Person class.  Person defines general data such as name and address. The additional attributes for Employee will be the employee's salary and ID number.

{{< code-download file="/courses/python-introduction/scripts/person_module.py" lang="python" >}}


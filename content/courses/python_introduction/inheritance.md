---
title: Data Hiding, Inheritance, Polymorphism, and Operator Overloading
toc: true
type: docs
draft: false
weight: 210

menu:
    python_introduction:
        parent: Object-Oriented Programming
        weight: 210
---

## Data Hiding

In stricter object-oriented languages, class data may be _public_ or _private_.  Public members are directly accessible from an instance.  Private attributes can only be accessed through methods; the data is said to be _encapsulated_ within the object.  Private methods can only be accessed by other methods.  This is to prevent outside code from changing the attributes, or the results of methods, without a "message" to the class instance being sent.  

### Data Hiding in Python

Python does not enforce this but does have a mechanism for "hiding" some members.
All symbols beginning, but not ending, in two underscores are not accessible through an instance.  They can only be utilized through a method.  Symbols beginning with a single underscore are understood to be part of the _implementation_ and not the interface.  Outside code must not rely on them and should rarely to never use them directly.

Example: 

{{< code-download file="/courses/python_introduction/scripts/mymodule_private.py" lang="python" >}}

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

{{< code-download file="/courses/python_introduction/scripts/privateclass.py" lang="python" >}}

## New Classes from Old

One of the cornerstones of Object-Oriented Programming is _inheritance_.  New
classes can be _derived_ from existing classes; the lower-level class is called the _base class_.  The derived class inherits the attributes and methods from its parent.  
Inheritance facilitates code reuse and extension of code functionality while minimizing the modifications required to the original code.  
The new class may add members, both attributes and methods, along with the inherited ones.  It can also _override_ the methods of the base class, adapting them to the requirements of the new class. No changes to the base class are required.

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

{{< code-download file="/courses/python_introduction/scripts/person_module.py" lang="python" >}}

## Polymorphism and Method Overriding 

_Polymorphism_ means literally "having many forms."  In computer science, it is when the same interface can be used for different types.  In most cases the interface is a function name.  Many of the built-in Python functions are polymorphic; the `len` function can be applied to lists, dictionaries, and strings. Polymorphic functions are often said to be _overloaded_.  The function's _signature_ is the unique description of the number and type of the arguments, and if relevant, the class to which it belongs.  The signature is the means by which the interpreter determines which version of the function to apply. This is called _overload resolution_.

A particular type of polymorphism is _subtype polymorphism_.  We can define a function in a subclass that has the same name as in the base class, but which applies to instances of the subclass.   This is _overriding_ the method.  

**Example**
{{< code-download file="/courses/python_introduction/scripts/zoo.py" lang="python" >}}

Notice that in this code the base class throws a `NotImplementedError` if it is invoked with an instance of Animal.  In our code, the Animal class is not intended to be used to create instances, and we do not have a base implementation of `speak`, so we make sure to warn the user of our class.

Since Python is mostly dynamically typed, the correct polymorphic instance is determined  at runtime.
Python uses “duck typing” (if it walks like a duck and quacks like a duck...);  the type is determined dynamically from context.  If your usage does not agree with what the interpreter thinks it should be, it will throw a type exception.

These built-in functions specify whether a given object is an instance of a particular class, or is a subclass of another specified class. All return Booleans.

In Jupyter or Spyder, enter the classes shown above, then in the interpreter window or cell type these lines.  (Recall that `>>>` is the interpreter prompt; you may see something different.)
```python
>>>print(issubclass(Animal, Canine))
>>>print(issubclass(Canine, Animal))
>>>print(issubclass(Animal, Feline))
>>>print(issubclass(Feline, Canine))
>>>print(issubclass(Feline, Feline))
>>>print(isinstance(zoo[0], Animal))
>>>print(isinstance(zoo[0], Canine))
>>>print(isinstance(zoo[0], Feline))
```

## Operator Overloading

Operators are themselves functions and can be overloaded.  In Python the arithmetic operators are already overloaded, since floats and integers are different within the computer.  The addition operator `+` is also overloaded for other purposes, such as to concatenate strings or lists.

```python
print(1+2)
print(1.+2.)
print("1"+"2")
```

We can overload operators in our classes so that we can, say, add two instances of our class.  Of course "adding" the instances should make sense. Python defines a number of _special methods_ which are distinguised by having a double underscore before and after the name; for this reason they are sometimes called "dunders" or they may be called "magic methods."  We have already encountered the `__init__` dunder but there are many others.

**Example**
We would like to create a Point class to define points in a three-dimensional Euclidean space.  Points are added by adding corresponding components; i.e.
$$
p1=(1,2,3),\  p2=(7,8,9),\ p1+p2=(8,10,12)
$$

To implement addition we will use the `__add__` dunder.  This dunder will take `self` as its first argument, another instance of Point as its second, and it must return another instance of the class Point, so we invoke the constructor in the dunder.

{{< code-download file="/courses/python_introduction/scripts/points.py" lang="python" >}}

We could similarly define subtraction with `__sub__`.

**Exercise**

Implement subtraction for points using the rule $x_1-x_2$, $y_1-y_2$, $z_1-z_2).
{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/exercises/points.py" lang="python" >}}
{{< /spoiler >}}

We'd like to be able to print a Point object in the standard mathematical format $(x,y,z)$.  To allow `print` to handle our class we overload the `__str__` dunder.  The `__str__` dunder is used by `str`, `print`, and `format`.  

{{< code-download file="/courses/python_introduction/scripts/points_printer.py" lang="python" >}}

There are many other dunders we can use in our classes.  Multiplication and division don't make sense for points, but they can be defined with `__mul__` and `__truediv__` respectively.

If we define the comparison dunders then we can invoke `sort` on our class instances.

A list of the most widely used magic methods is [here](https://python-course.eu/oop/magic-methods.php).

### Resources

A longer discussion of OOP in Python is available [here](https://www.python-course.eu/python3_object_oriented_programming.php).

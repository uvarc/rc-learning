---
title: Polymorphism and Operator Overloading
toc: true
type: docs
draft: false
weight: 220

menu:
    python-introduction:
        parent: Object-Oriented Programming
---

_Polymorphism_ means literally "having many forms."  In computer science, it is when the same interface can be used for different types.  In most cases the interface is a function name.  Many of the built-in Python functions are polymorphic; the `len` function can be applied to lists, dictionaries, and strings. Polymorphic functions are often said to be _overloaded_.  The function's _signature_ is the unique description of the number and type of the arguments, and if relevant, the class to which it belongs.  The signature is the means by which the interpreter determines which version of the function to apply. This is called _overload resolution_.

### Method Overriding

A particular type of polymorphism is _subtype polymorphism_.  We can define a function in a subclass that has the same name as in the base class, but which applies to instances of the subclass.   This is _overriding_ the method.  

**Example**
{{< code-download file="/courses/python-introduction/scripts/zoo.py" lang="python" >}}

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

{{< code-download file="/courses/python-introduction/scripts/points.py" lang="python" >}}

We could similarly define subtraction with `__sub__`.

**Exercise**

Implement subtraction for points using the rule $x_1-x_2$, $y_1-y_2$, $z_1-z_2).
{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/points.py" lang="python" >}}
{{< /spoiler >}}

We'd like to be able to print a Point object in the standard mathematical format $(x,y,z)$.  To allow `print` to handle our class we overload the `__str__` dunder.  The `__str__` dunder is used by `str`, `print`, and `format`.  

{{< code-download file="/courses/python-introduction/scripts/points_printer.py" lang="python" >}}

There are many other dunders we can use in our classes.  Multiplication and division don't make sense for points, but they can be defined with `__mul__` and `__truediv__` respectively.

If we define the comparison dunders then we can invoke `sort` on our class instances.

A list of the most widely used magic methods is [here](https://python-course.eu/oop/magic-methods.php).

### Resources

A longer discussion of OOP in Python is available [here](https://www.python-course.eu/python3_object_oriented_programming.php).

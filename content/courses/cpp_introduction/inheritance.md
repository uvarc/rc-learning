---
title: "Class Inheritance"
toc: true
type: book
weight: 93
---

One of the foundations of object-oriented programming is _inheritance_.  Often, two objects are related and we can save coding by reusing code from one object in another.  Rather than retyping or cutting and pasting, we can simply pass down the members of one object to another.  This can continue in an _inheritance chain_ indefinitely, though it would be poor practice to set up more than a few links in the chain.   
The lower-level link is called the _base class_ (or _parent_) and the subsequent one is called the _derived class_ (or _child_).  

Inheritance should reflect an IS-A relationship between the two objects.  A cat _is-a_ animal.  A rectangle _is-a_ shape.  

## Member Inheritance

Members declared in the base class cannot be inherited if they are declared `private`.  They must be `public` or `protected` to be transmitted.

The syntax to declare a derived class is
```no-highlight
class child-class: access-mode parent;
```
The access category is one of `private`, `public`, or `protected`.  If not specified the usual default of `private` is assumed.  

C++ permits multiple parents, but this is generally discouraged.
```no-highlight
class child-class: access-mode parent1, access-mode parent2;
```
The `access-mode` specifier for the base class affects how the members of that class are transmitted to the derived class. If it is `public` the public and protected members of the base class retain that access mode in the derived class.  If it is `protected`, both public and protected members are transmitted as `protected`.  In the private case, _all_ members are transmitted as `private`.  In all cases, private members in the base class are not accessible directly to the derived class and an accessor must be provided if they should be available.  Declaring the base class `public` is most common due to the restrictions imposed by the other access modes.

**Example**

{{< code-download file="/courses/cpp_introduction/codes/inheritance.cxx" lang="c++" >}}

The child inherits the constructor and `getName` accessor from the parent.
But `age` does not refer back to the parent, since that variable occurs only in the child, so it must be explicitly declared.
The constructor for the Child class invokes the Parent constructor and then sets the age.  

## Constructors

Constructors, destructors, and friend classes are not inherited from the base class by derived classes.  We can still create constructors, and the derived class can invoke the parent constructor.

{{< code-download file="/courses/cpp_introduction/codes/child_constructor.cxx" lang="c++" >}}


---
title: "Encapsulation and Data Hiding"
toc: true
type: book
weight: 92

---

There is more to object-oriented programming and classes than attributes and methods.  The basic principles are _encapsulation_, _abstraction_, _inheritance_, and _polymorphism_.  When we learned to write a C++ class, we were learning to deal with abstraction; how to express in code the _abstract data types_.  Now we will deal with encapsulation.

## Data Hiding

In object-oriented programming, encapsulation may refer to bundling of variables and procedures (attributes and methods) into a cohesive unit, but also to _data hiding_, in which internal variables are kept internal to the objects and must be accessed through methods.
This is to prevent outside units from accessing members in an uncontrolled way.
Making a member _public_ “exposes” it and allows anything that uses the class direct access to the member.
Typically, attributes are _private_ and methods are public.
Methods can be written to obtain or change the value of an attribute.  These are often called _accessors_ and _mutators_, or more informally "getters" and "setters."
(C++ also defines a category called _protected_, which we will discuss in more detail later.)

public
  * Accessible directly through an instance (myobj.var)
private
  * Accessible only within the class or to "friend" classes.  Must be communicated outside the class through an accessor and changed through a mutator.  This is the default for a C++ class member.
protected
  * Private within the class, accessible to "friend" classes and to descendant classes.

The default in C++ classes is `private` so we must explicitly declare members `public` if we wish them to be exposed.  Typically all or most attributes are kept private, whereas the methods must be public.  The default access class applies until an access label is encountered; subsequently the new access applies.
```c++
class MyClass {
   double var1;
   double var2;
   public:
      int var3;
      MyClass();
      set_var1(double);
      set_var2(double);
};
```
The attributes `var1` and `var2` are private by default.

Example:
Download and run the following code.  The exact error will depend on the compiler, but it should detect an illegal attempt to access a private variable directly.
{{< code-download file="/courses/cpp_introduction/codes/illegal_access.cxx" lang="c++" >}}

### Benefits of Data Hiding

We may ask what we gain from data hiding.  Consider an instance `foo` of a class `FooBar` containing an attribute `bar`.  If it is public we can change it with
```c++
foo.bar=101;
```
However, what if 101 is an invalid value for this variable?  No checking will occur and we may have introduced a bug that could be very difficult to find.  If we force the program to use a mutator
```c++
foo.set_bar(101);
```
then the `set_bar` method can perform checks on the input, take any other appropriate actions when `bar` is set or reset, and so forth.  

Once it is declared private, then we must also have an accessor if we need to know its value.

## Structs and Classes

We did not go into details in our discussion of structs, but in C++ structs may contain methods.  The salient difference between a struct and a class is the default access control.  Struct members are public by default, whereas class members are private by default.

If structs and classes are so similar, why have both?  Structs were inherited from C, with methods added as an extension, whereas classes are specific to C++.  The choice of a struct or a class is up to the programmer's personal style or coding standards established for a project; there are no hard and fast rules.
However, many programmers use structs when it is natural for all members to be public, especially if the struct consists mostly of methods, or for when the data type represents _POD_ or "plain old data."  The definition of POD is somewhat fluid but often refers to basic types such as are available in C.  This would limit it to primitive types such as numbers as well as `char` (but not string). This can be useful for interfacing with C libraries.

## Friends

We have stated that private and protected members are accessible to "friends" without explaining what those are.  Functions and classes may be "friends" of a class.  Friends have access to all members of their buddies.
A friend may be a function or a class.

### Friend Functions

A friend function is a function that is not part of the class.  It must be declared `friend`.

{{< code file="/courses/cpp_introduction/codes/testfriendfunc.cxx" lang="c++" >}}

Here `color` is a function that we might use elsewhere; it does not belong in the class, but we would like the class to be able to invoke it directly with its private attributes.

### Friend Classes

Classes may also be friends with other classes.
{{< code file="/courses/cpp_introduction/codes/testfriend.cxx" lang="c++" >}}

Here we have an empty class declaration (a "prototype" of sorts) because ClassB contains a method that uses an instance of the class so the compiler needs to be aware of its existence.  We must define ClassA after ClassB, however, because ClassB is a friend so ClassA must know its definition.  

Classes may use instance of other classes.  This is called "composition" and we will discuss it in more detail along with [inheritance](/courses/cpp_introduction/inheritance).

"Friendship" is not mutual unless explicitly declared; in the above example ClassB is a friend of ClassA, but ClassA is not a friend of ClassB.  

**Exercises**

1. Correct `illegal_access.cxx` by adding an accessor and mutator for the private variable.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/cpp_introduction/solns/access_demo.cxx" lang="c++" >}}
{{< /spoiler >}}

2. Modify your Employee class to use appropriate access levels for attributes and methods.  Add any "setters" and "getters" that you may need.  Place the interface into a `.h` file and the implementation into a corresponding `.cxx` file.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp_introduction/solns/class_struct_example/Employee.h" lang="c++" >}}
{{< code-download file="/courses/cpp_introduction/solns/class_struct_example/Employee.cxx" lang="c++" >}}
{{< code-download file="/courses/cpp_introduction/solns/class_struct_example/employees.cxx" lang="c++" >}}
{{< /spoiler >}}


---
date : "2021-06-23T00:00:00"
title: "Object-Oriented Programming and Classes"
toc: true
type: book
weight: 91

---

## Object-Oriented Programming

An __object__ is a data structure which has associated _data_ (variables) and _behaviors_ (procedures).
Objects work on their own data, communicating with outside units through an interface.
Objects _encapsulate_ related concepts and keep them unified.  They may _hide_ data from code outside the object.
Generally speaking, an object would contain the procedures required to update and maintain its _state_. 
In most object-oriented programming languages, objects are abstract concepts that are represented in code by _classes_. 

Object-oriented programming (OOP) grew from programming fields that simulated real-world, physical objects and it may be easiest to grasp by thinking of that model.  These objects are well-defined and their interactions can be managed through the _interface_ that each object provided.  A user interface is a good example.  We have various objects such as figures, text, menus, and so forth. The figures might be one-dimensional (lines, arrows, etc.) or two-dimensional (rectangles, ellipses, etc).  They have characteristics such as length/radius, they have a state such as current size, color and location of their centers, and they have behaviors such as moving or rotating.  Text has location and the content of the string that it displays.  All these objects interact with each other, as the user takes various actions or the program executes.

For another example, return to the Employee struct.  The struct collected data about an employee into a unified type.  However, actions may be associated with an employee.  Salaries could be raised, addresses could be updated, employees could join or quit the company, and so forth. We would implement procedures to carry out these actions and include them in the object.

## Classes in C++

We define a class with the keyword `class`.
A variable that is a member of the class is often called an _attribute_.  A _method_ is a procedure that is a member of a class.  An invocation of a method may be called _sending a message_.
The class definition contains full declarations of attributes, but only the 
prototypes of the methods.
For now we will declare all members `public` so that they will be directly accessible by code that uses the class.
```c++
class MyClass{
   public:
     double var1, var2;
     int var3;
     double function1(double x, double y);
     double function2(double x);
};
```
The methods are defined using the scoping operator as `class::function`.
```c++
double MyClass::function1(double x,double y) {
    return var1*x+var2*y;
}
double MyClass::function2(double x) {
   var1=x*var2;
   return;
}
```
Notice that `function2` does not return anything explicitly.  This is because it acts upon a member of the class.  Such methods should not return their results.  In contrast, `function1` returns a value to the outside caller. 

An _instance_ of a class is a variable of that class.  Objects are represented in code by instances and sometimes the terms are used somewhat interchangeably.
```c++
MyClass A, B
```
A and B are instances of MyClass.   

As for structs, we reference the members through the `.` operator.
```c++
   A.var1=10.;
   A.var2=11.;
   A.var1=7;
   A.function1(2.,9.);
```

### Constructors and Destructors

A class always has a _constructor_ and a _destructor_.  If not provided by the programmer, the compiler will try to fill them in.
The constructor is automatically called when an instance of the class is created.
The constructor is invoked when a variable of the class type is declared.
For a declaration of type pointer-to-class the constructor is called by the `new` operator.
The constructor has the same name as the class.
The destructor is called when the object is released.
The destructor has the same name as the class but preceded by `~`.
Explicit destructors are often not required.

```c++
class MyClass{
   public:
      double var1, var2;
      int var3;
      MyClass(double v1, double v2, int v3);
      ~MyClass(); // destructors never have arguments
      double function1(double x, double y);
      double function2(double x);
};

MyClass::MyClass(double v1, double v2, int v3) {
   var1=v1; var2=v2; var3=v3;
   return;
}

MyClass::~MyClass(){
//Not much to do in this example, usually would not implement
}

double MyClass::function1(double x,double y) {
   return var1*x+var2*y;
}

double MyClass::function2(double x) {
   var1=x*var2;
   return;
}
```

Like a struct, a class is a scoping unit.  Variables belonging to the class are in scope only within the class (specifically, its instance).

## this

An _instance variable_ is always passed to a method.  This variable stands for the current instance, that on which the method is invoked.  Some languages such as Python require that it be the first argument to any method.
In Python it is conventionally represented by `self`.
```python
def a_method(self,x,y):
   self.z=x+y
```
C++ does _not_ pass the instance variable explicitly.
If you need access to it in a method, use the `this` pointer.
Since it is a pointer, it requires the arrow operator.

One example where it might be required is using the same variable name for an argument and an attribute.
```c++
MyClass::MyClass(x,y,z) {
   this->x=x;
   this->y=y;
   this->z=z;
}
```

**Exercise**

Convert the Employee struct to a class.  Write a constructor for it. Incorporate an `updateSalary` method that takes a raise percentage (or fraction, your choice), computes the new salary, and updates the employee attribute. Why does it not have to take the employee's
current salary as a parameter?

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/employees.cxx" lang="c++" >}}
{{< /spoiler >}}


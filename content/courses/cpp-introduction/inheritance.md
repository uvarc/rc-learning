---
date : "2021-06-23T00:00:00-05:00"
title: "Class Inheritance"
toc: true
type: book
weight: 93
---

One of the foundations of object-oriented programming is _inheritance_.  Often, two objects are related and we can save coding by reusing code from one object in another.  Rather than retyping or cutting and pasting, we can simply pass down the members of one object to another.  
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
The `access-mode` specifier for the base class affects how the members of that class are transmitted to the derived class. If it is `public` the public and protected members of the base class retain that access mode in the derived class.  If it is `protected`, both public and protected members are transmitted as `protected`.  In the private case, _all_ members are transmitted as `private`.  In all cases, private members in the base class are not accessible directly to the derived class and an accessor must be provided if they should be available.  Declaring the base class `public` is the most common due to the restrictions imposed by the other access modes.

**Example**

{{< code-download file="/courses/cpp-introduction/codes/inheritance.cxx" lang="c++" >}}

Notice that an antelope _is-a_ animal and a reptile _is-a_ animal, but an antelope is *not* a reptile.  So derived classes do not need to have a direct relationship with one another aside from their inheritance.

We could introduce another level of derivations since "Reptile" is a broader category than "Antelope."  For example, we could declare class Mammal
```c++
class Mammal: public Animal {
   protected:
      string furColor;
      string order;
      string getOrder();
};
```

We could then derive Antelope from Mammal.  Further subdivisions would be possible (`class Ungulate` and so forth).
Derived classes can continue like this in an _inheritance chain_ indefinitely, though it would be poor practice to set up more than a few links in the chain.   

{{< diagram alt="A diagram showing how classes are inherited. Animal is the parent of Reptile and Mammal, and Mammal is the parent of Antelope.">}}
graph TD;
A(Animal) --> B(Reptile)
A(Animal) --> C(Mammal)
C(Mammal) --> D(Antelope)
{{< /diagram >}}

## Constructors

Constructors, destructors, and friend classes are not inherited from the base class by derived classes.  We can still create constructors, and the derived class can invoke the parent constructor.

{{< code-download file="/courses/cpp-introduction/codes/child_constructor.cxx" lang="c++" >}}

In this example, the child inherits the `getName` accessor from the parent.
But `age` does not refer back to the parent, since that variable occurs only in the child, so it must be explicitly declared.
The constructor for the Child class invokes the Parent constructor and then sets the age.  

In C++11 the `using` keyword may be employed to bring in the base constructor.
```c++
class Child: public Parent {
    private:
        int age;
    public:
        using Parent::Parent;
        void setAge();
        int getAge();
};
```

The Parent constructor cannot be used to set the new member `age`, so a mutator would be defined.  For this reason, some software engineers recommend keeping the older parent constructor syntax if the derived class defines its own constructor.

**Exercises**

1. Add a new class Mammal as sketched above.  Derive Antelope from that. Add an attribute `scaleColor` to the Reptile class.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/inherit_chain.cxx" lang="c++" >}}
{{< /spoiler >}}

2. Create a constructor for Animal that sets the name, food, foodQuantity, and vocalization.  Pass it through to the descendant classes and in each one, add the attributes new to that class.  Remove functions made unnecessary by the constructor.  Optionally implement the `using` syntax in the Antelope class.  Depending on your compiler version, you may need to add a flag `-std=c++11` or equivalent.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/constructor_chain.cxx" lang="c++" >}}
{{< /spoiler >}}

*Extra Exercises*
Clean up the solution to Example 2 by declaring attributes private or protected and implementing all required accessors and mutators.  

Optionally, implement Animal in its own interface and implementation files.  Include its implementation header into the source with 
```c++
#include "animal.h"
```
You will need to compile your source files separately and link `animal.o` appropriately.

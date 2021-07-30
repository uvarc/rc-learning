---
title: "Containers and Template Libraries"
toc: true
type: book
weight: 44

menu:
    cpp_introduction:
        parent: Containers and Template Libraries
        weight: 44
---

A _container_ is a data structure that can store a group of items, which typically are related and are individually accessed by standard methods.  By this definition a simple array can be considered a container, though some strict computer-science definitions disallow a data structure with a fixed size from the category.  C++ containers generally permit resizing of the data structure by inserting or deleting elements or even by clearing all elements.

C++ implements most containers as _templates_.  Templates are an implementation of _generic programming_.  In generic programming, algorithms are expressed in a way that can apply to more than one type.  Templates can be _functions_ or _classes_ and, for C++14 and later, variables.  We will cover classes [later](/courses/cpp_introduction/classes) but in brief, a class is a data structure that encompasses related variables and the procedures that operate on them.  We can declare variables whose type is the class; this is called _instantiation_ and each variable is an _instance_ of the class.  We then access the variables and procedures through the name of the variable.  For a template class, we pass through a type for the variable as a parameter when it is declared.
```c++
  myTemplateClass<float>  myfloat;
  myTemplateClass<double> mydouble;
```
The angle brackets _are_ included and enclose the specific type.

Associated with many templated containers are _iterators_.  An iterator is a variable that can be used to access elements.  It has properties similar to pointers.  It can be incremented by adding integers to the current value and dereferenced with the `*` operator.  

Most C++ programmers primarily use templates through _libraries_.  These libraries provide many data structures and perform a number of tasks that occur frequently in programming, thus relieving every programmer of the effort of writing his or her own code. 

## The Standard Library

The C++ Standard Library is defined as part of the language definition.  It incorporates much of the _Standard Template Library_ or STL.  The STL was initially an add-on to C++, [developed](https://en.wikipedia.org/wiki/History_of_the_Standard_Template_Library) with the intention of exploring generic programming.  
The standard library is huge, with many subcomponents.  We have been using it regularly already; every invocation of it requires the prefix `std::`.
```c++
#include <iostream>
int main() {
    std::cout<<"Hello World\n";
}
```
A few examples we have seen so far have employed the shortcut `using`, which we will discuss when we study [scope](/courses/cpp_introduction/scope), but although common practice, often it is best to avoid it.
```c++
#include <iostream>
using namespace std;
int main() {
    cout<<"Hello World\n";
}
```

The standard library consists of _containers_, _algorithms_, _iterators_, and a number of general-purpose and numerical sublibraries.  A comprehensive list is [here](https://www.cplusplus.com/reference/).

### Useful Standard Containers

Several templated containers are available in the standard library.

#### Array

The array container "wraps" C-style fixed-size arrays.  Unlike a bare array, the container carries metadata, including the array size and other descriptive elements.  It also does not "decay" to a pointer when passed to a function, meaning that it doesn't "forget" the metadata within the function.

{{< code file="/courses/cpp_introduction/codes/std_array.cxx" lang="cxx" >}}

A standard array is a sequence and we can use a range-based `for` loop with it.  Note also the peculiar declaration of the loop variable in the three-element loop; this is required because the array container defines its size and index variables to be of an _unsigned_ int of type `size_type`. Strictly speaking, it should always be used for loop variables for standard containers, since they are defined internally that way.  The intention was to allow for very large array sizes without overflowing a loop variable.  A standard `int` or `long` will nearly always work, and the compiler will not complain, but best practice is to make the loop variable match the internal definition.  The `size_type` depends on the vector template so would have to be declared `std::vector<float>::size_type`; a shortcut that is safe, but still less wordy, is to use `std::size_t`.
```c++
   for (std::size_t i=0; i<vec.size(); i++) {
      //code
   }
```
The std::size_t type is guaranteed to be large enough to index any object possible within the limitations of the compiler.

The `array` container has some drawbacks.  It is one dimensional only; no multidimensional arrays are allowed.  It cannot be dynamically allocated (there is some capability for that in the C++20 standard, but it is limited); only literals or const variables are allowed for its declaration.  Therefore, we will look at another, very widely used, container, the `vector`.

#### Vector

The vector container is similar to the array. It represents a one-dimensional ordered sequence of elements. Elements are accessed by integers 0..N-1 (for size N).
But unlike arrays, vectors are dynamic.  It's possible to enlarge and shrink them.

Initializing vectors:
{{< code-download file="/courses/cpp_introduction/codes/std_vector.cxx" lang="c++" >}}

Many operations are defined for a vector.  These are some of the most commonly used:
|  V.push_back(item)  |  Append `item` to V |
|  V.at(index)        |  Access `index` with bounds checking ([] does no checking |
|  V.start()          |  Starting point for iterator  |
|  V.end()            |  End point (beyond last element) of iterator |
|  V.size()           |  Number of elements
|  V.clear()          |  Empty V and make it size 0

Vectors make some tradeoffs with memory and speed that array containers do not; this is to allow for adding elements even if the initial declaration is static. The vector type is also built upon C-style arrays, so certain operations, especially insertion, can be relatively slow.  However, their flexibility usually more than makes up for this.

A full reference guide can be found [here](https://en.cppreference.com/w/cpp/container/vector).

## The Boost Library

One of the most popular add-on libraries for C++, especially numerical or scientific programming, is [Boost](https://www.boost.org/).  Boost is not included with any compilers, but is generally easy to obtain.  For Linux systems generally it is available through the package manager.  

### Installing Boost

Boost uses its own build system on Unix and Mac OS.  On Windows there is an experimental CMake system but generally the "bjam" builder is still used.
In all cases, installing Boost requires some familiarity with using a command line.

#### Unix and Mac OS

On a Linux system, the simplest way to install Boost is to utilize the package manager of the distribution.  For example on Ubunutu the command is
```no-highlight
sudo apt install libboost-all-dev
```
or on Fedora (or Centos 8 and beyond)
```no-highlight
sudo dnf install boost
sudo dnf install boost-devel
```
If you do not have superuser (sudo) permission or you wish to install Boost somewhere other than the main system libraries, follow the general [instructions](
https://www.boost.org/doc/libs/1_76_0/more/getting_started/unix-variants.html).
The default prefix for installation from `b2 install` is `/usr/local`.  This is true for both Linux and Mac OS.  If you wish to install to `/usr/local`, which is normally in the system search paths, you will need to run the installation command with sudo
```no-highlight
./b2 install
```
If you do not have sudo privileges or you wish to install it someplace else, keep in mind that this will affect how you reference the headers and libraries with `-I` and `-L`.

#### Windows

Installation on Windows is somewhat more complicated than on Unix variants.  First download the zipped source file.
1. Start a command window. If you wish to install Boost in a standard search path, you will need to run it as administrator.
2. Unzip the .zip file into a folder of your choice `C:\yourpath`.  It will unpack to a folder with a version number, which may vary from our example `C:\yourpath\boost_1_76_0`
3. Create some folders, e.g.
      mkdir C:\boost-build
      mkdir C:\yourpath\boost_1_76_0\boost-build
      mkdir C:\boost
4. From the command prompt, `cd C:\yourpath\boost_1_76_0\tools\build
5. Run `boostrap.bat gcc`
6. Install the build system with `b2 --prefix="C:\boost-build" install`
7. Modify your session PATH with `set PATH=%PATH%;C:\boost-build\bin`
8. Return to your source directory `cd C:\yourpath\boost_1_76_0`
9. Build with `b2 --build-dir="C:\install\boost --prefix="C:\boost" --build-type=complete toolset=gcc install`
10. This will install to "C:\boost\include\boost-1_76_0" and "C:\boost\lib"
      You may remove temporary unpacking and build directories if you wish.  You may also move the header files up to C:\boost\include if you prefer.  Remember that `<boost/boostheader.hpp>` will use `-I` to start looking for that subdirectory.

Using Boost is probably simplest with a Makefile.  The [example](/courses/cpp_introduction/codes/makefile.windows_boost) is for Windows with a particular choice of location for the boost header files and libraries.  Change the locations for `-I` and `-L` as appropriate if they are located elsewhere on your system.  Please refer to the earlier [chapter](/courses/cpp_introduction/make) for a review of setting up Makefiles.  This example goes with a standard Boost [example](/courses/cpp_introduction/codes/boost_example.cxx).

If you have installed Boost onto a Linux or Mac OS system to a system default search location such as `/usr` or `/usr/local` you will not need to specify the `-I` or `-L` paths at all.  The example [makefile](/courses/cpp_introduction/codes/makefile.linux_mac_boost) assumes installation in system paths in the compiler's default search paths.

**Exercise**

Consult the vector [documentation](https://en.cppreference.com/w/cpp/container/vector) and write a program to do the following:
1. Define a vector of type int with N elements.  Initialize N to 10.
2. Initialize each element to 2k+1 for k=0 to N-1
3. Append -20 to the end of the vector
4. Insert the value 15 after position 3
5. Add 100 to element 0. Replace element 1 with 102.  Use two different methods to access the element.
6. Use an iterator with begin and end to print the contents.
7. Use a range for loop to print the elements.
Hints: 
For a templated container like a vector, we can't assume an integer for iterating so we use a type `std::vector<type>::iterator` to declare it. You will need it for `begin` and `end` in a loop.  It can also be used to `insert`.  
Insert is a bit tricky.  It has several forms but the simplest is `<vec>.insert(location, const<type>& value)`, e.g. `myvec.insert(myvec.begin()+2,42)`.  
You may use an integer for appropriate loop variables, but remember that `size_type` is more correct.  You may also use `using namespace std` if you wish.

{{< spoiler text="Example Solution" >}}
{{< code file="/courses/cpp_introduction/solns/vector.cxx" lang="c++" >}}
{{< /spoiler >}}

The sample uses `endl` (output an end-of-line character) rather than `\n`.  There is a slight difference between the two which we will discuss [later](/courses/content/cpp_introduction/console_io).


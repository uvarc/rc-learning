---
title: "Pointers and Memory Management"
toc: true
type: book
weight: 53

---

We have learned that variables are "nicknames" for the contents of a specific location in memory.  In many languages, including C++, we can also define variable that contain the actual address of a memory location.  These variables are called _pointers_ because they "point to" memory directly.

Since an ordinary variable is also associated with memory, we can use the **reference opearator** `&` to obtain the location in memory.  Inversely, given a location, we can use the **dereference operator** `*` to get the value stored there.

{{< code file="/courses/cpp_introduction/codes/ref_deref.cxx" lang="c++" >}}

## Pointer Variables

Pointer variables are declared using the dereference operator.  
```c++
int* p;
```
The type of `p` is _pointer to int_.  

Spaces are ignored by the compiler, so
```c++
int* p;
int *p;
```
and even
```c++
int * p;
```
are equivalent.  However, be aware that
```c++
int* p, i, j;
```
does _not_ declare pointers to int `p`,`i`, and `j` in C++.  Only `p` is a pointer in this declaration.  For this reason it is recommended to keep declarations of pointers separate.

The choice between `int* p` and `int *p` is largely a matter of taste and emphasis.  Some programmers believe that `int* p` is more appropriate for C++ whereas `int *p` is more "C like."  However, there is no rule.  As for curly braces, programmers should be consistent in notation.

Example:
{{< code-download file="/courses/cpp_introduction/codes/pointers.cxx" lang="c++" >}}

### The NULL Pointer

If the memory location referenced by a pointer is invalid, the pointer can be set to a special value, NULL.  NULL will never compare as equality to any valid pointer value. 

It is frequently used to initialize a pointer variable.
```c++
float *p=NULL;
```
Setting a pointer variable to NULL can prevent referencing an unintended memory location.

## Memory Allocation and the New Operator

### C-Style Allocation
Previously we saw that arrays could be initialized with a variable that was set at compile time and declared `const`.   However, this limits the flexibility of the code.  Anytime a change is required, the code must be recompiled.  Modifying hard-coded values is also error prone.  Finally, in many cases we need to be able to adapt the code to the circumstances of a particular run.  This is especially true for programs that use arrays, since they are frequently used to represent grids or other numerical entities, and the program should be capable of running at different resolutions without the need to recompile each time.

C has a set of `alloc` routines of which `malloc` (memory allocation) is most frquently used.  The malloc function allocates a block of memory of the specified number of _bytes_.  It must be told the number of bytes to obtain by the programmer.  Nearly always the `sizeof` function must be invoked to be sure the correct number of bytes is allocated.
Malloc returns a pointer of type `void` so it must be cast to the desired type.

These blocks are raw chunks of memory, but can be referenced as the C-style arrays we have already seen.  
```c++
    float *y = (float *)std::malloc(N*sizeof(float));
    for (int i=0; i<N; ++i) {
        std::cout<<y[i];
    }
    std::cout<<"\n";
```

Malloc and the related routines `calloc` (contiguous allocation) and `realloc` (reallocate) are supported by C++.

{{< code file="/courses/cpp_introduction/codes/malloc.cxx" lang="c++" >}}

The compiler is not required to initialize the memory returned by malloc, but at least some do, initializing it to zero.

If the block cannot be acquired, the return value of the pointer is NULL.

### New

The older C-style allocation functions are prone to errors and are generally useful only for built-in types.  C++ defines the `new` operator to allocate memory for any type for which a _constructor_ exists.  The constructor is a function that tells the compiler how to set up a variable of a particular type.  Programmers write their own for [classes](/courses/cpp_introduction/classes) but the compiler provides constructors for built-in types.

The syntax of `new` is
```c++
<type>* var=new <type>[N];
```
where the angle brackets indicate an option and are not typed into the code.

{{< code file="/courses/cpp_introduction/codes/new.cxx" lang="c++" >}}

This creates an array of `N` floating-point numbers.

There are several differences between `malloc` and `new`, the most important of which is that `new` uses the constructor, plus the item count, to compute the number of bytes of memory to allocate.  Generally, `new` is the "C++ way" even for built-in types and its use is recommended over `malloc`.

### Multidimensional C-Style Arrays with New

We will only illustrate 2d arrays here; the concept can be extended to higher-dimensional arrays.
Two-dimensional arrays are 1-d arrays of pointers to a one-dimensional array.

{{< code-download file="/courses/cpp_introduction/codes/twod.cxx" lang="c++" >}}

A two-dimensional array can be declared with two asterisks, indicating that the variable is a "pointer to pointer."

## Memory Deallocation

If memory is allocated, it must be released (deallocated) or the result may be a **memory leak**.  If new (or malloc) is called, a pointer is returned to a block of memory.  Another call to new can be made returning to the _same variable_; C++ will not stop this.  When the old pointer value is overwritten, access to its corresponding block of memory is lost, but the memory is still allocated and is not available for any other use.  

{{< code-download file="/courses/cpp_introduction/codes/leaker.cxx" lang="c++" >}}

A memory leak can rapidly fill up memory.  In modern computer operating systems, running executables are restricted to a particular region of memory and an attempt to access memory outside that area may result in a **segmentation violation**.  If it runs long enough, a leaking executable will eventually encounter a segmentation violation or sometimes, at least on Linux, a **bus error** (a bus error occurs when an attempt is made to access a location in memory that does not exist).  Occasionally, however, leaking codes can still take down an entire system.   Therefore memory leaks must be avoided.

To release memory allocated with `malloc` or similar, use `free`.  As a general rule, each `malloc` must be paired with one `free`.
{{< code file="/courses/cpp_introduction/codes/better_malloc.cxx" lang="c++" >}}

For `new` use `delete`
{{< code file="/courses/cpp_introduction/codes/better_new.cxx" lang="c++" >}}
As for `malloc` and `new`, `free` is a function while `delete` is an operator.

If an array is created with `new` the correct form of delete places square brackets before the name of the pointer:
```c++
   float *x =new float[N];
   delete [] x;
```
Otherwise use delete without square brackets.
```c++
   float *x = new float;
   delete x;
```

**Exercise**
Correct the memory-leaking example.  Leave the multiple `new` statements (even though they have no consequences) and fix the code as is.  Is there a way to tell whether the fix was successful?

{{< spoiler text="Example Solution" >}}
{{< code file="/courses/cpp_introduction/solns/fixed_leaker.cxx" lang="c++" >}}
{{< /spoiler >}}


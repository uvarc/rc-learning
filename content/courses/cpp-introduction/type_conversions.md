---
date : "2021-06-23T00:00:00-05:00"
title: "Type Conversions"
toc: true
type: book
weight: 25

---

## Type Conversions

As we have seen with the example of dividing two integer, operators are defined on specific types and return a specific type.  What if we write `2./3`?  The first operand is a double, whereas the second is an integer.  This is called a _mixed expression_.  For consistency, one type must be converted to match the other before the operator is applied.  

### Implicit Conversions

Most compilers will automatically cast numeric variables to make mixed expressions consistent.  The 
hierarchy, from lowest to highest, is bool -> char -> short int -> int -> unsigned int -> long -> unsigned -> long long -> float -> double -> long double.  
Each variable will be _promoted_ until all in the mixed expression are the same type.

Explicit conversion also occurs when a variable or literal of one type is assigned to a
variable of another type.  If the conversion is legal, the compiler will force
the type of the quantity on the right-hand side of the assignment to match the declared type of the variable on the left-hand side.  

The rules for numerical type conversions may result in some surprises.  For example, when a float is converted to a double, the extra bits in the significand are filled ("padded") with zeros.  There is no magic that tells the compiler how to extend it "correctly."  
This can result in loss of precision or even seemingly-bizarre results, such as when a signed int is converted to an unsigned int.

To illustrate:
{{< code file="/courses/cpp-introduction/codes/conversions.cxx" lang="c++" >}}

The result on a Unix system with g++ is
```no-highlight
Signed to unsigned:
-1100020
4293867276
Float to int:
4.78000020980835
4
Double to float:
3.141592653589793
3.141592741012573
```
Notice that the compiler prints out 16 digits of the `float` variable when told to do so, even though they are incorrect past the 7th decimal place.  Moreover,
conversion to int from any floating-point type truncates, it does not round.

### Explicit Conversion

Explicit type conversion is also called _casting_.
Use explicit casting to be clear, or in circumstances such as argument lists where the compiler will not do it.

Explicit casting among numeric types:
```c++
r=(float) i;
j=(int) w;
d=(double) f;
```

The same phenomena apply as for implicit conversions when one numeric type is converted to another. 

{{< code file="/courses/cpp-introduction/codes/casts.cxx" lang="c++" >}}

The result on a Unix system with g++ is
```no-highlights
Cast 0.3333333432674408
Literals 0.3333333333333333
```
Recall that literal floating-point values are typed as `double` by C++.

The above format was inherited from C.  C++ also supports a functional notation:
```
r=float(i);
j=int(w);
d=double(f);
```

All these conversions are called **C style casts**.  They are very permissive.
This is not a problem for ordinary numeric variables as long as the programmer understands the rules.  It can become a problem when converting from one _pointer_ type to another.  
We will discuss this in more detail when we talk about [pointers](/courses/cpp-introduction/pointers_mem).

Newer **C++ style casts** look like [templates](/courses/cpp-introduction/polymorphism).

The _static_ cast is most widely used and behaves similarly to the C-style casts.
```c++
int n=100000;
double s = static_cast<double>(n);
```

The _dynamic_cast_ is mostly used for pointers or reference to classes and is beyond our scope here.  The _constant_cast_ can be used to both cast and alter the value of a variable declared `const` and likewise is beyond our scope in this introduction. Finally, a _reinterpret_cast_ is defined; it is used to convert pointers from one type to another and also will not be discussed in this introduction.


### String/Numeric Interconversions

C++ has introduced _stringstreams_ for this purpose.
Stringstreams are internal string _buffers_.

In this example we have included the line 
```c++
using namespace std;
```
for convenience.  We will discuss namespaces in when we talk about 
[scope](/course/cpp-introduction/scope).  In short, it makes the _standard_ namespace the default, so that we may omit in before keywords such as `cout` and `string`.

```c++
#include <iostream>
#include <string>
#include <sstream>

using namespace std;

int main() {
    string age;
    int iage;
    iage=39

    //Convert numeric to character:
    stringstream ss;
    ss<<iage;       //load iage into buffer
    age=ss.str();
   //Convert character to numeric:
    age='51'
    stringstream ss2(age);
    ss2>>iage;
}
```
This may make more sense once we understand file [input/output](/courses/cpp-introduction/file_io).

#### C++11 String Conversions

The C++11 standard has introduced several type-conversion functions:

{{< table >}}
| Function    |  Conversion | 
|-------------|-------------|
|`std::stoi`  |  string to integer |
|`std::stol`  |  string to long    |
|`std::stoul`    |  string to unsigned integer | 
|`std::stoll`    |  string to long long | 
|`std::stof`     |  string to float |
| `std::stod`    |  string to double |
| `std::stold`   |  string to long double|
| `std::to_string`    |  number to string |
| `std::to_wstring`   |  number to wide string |
{{< /table >}}

These are in the standard string class for C++11 and above.  Some compilers may require the addition of a `-std=c++11` flag to access them.

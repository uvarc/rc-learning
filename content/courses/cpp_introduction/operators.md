## Type Conversions

Most compilers will automatically cast numeric variables to make mixed expressions consistent.  The variables are promoted according to their rank.  Lowest to highest the types are integer, float, double.

Use explicit casting to be clear, or in circumstances such as argument lists where the compiler will not do it.

Strings may be cast to numbers and vice versa by a stringstream (this is the "correct" C++ way).  Details will be discussed [later](/courses/cpp_introduction/characters_strings).

Explicit casting among numeric types, default kind.
```c++
r=(float) i;
i=(int) r;
d=(double) r;
```


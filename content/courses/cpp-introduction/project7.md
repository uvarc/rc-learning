---
title: Project 7
toc: true
type: book
draft: false
weight: 150
---

1. Write a Fraction class that implements a representation of a fraction, where each instance consists of a numerator and a denominator. Overload addition, subtraction, and multiplication for this class.  Write a method to format each fraction in the form
```no-highlight
5/7
```
Optionally overload assignment (=).

For your first attempt it is not necessary to reduce the fraction, i.e. it is acceptable to have fractions like 6/8. Be sure to check for division by zero in your division procedure.

2. Set fractions with a denominator of 0 to 0/0 (this is arbitrary).

3. Write a driver to test all your procedures.

4. Add a reduce method that finds the [least common multiple](https://en.wikipedia.org/wiki/Least_common_multiple) to obtain the lowest common denominatorand reduce the fraction.

Extra: Look up how to overload `<<` so that you can write
```c++
cout<<f<<"\n";
```

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/fractions.cxx" lang="c++" >}}
{{< /spoiler >}}


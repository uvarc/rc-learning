---
date : "2021-06-23T00:00:00-05:00"
title: "Loops"
toc: true
type: book
weight: 43

---

Much computing is repetitive work.  Evaluate an expression many times with different values.  Read lines of a file.  Update a large number of variables.  To accomplish this, we use _loops_.

Loops may invoke other loops as part of their bodies.  These are generally called _nested_ loops.  Nested loops can result in many millions of executions of statements, so some care may be required in their construction to avoid unnecessary repetitions.

## For Loops

The `for` loop executes a fixed number of iterations unless explicitly terminated.
It is most generally expressed as
```c++
for ( initializer; stop condition; increment ) {
    code;
}
```
Starting from the initializer statement, the "stop condition" is evaluated.  If it is false the flow continues to the body of the loop.  If it is true, the loop is exited without executing any of the statements in the body.

One of the most common forms employs integer loop variables.
```c++
for (int i=l;i<=u;i+=s) {
  code;
}
```
* `i`: Loop variable
* `l`: Lower bound
* `u`: Upper bound
* `s`: Stride, or increment.  Usually we use `i++` for a stride of 1.
     * `s` can be negative, in which case `l` must be greater than `u`. For an increment of -1 use `i--`.

Do not change the loop variable within a loop.  That is, it should not appear on the left-hand side of an assignment.

There is a subtle difference between
```c++
int i;
for (i=0;i<10;i++) {
    std::cout<<i<<"\n";
}
```
versus
```c++
for (int i=0;i<10;i++) {
    std::cout<<i<<"\n";
}
```
In the first case, `i` is in _scope_ within and beyond the loop.  In the second case, `i` is in scope only within the loop.  Try the example:

{{< code-download file="/courses/cpp-introduction/codes/loop_scope.cxx" lang="c++" >}}

Uncomment the line to print `j` after the second loop and try it again.  What happened?  To the compiler, the variable `j` literally does not exist outside the loop.

We will discuss scope in more detail [later](/courses/cpp-introduction/scope).

### Range-Based For Loops

The C++11 standard has introduced a new version of the `for` loop that may be familiar to programmers of Python and similar languages.  This loop steps through an _iterator_.  An iterator is a sequence that can be traversed in a unique order.  The only iterator we have encountered so far is the string.  For example, we can loop over a string and extract each character:

{{< code file="/courses/cpp-introduction/codes/iter_for.cxx" lang="c++" >}}

## While Loops

Whereas `for` loops execute a particular number of iterations, `while` loops iterate based on the truth or falsity of an expression.  The `while` continues as long as the expression is `true` and terminates when it becomes `false`. It is up to the programmer to be sure to add statements to ensure that the expression eventually evaluates to `false` so the loop will end.
```c++
while (boolean expression) {
   statement;
   statement;
   statement somewhere to check expression;
}
```

Example:
{{< code-download file="/courses/cpp-introduction/codes/while_demo.cxx" lang="c++" >}}

## Do While

The standard `for` and `while` loops test at the _top_.  That is, upon entry to the loop, the termination condition is evaluated.  If it is false, the statements of the loop are executed.  Otherwise the loop is exited.
With the ability to break out of the loop at will, we can change this pattern.
C++ provides a `do while` construct for this.

```c++
do {
   statement;
   statement;
   while (boolean expression);
}
```
A standard `while` loop may not be entered if the condition is initially false, whereas a do-while will always be executed at least once.

{{< code-download file="/courses/cpp-introduction/codes/do_while.cxx" lang="c++" >}}

## Exiting Early and Skipping Statements

The `break` statement leaves the loop immediately.
A `break` is able to break out of _only_ the loop level _in which it appears_.  It cannot break from an inner loop all the way out of a nested set of loops.  This is a case where `goto` is better than the alternatives.

The `continue` statement skips the rest of loop and goes to the next iteration. Like `break`, it applies only to the loop level in which it is located.

```c++
x=1.;
while (x>0.0) {
    x+=1.;
    if (x>=10000.0) break;
    if (x<100.0) continue;
    x+=20.0;
}
```

**Exercises**

1. Loop from 0 to 20 by increments of 2.  Make sure that 20 is included.  Print the loop variable at each iteration.
2. Start a variable n at 1.  As long as n is less than 121, do the following:
    * If n is even, add 3
    * If n is odd, add 5
    * Print n for each iteration.  Why do you get the last value?
3. Set a real value x=0. Loop from 1 to N inclusive by 1.
  * If the loop variable is less than M, add 11.0 to x.
  * If x > w and x < z, skip the iteration.
  * If x > 100., exit the loop.
  * Print the final value of x.
  * Experiment with different values for the variables.  Start with N=50, M=25, w=9., z=13.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/loops.cxx" lang="c++" >}}
{{< /spoiler >}}


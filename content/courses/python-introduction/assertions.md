---
title: The Assert Statement
draft: false
toc: true
type: docs
weight: 265
menu:
    python-introduction:
       parent: Testing and Debugging
---

We finally obtained a correct version of the `dow.py` day-of-the-week calculator, but we arrived by a rather cumbersome process of manually adding code.  We determined several test cases that revealed bugs in our code, but we added each one individually.  Why not automate this testing?

We can write what is sometimes called a _test harness_ for the `DoW` function.  We will attempt to cover easier cases and the "corner cases" mentioned earlier.  It is particularly important to test leap years and non-leap years, since that is a major potential source of errors.

We can recast our tests for dow.py in a somewhat ad hoc manner.

{{< spoiler text="Setting up tests for our DoW function." >}}
{{< code-download file="/courses/python-introduction/scripts/dow_tests.py" lang="python" >}}
{{< /spoiler >}}

We can easily add individual days to the list of scattered test cases, and could repeat the while block to test additional individual years.

## Assert

The `assert` keyword, as its name implies, tests whether a conditional is True.  If it is, execution continues.  If it returns False, the statement throws an `AssertionError` and execution ceases.  

The syntax is `assert` \<conditional>, \<optional message>.  The message is printed if the assertion fails.  

```python
doW=DoW(30,5,2016)
assert doW=="Monday", f"Should be Monday, got {doW}"
```

NumPy adds some additional array assertion [functions](https://numpy.org/doc/stable/reference/routines.testing.html), several of which are particularly useful for floating-point numbers, which cannot be reliably compared for strict equality.

The assert statement is intended for debugging only; use standard `try` and `except`, or other validation methods such as `if` statements, for checking production cases such as file errors, type errors, erroneous input to functions, and so forth.  This is just good practice in general, but it is also because the Python intepreter can be told to suppress assert statements.  One way to do so is to run from the command line with the `-O` (optimize) option.

**Example**
Run the following code from the command line (or use !python in an iPython console, or open a terminal in JupyterLab), first with `python assert_it.py` and then with `python -O assert_it.py`.  What should you do instead of using `assert` in this code?

{{< code-download file="/courses/python-introduction/scripts/assert_it.py" lang="python" >}}

**Exercise**
Starting from the dow_buggy.py script, add assertions to test for bugs.

{{< spoiler text="Setting up tests for our DoW function." >}}
{< code-download file="/courses/python-introduction/scripts/dow_assert.py" lang="python" >}}
{{< /spoiler >}}

Work through the bugs as you did before.  Hint: you may need to play with working through next, step in, continue, going in and out of debugging mode, etc.  Pay attention to the value of test_id.


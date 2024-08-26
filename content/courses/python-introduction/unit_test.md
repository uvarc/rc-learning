---
title: Unit Testing
draft: false
toc: true
type: docs
weight: 270
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
       parent: Testing and Debugging
---

A function, module, or class is a _unit_ of code.  Testing each unit is thus called _unit testing_.  It is not always possible to completely isolate each function and test it, but every function should be tested in some way as it is written.  Do not wait until you have written dozens or hundreds of lines of code to start testing.

## Unit Testing Frameworks

We have constructed our own attempt at testing our unit, the function `DoW`.  More formalized unit-testing frameworks exist for Python scripts.  The built-in standard package is [PyUnit](https://docs.python.org/3/library/unittest.html) (formerly called, and still imported as, unittest).  [Nose2](https://docs.nose2.io/en/latest/) can be installed to add extensions and plugins to PyUnit.  PyUnit may be somewhat cumbersome for a beginner, since it involves subclassing and writing methods.  A fairly simple, and very popular, framework is _PyTest_.  It should be installed by default with Spyder or the VSCode Python extension, or it can be explicitly installed if needed.

Other, more sophisticated or special-purpose, frameworks exist, but we will focus on PyTest for our example.

### PyTest

PyTest is easy to install and simple to use.  To implement our tests, we write functions that begin with `test_` and contain at least an `assert` statement.

**Example**
This is a very simple example from the PyTest [documentation](https://docs.pytest.org/en/7.1.x/getting-started.html)

{< code-download file="/courses/python-introduction/scripts/test_example.py" lang="python" >}}

#### PyTest in Spyder or other IDEs, or Command-Line

This is easy to run in Spyder.  In the iPython console window, type
```python
!pytest test_example.py
```
Depending on your working directory in Spyder, you may need to provide a path, such as
```python
!pytest /home/myid/Python/scripts/test_example.py
```
This test is intentionally written to fail.  You should see a message.

<<<<<<< HEAD
The pytest executable can also be run from a command line, such as a Linux or Mac OS terminal, or the Miniforge prompt.
=======
The pytest executable can also be run from a command line, such as a Linux or macOS terminal, or the Anaconda prompt.
>>>>>>> 6a198f065e2a33f7e1411ff16f3c3c7858f3694c
```no-highlight
pytest /Users/myid/Python/scripts/test_example.py
```
Pytest allows, but does not require, the programmer to set up  multiple testing functions in a class. Another example from their documentation:
 
{< code-download file="/courses/python-introduction/scripts/test_class.py" lang="python" >}}

The name of the class must begin with `Test`.  The tests are run by specifying the module name at the command line.

```python
pytest test_class.py
```

We may need to run the same test with multiple values for the input.  Pytest provides the [parameterize](https://docs.pytest.org/en/latest/how-to/parametrize.html#parametrize-basics) capability.  We invoke it with a decorator:
```python
@pytest.mark.parametrize("test_input,expected",[("3+5", 8),("2+4", 6),("6*9", 42)])
def test_eval(test_input, expected):
    assert eval(test_input) == expected
```

Pytest has a number of other capabilities, including [fixtures](https://docs.pytest.org/en/7.1.x/how-to/fixtures.html#how-to-fixtures), which are functions that can be used to generate data for input into test functions.  This is beyond our scope, however.

#### PyTest with Jupyter Notebooks

There are a few options for using PyTest with Jupyter notebooks.  A popular package is [testbook](https://testbook.readthedocs.io/en/latest/).  It can be installed from conda-forge in Miniforge, or with pip. Testbook is not run within a notebook, but loads the notebook into a Python script and runs the tests.

**Example**

Our notebook `testnb.ipynb` contains the following two cells
```python
def func(x):
    return x + 1

def func2(x,y):
    return x + y
```
We have two functions we wish to test.  Testbook will require us to use the PyTest _fixtures_ feature mentioned above, so that we can access both functions through the `tb` object set up by PyTest. 

{< code file="/courses/python-introduction/scripts/testnb.py" lang="python" >}}

We would run this as for other PyTest files with
```python
pytest testnb.py
```
at a command line, or
```python
!pytest testnb.py
```
within an iPython interpreter.

**Exercise**

We went to all the trouble to write the day-of-the-week function, but the `datetime` module has it built in.
```python
import datetime
days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

day_number = datetime.date(year=2016, month=5, day=30).weekday()
print(days[day_number])
```

Use this to write a unit test for the DoW function. You will need to parameterize the test. See the [documentation](https://docs.pytest.org/en/6.2.x/parametrize.html) for a more detailed explanation of parameterization.

{{< spoiler text="Example solution" >}}
{< code-download file="/courses/python-introduction/exercises/test_dow.py" lang="python" >}}
{{< /spoiler >}}

## Test-Driven Development

In _test-driven development_, the programmer develops unit tests for each function first, before writing the code.  This forces the developer to think in terms of small, easily-tested units, Code is then written to pass each test in turn.  Once all tests pass and the unit is complete, it can be combined with others into larger units and a complete program.

Even if full unit testing is not practical, especially in the case of non-professional programmers, the fundamental principle of testing code continuously is easy to apply.  Tests should also be automated in some manner, even if a more formal framework is not used.  Do not wait to start testing until you have written hundreds of lines of code.  

In addition to their other benefits, unit tests can help to detect and correct _regressions_.  A regression occurs when a change to the code introduces new bugs.  Regression testing should also be part of the development process.  When changes are made, the entire code should be retested, not just the new units.

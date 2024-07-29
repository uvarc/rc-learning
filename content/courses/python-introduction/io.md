---
date: "2020-11-17"
title: "Input and Output"
weight: 81
---

## Console Input

The console is a text interface for input to and output from the computer.  In Spyder, the iPython window itself can serve as a console.  In JupyterLab the output console is indicated by lines marked with `Out []` whereas a reference to an input console will open a textbox. 

To read input from the console we use the `input()` function (Python 3+).  In Python 2.7 the equivalent is `raw_input()`.  Any string within the parentheses is optional and, if present, it will be printed to prompt the user.  The input from the user is captured as a string and returned; it must be stored for any subsequent use.

The input function returns _only_ a string.  If you need to use the values as any other type, you must perform the conversion yourself.

Example:

```python
weight=input("Enter your weight in pounds:")
print(type(weight))
weight=float(input("Enter your weight in pounds:"))
print(type(weight))
```

## Console Output

We have already been using the `print` function.  Let us now examine it in more detail.

The print function

* always inserts a space between its arguments
* always adds a newline character unless the `end` argument is added.
  * `print(var,end="")`

Messages can be added in between variables.

```python
h=1.45;
w=62.
print("Your BMI is",w/ht**2)
```

**Exercise**

Use Spyder or another IDE to write a <em>complete</em> program to compute BMI from weight and height input from a user.  First request the user's choice of units.  We have not spent much time with strings yet so you may use a digit to indicate the user's choice, but remember it will still be a string on input. Then request weight and height.  You will need to convert these from strings. Look up the correct conversion factors for Imperial to metric units. Compute the BMI. 

The categories are 
{{< table >}}
|   BMI     |   Category |
|-----------|------------|
| less than 18.5    |  Underweight |
| 18.5 to 25.0      |  Normal      |
| 25.0 to 30.0      |  Overweight  |
| 30.0 to 35.0      |  Obese Class I |
| 35.0 to 40.0      |  Obese Class II |
| more than 40.0      |  Obese Class III | 
{{< /table >}}

Print the user's BMI value and category.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/user_input_bmi.py" lang="python" >}}
{{< /spoiler >}}


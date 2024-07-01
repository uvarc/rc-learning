---
title : "MATLAB Techniques"
date : "2019-06-23T08:37:46-05:00"
type : docs
toc : true
weight: 121

menu:
    matlab-programming:
        parent: Effective MATLAB Programming
---

# Matlab Programming Review
The section is a review of the basic programming constructs presented in the
**Matlab Fundamentals** tutorial.

## Logical operations

<br/>
**[Documentation: if,elseif,else](https://www.mathworks.com/help/matlab/ref/if.html)**

**[Exercise: Logical Operations](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=8&lesson=2&section=2)**

## Decision Branching

{{< figure src="/courses/matlab-programming/branching1.png"  >}}
<br/>
<br/>
**[Exercise: Using if-elseif-else](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=3&section=6)**

**[The switch-case Construction](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=3&section=9)**

**[Exercise: Using switch-case](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=3&section=10)**

## For Loops

**[Video: For Loops](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=5&section=2)**

<br/>
{{< figure src="/courses/matlab-programming/forLoop1.png"  >}}
<br/>
<br/>
**[Exercise: Looping Through a Vector](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=5&section=6)**

## While Loops
To use a for loop, you need to know in advance how many iterations are required. If you want to execute a block of code repeatedly until a result is achieved, you can use a while-loop.
<br/>
<br/>
**[Slide: The while-Loop Construction](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=6&section=3)**
<br/>
<br/>
{{< figure src="/courses/matlab-programming/whileLoop1.png"  >}}
{{< figure src="/courses/matlab-programming/whileLoop2.png"  >}}
<br/>
<br/>
**[Exercise: Finding eps](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=14&lesson=6&section=7)**

## Creating and Calling Functions
<br/>
**[Slide: Creating and Calling Functions](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=15&lesson=2&section=1)**
<br/>
<br/>
{{< figure src="/courses/matlab-programming/function1.png"  >}}
<br/>
<br/>
**[Exercise: Create and Call a Function](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=15&lesson=2&section=4)**

**[Slide: Creating Functions Files](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=15&lesson=3&section=2)**
<br/>
<br/>
**Calling Function Files**

{{< figure src="/courses/matlab-programming/function2.png"  >}}
<br/>
<br/>
**[Exercise: Compare Using a Tolerance](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlbe#chapter=15&lesson=3&section=5)**
<br/>
<br/>
{{< figure src="/courses/matlab-programming/function3.png"  >}}
<br/>
<br/>
# Storing Heterogenous Data

## Matlab Data Types
<br/>
**[Slide: MATLAB Data Types](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=1&section=1)**

**[Exercise: Creating Variables of a Specific Data Type](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=1&section=3)**

## Table Basics

<br/>
**[Documentation: Tables](https://www.mathworks.com/help/matlab/ref/table.html)**

**[Exercise: Create a Table from Workspace Variables](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=2&section=1)**

## Extracting Data from a Table
<br/>
**[Exercise: Extracting Data from a Table: Dot Notation](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=5&section=1)**

**[Exercise: Extracting Data from a Table: Curly Braces](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=5&section=2)**

## Cell Array Basics

<br/>
**[Documentation: Cell Arrays](https://www.mathworks.com/help/matlab/ref/cell.html)**

**[Exercise: Creating and Concatenating Cell Arrays](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=6&section=1)**

**[Exercise: Cell Array Extraction](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=7&section=1)**


## Structures

<br/>
**[Documentation: Structures](https://www.mathworks.com/help/matlab/ref/struct.html)**

**[Exercise: Create a Structure and Access Fields](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=8&section=1)**

**[Exercise: Create a Structure Array](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=2&lesson=9&section=1)**



# Structuring Heterogenous Data


## Structuring Data Considerations
<br/>
**[Slide: Structuring Data Considerations](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=1&section=1)**

**[Exercise: Cell Array](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=1&section=3)**

**[Exercise: Structure Array](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=1&section=6)**

**[Exercise Table](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=1&section=8)**

## Extracting Multiple Elements from Cell and Structure arrays
<br/>
**[Slide: Gathering Output](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=2&section=2)**

**[Exercise: Multiple Elements from a Cell Array](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=2&section=4)**

**[Exercise: Multiple Elements from a Structure Array](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=2&section=3)**
<br/>

## Function Handles
<br/>
**[Using Function Handles](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=3&section=1)**

**[Exercise: Optimization](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=3&section=4)**
<br/>
## Applying Scalar Functions to Arrays
<br/>
{{< figure src="/courses/matlab-programming/cellfun1.png"  >}}
<br/>
<br/>
<br/>
**[Video: Applying Scalar Functions to Arrays](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=4&section=2)**

**[Exercise: Applying Functions to Groups](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=4&section=3)**

**[Exercise: cellfun Basics](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=4&section=8)**


## Converting Data Types
<br/>
**[Slide: Converting Data Types](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=5&section=1)**

**[Exercise: Numeric to Cell Array](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=5&section=2)**

**[Exercise: Converting Strings](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=3&lesson=5&section=5)**

# Managing Data Efficiently
<br/>
**[Slide: Datatypes and Memory](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=1&section=1)**

**[Quiz 1](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=1&section=2)**

**[Quiz 2](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=1&section=3)**

**[Video: Preallocation](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=2&section=1)**

**[Slide: Preallocating Numeric, Cell, and Structure Arrays](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=2&section=3)**

**[Exercise: Preallocation Experiment](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=2&section=5)**

## Vectorization
<br/>
{{< figure src="/courses/matlab-programming/vectorization1.png"  >}}

{{< figure src="/courses/matlab-programming/vectorization2.png"  >}}

## Copy-on-write with Function Parameters

<br/>
**[Slide: Copy-on-write Behavior](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=6&section=1)**

## In-place Optimizations

<br/>
**[Slide: In-place Optimizations](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=7&section=1)**

## Nested Functions

{{< figure src="/courses/matlab-programming/nestedFunction1.png"  >}}

<br/>
**[Slide: Nested Functions](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=8&section=2)**

**[Exercise: Create a Nested Functions](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=4&lesson=8&section=5)**

# Creating Flexible functions

<br/>
**[Video: Creating Flexible Function Interfaces](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=1&section=1)**

## Creating Multiple Interfaces with Wrapper Functions

<br/>
**[Slide: Separating the Interface from the Algorithm](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=2&section=1)**

**[Exercise: Create a Wrapper Function with a Fixed Interface](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=2&section=3)**

## Setting Default Input Values

<br/>
**[Slide: Setting Default Input Values](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=5&section=1)**

##  Missing Input Arguments

<br/>
**[Slide: Missing Input Arguments](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=5&section=2)**

**[Slide: Empty Input Arguments](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=5&section=6)**

**[Exercise: Skipping the Input](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=5&section=7)**

## Allowing Any Number of Inputs

<br/>
**[Slide: Functions with a Variable Number of Inputs](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=6&section=1)**

**[Slide: Variable Length Input Argument List](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=6&section=2)**

**[Slide: Passing Argument Lists to Another Function](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=6&section=6)**

## Allowing a Variable Number of outputs

<br/>
**[Slide: Defining Different Behaviors Based on Outputs](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=8&section=1)**

**[Exercise: Matrix or Vector Output](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=8&section=2)**

## Changing the Function Interface with Anonymous Functions

<br/>
**[Slide: Modifying Function Interfaces](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=9&section=1)**

**[Slide: Wrapping Functions with Anonymous Functions](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=9&section=2)**

**[Exercise: Write and Use an Anonymous Function](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=9&section=3)**

**[Slide: Common Function Handle Uses](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=9&section=4)**

**[Exercise: Change Function Interface with an Anonymous Function](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=5&lesson=9&section=5)**

# Creating Robust Applications

## Restricting Access Using Private Functions
{{< figure src="/courses/matlab-programming/privateFunction1.png"  >}}

<br/>
**[Slide: Making Functions Private](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=6&lesson=2&section=2)**
<br/>
<br/>
{{< figure src="/courses/matlab-programming/privateFunction2.png"  >}}


## Writing Local Functions

{{< figure src="/courses/matlab-programming/localFunction1.png"  >}}
{{< figure src="/courses/matlab-programming/localFunction2.png"  >}}

{{< figure src="/courses/matlab-programming/localFunction3.png"  >}}

## Comparison of Functions

{{< figure src="/courses/matlab-programming/compareFunction1.png"  >}}

<br/>
**[Exercise: Create Local Functions](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=6&lesson=3&section=4)**

## Validating Function Inputs

{{< figure src="/courses/matlab-programming/validateInput1.png"  >}}

{{< figure src="/courses/matlab-programming/validateInput2.png"  >}}

<br/>
**[Quiz](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=6&lesson=5&section=4)**

# Verifying Application Behavior

## Why Use a Testing Framework?
<br/>
**[Video: Why Use a Testing Framework](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=1&section=1)**

## What is a Test
{{< figure src="/courses/matlab-programming/test1.png"  >}}

<br/>
**[Slide: Elements of a Test](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=2&section=1)**

### is Functions
{{< figure src="/courses/matlab-programming/isFunction.png"  >}}

<br/>
**[Exercise: isequal Versus ==](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=2&section=3)**

### Test Response
{{< figure src="/courses/matlab-programming/assertFunction.png"  >}}

<br/>
**[Exercise: assert](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=2&section=5)**

## Writing and Running a Test Script
{{< figure src="/courses/matlab-programming/testScript1.png"  >}}

### Writing a Test Script
{{< figure src="/courses/matlab-programming/testScript2.png"  >}}

<br/>
**[Exercise: Write a Test Script](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=3&section=3)**

### Running a Test Script

<br/>
**[Slide: Running a Test Script](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=3&section=4)**

**[Exercise: Run a Test Script](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=3&section=5)**

**[Exercise: Fix Broken Tests](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=7&lesson=3&section=8)**


# Utilizing Development Tools

## Developing and Maintaining Code
{{< figure src="/courses/matlab-programming/devTools1.png"  >}}

## Folder Reports

<br/>
**[Slide: Folder Reports](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=2&section=2)**
## Errors and Debugging

<br/>
**[Video: Different Kinds of Errors](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=3&section=1)**

## Code Analyzer

{{< figure src="/courses/matlab-programming/codeAnalyzer1.png"  >}}

<br/>
**[Slide: Suppressing and Fixing Code Analyzer Warnings](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=4&section=2)**

**[Exercise: Remove Code Analyzer Warnings](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=4&section=6)**

## Debugging Runtime Errors

{{< figure src="/courses/matlab-programming/debugRun1.png"  >}}

<br/>
**[Slide: Debugging Run-Time Errors](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=5&section=3)**


## Measuring Performance

{{< figure src="/courses/matlab-programming/performance1.png"  >}}

<br/>
**[Slide: Tic and Toc](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=6&section=2)**


## Finding Bottlenecks

{{< figure src="/courses/matlab-programming/bottleneck1.png"  >}}

<br/>
**[Video: The MATLAB Profiler](https://matlabacademy.mathworks.com/R2019a/portal.html?course=mlpr#chapter=8&lesson=7&section=2)**

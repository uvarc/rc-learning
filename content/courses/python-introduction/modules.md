---
title: Modules 
toc: true
type: docs
draft: false
weight: 85

menu:
    python-introduction:
        parent: Functions and Modules
---

Modules are fundamental to the programming model of Python.  Modules are programming units that (should) consist of _related_ variables and functions that form a coherent block of _data_+_procedures_ (functions).  They are an extension of the concept of packaging code that we have already studied with functions.

In Python every file you write is a module.  The name of the module is the file name without the extension.  Every Python file must end in `.py` regardless of your operating system, so if you write a file `mycode.py` the corresponding module is `mycode`.  If you are using Jupyter you will need to export your code into a script in order to create a new module (Notebook tab -> Export to -> Executable Script).

Modules that are not run as the main program must be _imported_ for its contents to be accessible.  When a module is imported, it is compiled to a special representation called __bytecode__.  A new file of the same base name with the suffix `.pyc` will be created.  

Many modules and packages (collections of modules) are available through a base Python installation.  Anaconda provides dozens more, with others available for installation through the Environments tab of the Navigator.  We have already seen a handful of these built-in modules.  

```python
import math 
import matplotlib.pyplot
```

When we import a module we bring in its __namespace__.  A namespace is an environment that holds a group of identifiers, such as variable and function names.  In Python the namespaces take the name of the module in which they are defined.  Namespaces can be renamed when the module is imported, but __not__ afterward.

## Importing Modules

With a simple import statement we must refer to the module's components with its native namespace.

```python
import math 
import os 
import numpy 

z=math.sqrt(x)
home_dir=os.getenv("HOME")
A=numpy.zeros(200)
```

We can select only certain components with `from`

```python
from modulename import func1, func2
```

Now only func1 and func2 can be used, and we do \_not\* precede their names with the native namespace.

```python
z=func1(x,y)
w=a+b*func2(z)
```

To import __all__ symbols without a prefix use

```python
from modulename import *
```

This statement imports all names that do not begin with a single underscore (\_) and makes them accessible without being preceded by the module name.

We can also rename individual symbols

```python
from math import sqrt as squareroot
w=squareroot(10.)
```

One of the most commonly used versions of import changes the name of the namespace, typically to something simpler.

```python
import numpy as np
import pandas as pd

z=np.zeros(200)
data=pd.read_csv("my_data.txt")
```

## Main Modules

When you run a script directly through the interpreter, such as by using the Run arrow in Spyder, it is in the "main" namespace.  Your module can also be imported into the interpreter or into another module.  It will still execute everything in the module, including requests for input and the like, unless you use the special variables \_\_name\_\_ and \_\_main\_\_ (two underscores on each side).  If you use \_\_main\_\_ you can place all code you want to execute only when run directly after a conditional.  

```python
if __name__=="__main__":
    do_work
```

It is customary to include code for the main namespace into a function named `main()`.  

```python
def main():
    do_work

if __name__=="__main__:"
    main()
```

Example

{{< code-download file="/courses/python-introduction/scripts/rooter.py" lang="python" >}}

**Exercise**

Type in the example.  Save it into a file called `rooter.py`.  Type in and save a file `testmain.py`

```python
import rooter
sqrtrt=rooter.MySqrt(11.0)
print(sqrtrt)
```

First run rooter.py as a standalone script, then run testmain.py.  What's the difference?


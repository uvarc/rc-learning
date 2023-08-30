---
title: IDEs
toc: true
type: docs
draft: false
weight: 13

menu:
    python-introduction:
        parent: Introduction to Programming in Python
---

Now we will switch to Spyder.  Spyder is an Integrated Development Environment, or __IDE__, aimed at Python.  It is well suited to developing longer, more modular programs.  To start it, return to the Anaconda Navigator and click on its tile.  It may take a while to open (watch the lower left of the Navigator).  Once it starts, you will see a layout with an editor pane on the left, an explorer pane at the top right, and an iPython console on the lower right.  This arrangement can be customized but we will use the default for our examples, but with a different theme. 

The first time you open Spyder it will offer a tour of its features. We recommend you go through it.  You can change the appearance, layout, and other options from Tools->Preferences.

Type code into the editor.  The explorer window can show files, variable values, and other useful information.  The iPython console is a frontend to the Python interpreter itself.  It is comparable to a cell in JupyterLab.

![Spyder](/courses/python-introduction/imgs/Spyder.png)

Spyder automatically inserts a few lines at the top of the editor pane for each new script.  The first line, starting with the hash mark (`#`) is a _comment_ and is ignored by the interpreter. After that are some lines that start and end with triple double quotes (`"" "" ""`).  This is a special kind of comment called a documentation string or _docstring_.  We will discuss comments and docstrings [later](/courses/python-introduction/expressions_statements). Start any of your scripting lines below the docstring.

If you are using Python 2.7, add the line `from future import print_function` immediately after the triple-quoted section at the top of the editor pane.

Type

```python
print("Hello World")
```
into the editor pane. Anything entered into the editor pane is a _script_ and you must run it as a whole in order for the command to be carried out. Click the green arrow in the top ribbon to run the current script.

You can also type commands directly into the iPython console, located by default in the lower right pane. Just as with JupyterLab, if you type an expression its value will be printed.

Type into the console:
```python
In  [1]: x=5
In  [2]: y=7
In  [3]: x+y
Out [3]: 12
```

This is not the case for expressions typed into the editor pane.

```python
x=5
y=7
x+y
```
We will see nothing if we run this as a script.  You must add a `print` command to see any output as well as running the script.  Expression evaluation is only available directly from an interpreter console.

In the iPython console we can also use up-down arrow keys to scroll through our commands, and right-left arrows to edit them.

**Example**

We can see some of these features in action by creating a simple plot. After the triple double quotes in an "untitled" editor tab, type

```python
import matplotlib.pylab as plt
```
First we see a yellow triangle, indicating a syntax problem -- in this case, `plt` is imported but not used.  We ignore this warning since we will be using it.  As we type

```python
x=plt.
```

we see the editor show us our choices from the pylab _package_. We can select one or keep typing.  We type

```python
x=plt.linsp
```

to narrow it down further.  That leads us to

```python
x=plt.linspace
```

The editor then pops up a box with the arguments required by linspace.  Finally we type inside the parentheses

```python
-1.*plt.pi,plt.pi,100
```

for a final result of

```python
x=plt.linspace(-1.*plt.pi,plt.pi,100,endpoint=True)
```
(Note that we are using a different choice from the "pyplot" package in this example. As we will learn, most packages contain many functions, and the user must learn to go through the documentation.)

After this we type

```python
y=plt.sin(x)
plt.plot(x,y)
```

You must save a file before you can run it.  Go the File menu, Save As, and name it `sine.py`  Be sure to save it to the project folder you created earlier, which will require navigating there in the file-browser window.  

When we run this code, we see the plot appear in the Plots tab of the upper-right pane.  We can right-click on the image to bring up a menu that allows us to save the plot.

![SpyderSine](/courses/python-introduction/imgs/SpyderSine.png)

#### The Variable Explorer

The Variable Explorer allows us to show the values of variables in our programs.  It is particularly helpful for looking at a group of values (an array). We can examine a variable more closely by double-clicking its name to bring up a new window. In this window you can change the number of decimal places printed by clicking `Format` and typing in an expression such as `%.3f` for three decimal places.  The Variable Explorer also includes icons for saving, refreshing, or importing data to our workspace.

To clear all values in the workspace, type at the iPython console

```python
%reset
```
The same effect can be achieved through the trash-can icon in the Variable Explorer.

Now re-run your sine-plotting code and observe how the variables acquire values.  Change `sin` to `cos` and rerun your script.

## Other IDEs

Other popular IDEs for Python include the "fremium" [PyCharm](https://www.jetbrains.com/pycharm/).  The "Community" (free) edition is adequate for most casual programmer's needs.  Some organizations, such as the UVA Computer Science Department, hold licenses that students or employees can access.

[VSCode](https://code.visualstudio.com/docs/languages/python) is another IDE.  It is multilanguage but can be easily extended to support Python.

## Resources

Spyder documentation is [here](https://docs.spyder-ide.org/current/index.html).

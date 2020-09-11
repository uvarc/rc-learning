---
title: Getting Started
toc: true
type: docs
draft: false
weight: 2
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 2
---

## Setting up Your Environment

The Anaconda environment from [Anaconda Inc.](https://anaconda.com) is widely used because it bundles a Python interpreter, most of the popular packages, and development environments. It is cross platform and freely available. Download and install the Anaconda Distribution product appropriate for your operating system (Windows, Mac OSX, or Linux).  There are two somewhat incompatible versions of Python; version 2.7 is deprecated but still fairly widely used.  Version 3 is the supported version.  We will use Python 3 but if you know you need to use Python 2.7, you can download that instead.  Anaconda even makes it relatively easy to install both versions.

Once you have installed Anaconda, find the Navigator application.  You should see a workspace similar to the screenshot, with several options for working environments, some of which are not installed.  We will use Spyder and Jupyterlab, which should already be installed.  If not, click the button to install the package.

### JupyterLab

![AnacondaNavigator](/courses/python_introduction/AnacondaNavigator.png)

We will start with Jupyterlab.  Launching it will cause a tab to open in your Web browser. Select the Jupyterlab icon.  It may take a while to start.  When it opens, you will see a list of your files on the left and three icons to select the mode.  Jupyterlab incorporates a Jupyter notebook server as well as a plain Python console and a simple text editor.  We want to start a Jupyter notebook so click on the top tile.

![JupyterLabSetup](/courses/python_introduction/JupyterLabSetup.png)

A textbox will open.

![JupyterLabInput](/courses/python_introduction/JupyterLabInput.png)

Your notebook is untitled.  Open the File menu and click Rename.  Name your notebook hello.ipynb then click the Rename button.

![JupyterLabRename](/courses/python_introduction/JupyterLabRename.png)

#### Cells

The blank line with the blinking cursor is a cell.  You can type code into the cell.  After the `In[]` prompt type `print("Hello")`
To execute the cell click the arrowhead, or type the `shift+enter` keys together.

#### Your First Program

If you are using Python 2.7 please begin all your programs with the line
`from __future__ import print_function`
The symbols surrounding `future` are double underscores.

Type the following lines into a cell.

```python
Numerals=list(range(1,11))
print(Numerals[3])
print(Numerals[:3])
print(Numerals[3:])
print(Numerals[4:6])
```

Run this cell.  Is the result what you expected?

Now add lines

```python
Numerals.extend(list(range(11,21)))
Numerals[3]=11
del Numerals[4]
print(Numerals)
len(Numerals)
```

##### Some Strings

In a new cell Type

```python
greeting="Hello World"
hello=greeting[0:5]
greeting2=hello+" there"
output=greeting2+"\n"*2
```

The symbol `\n` stands for "new line."  Run this cell.  In a new cell type

```python
output
```

Run cell.  then

```python
print(output)
```

When your are working directly at the interpreter, you can type a variable and it will print the value.  This is called _expression evaluation_.  Using the `print` function observes any formatting.

#### Text Editor

JupyterLab includes a simple text editor you can use to create files.  In the upper left of your JupyterLab tab, click `+` to start the launcher. Choose the text editor. Type

```python
def hello():
    print("Hello")
    return None
hello()
```

Be sure to indent lines exactly as shown, and to return completely to the margin for `hello()`. Select your text. From the Editor menu select Language.  Scroll (far) down to Python.  You will now enable syntax coloring for this file.  From the File menu choose Save As. Name the file `hello_func.py`  By default, files will be saved into the folder in which JupyberLab is working. The default is your "User" directory.  After saving the file, return to your Jupyter notebook page and type

```python
import hello_func
```

Then run the cell.

#### Exporting

You can export embedded text in your notebook into a script.  First make sure your notebook has a name.  If you have not named your current notebook yet, call it `first_script.ipynb`.  From the Notebook menu find Export To->Executable Script.  Save the script in the usual way from your browser.  If it is in `Downloads` move it to a location of your choice.  You can make a new directory for your Python scripts if you wish.

### Spyder

Now we will switch to Spyder.  Spyder is an Integrated Development Environment, or __IDE__, aimed at Python.  It is well suited to developing longer, more modular programs.  To start it, return to the Anaconda Navigator and click on its tile.  It may take a while to open (watch the lower left of the Navigator).  Once it starts, you will see a layout with an editor pane on the left, an explorer pane at the top right, and an iPython console on the lower right.  This arrangement can be customized but we will use the default for our examples. Type code into the editor.  The explorer window can show files, variable values, and other useful information.  The iPython console is a frontend to the Python interpreter itself.  It is comparable to a cell in JupyterLab.

![Spyder](/courses/python_introduction/Spyder.png)

If you are using Python 2.7, add the `from future` line immediately after the triple-quoted section.

If you type

```python
print("Hello World")
```

into the editor pane, it is a script and you must run it in order for the command to be carried out.  Click the green arrow to run the current script.

You can also type commands directly into the iPython console.  Just as with JupyterLab, if you type an expression its value will be printed.

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

We will see nothing if we run this as a script.  You must add a `print` command to see any output as well as running the script.

In the iPython console we can also use up-down arrow keys to screen in our commands, and right-left arrows to edit them.

#### Example

We can see some of these features in action by creating a simple plot. after the green triple quotes in an "untitled" editor tab, type

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
x=plt.linspace(-1.*plt.pi,plt.pi,100)
```

After this we type

```python
y=plt.sin(x)
plt.plot(x,y)
```

You must save a file before you can run it.  Go the File menu, Save As, and name it `sine.py`  Use the project folder you created earlier.  When we run this code, we see the plot appear embedded in the iPython window.  We can right-click on the image to bring up a menu that allows us to save the plot.

![SpyderSine](/courses/python_introduction/SpyderSine.png)

#### The Variable Explorer

The Variable Explorer allows us to show the values of variables in our programs.  It is particularly helpful for looking at a group of values (an array). We can change the number of decimal places printed by clicking `Format` and typing in an expression of the `%3f` for three decimal places.  The Variable Explorer also includes icons for saving, refreshing, or importing data to our workspace.

To clear all values in the workspace, type at the iPython console

```python
%reset
```

Now re-run your since-plotting code and observe how the variables acquire values.

#### Plotting in JupyterLab

In JupyterLab, open a new notebook with the `+` icon.  From the Files sidebar, navigate to your projects folder.  Double-click on the `sine.py` file to open it in the text editor.  Cut and paste the text into one or more cells in JupyterLab.  Run the cell(s).  What happens?

You need to add a line

```python
plt.show()
```

Put this into a new cell and run it.  In the upper cell change `sin` to `cos`.  In the Notebook menu select `Run All Cells`

### Resources

Several tutorials are available for Jupyter and Jupyterlab online.  One good one is [here](https://www.tutorialspoint.com/jupyter/index.htm).

Spyder documentation is [here](https://docs.spyder-ide.org/current/index.html).

When using Anaconda, never install Jupyter/Jupyterlab or Spyder independently; you can upgrade them with the Conda package manager.

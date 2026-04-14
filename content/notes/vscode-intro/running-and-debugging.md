---
title: Running and Debugging
date: 2026-04-14T05:31:18Z
type: docs 
toc: true
weight: 30
menu: 
    vscode-intro:
---

### **Debugging**

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_26.png" alt="Screenshot of the VS Code debugger interface overview" width="600px" >}}

VSCode lets you debug without modifying your code

Add breakpoints and view variables be updated as the code runs

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_27.png" alt="Screenshot showing breakpoints placed in a Python file in VS Code" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_28.png" alt="Screenshot of the VS Code debug toolbar with step control buttons" width="600px" >}}

| **Action** | **Shortcut** | **Explanation** |
| :-- | :-- | :-- |
| Continue / Pause | `F5` | **Continue** : Resume normal program/script execution (up to the next breakpoint). **Pause** : Inspect code executing at the current line and debug line-by-line. |
| Step Over | `F10` | Execute the next method as a single command without inspecting or following its component steps. |
| Step Into | `F11` | Enter the next method to follow its execution line-by-line. |
| Step Out | `Shift+F11` | When inside a method or subroutine, return to the earlier execution context by completing remaining lines of the current method as though it were a single command. |
| Restart | `Ctrl+Shift+F5` | Terminate the current program execution and start debugging again using the current run configuration. |
| Stop | `Shift+F5` | Terminate the current program execution. |

---

#### **Debug a File**

Insert breakpoints by selecting a line to place a "red dot"

**Run > Start Debugging**

Opens a terminal automatically

Variables visible on the left side

Walk through the debug tools

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_29.png" alt="Screenshot of inserting a breakpoint by clicking the line gutter in VS Code" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_30.png" alt="Screenshot of a VS Code debug session running with the integrated terminal open" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_31.png" alt="Screenshot of the VS Code variables panel showing variable values during a debug session" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_32.png" alt="Screenshot of VS Code debug controls during an active debug session" width="600px" >}}

---

### **Open a Terminal**

**Terminal > New Terminal**

From here you can use regular terminal commands like `pwd` or even load modules!

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_33.png" alt="Screenshot of the VS Code integrated terminal panel" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_34.png" alt="Screenshot of the VS Code terminal with HPC module load commands" width="600px" >}}

---

### **Create a Conda Environment**

In a new terminal:

```bash
conda create -n my_env python=3.11 numpy pandas
conda activate my_env
pip install matplotlib
```

---

### **Set Your Interpreter**

Select your environment from your list!

When you run your code it will run in your environment.

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_35.png" alt="Screenshot of the Python interpreter selection prompt in VS Code" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_36.png" alt="Screenshot of the list of available Python interpreters in VS Code including conda environments" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_37.png" alt="Screenshot of the selected Python interpreter displayed in the VS Code status bar" width="600px" >}}

---

### **Run a File**

Modify your file with some basic runnable python code

**Run > Run Without Debugging**

Opens a terminal automatically

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_38.png" alt="Screenshot of the VS Code Run menu with Run Without Debugging option highlighted" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_39.png" alt="Screenshot of a Python script executing in the VS Code integrated terminal" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_40.png" alt="Screenshot of Python script output displayed in the VS Code terminal" width="600px" >}}

---

### **Jupyter Notebooks**

Select kernel from the list of available kernels- or make your own!

Recently used kernels are "starred"

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_41.png" alt="Screenshot of the Jupyter kernel selection dialog in VS Code showing available kernels" width="600px" >}}

**Create your own kernel from a conda environment:**

[Custom Jupyter Kernels (RC Documentation)](https://www.rc.virginia.edu/userinfo/howtos/rivanna/custom-jupyter-kernels/)

---

### **Customization**

**File > Preferences**

These changes stay even when you open a new interactive session

You can even download extra extensions!

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_42.png" alt="Screenshot of the VS Code File menu with Preferences option" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_43.png" alt="Screenshot of the VS Code settings panel" width="600px" >}}

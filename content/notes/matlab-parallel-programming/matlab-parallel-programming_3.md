---
title: Accelerating and Parallelizing MATLAB Code
date: 2024-11-16-20:28:40Z
type: docs 
weight: 150
menu: 
    matlab-parallel-programming:
---

To optimize the performance of your MATLAB® code, it’s important to first analyze and address potential bottlenecks before considering approaches like parallel computing or code generation. One effective way to accelerate your MATLAB code is by optimizing your **serial code** through techniques like **preallocation** and **vectorization**.

- **Preallocation** involves initializing an array with its final size before use, which helps prevent the dynamic resizing of arrays, especially in loops (e.g., for and while loops).  
- **Vectorization** replaces loops with matrix and vector operations, enabling MATLAB to process data more efficiently.

Additionally, replacing sections of your code with **MEX-functions**—MATLAB executable files—can yield significant performance improvements. Using **MATLAB Coder™**, you can generate readable and portable C code, which is then compiled into a MEX-function to replace the equivalent MATLAB code.

Before modifying your code, it's important to focus on the most critical areas. The **Code Analyzer** and **MATLAB Profiler** are two essential tools to help identify where optimizations are most needed:

- The **Code Analyzer** works in the MATLAB Editor, checking your code as you write it. It flags potential issues and suggests modifications to improve performance.
- The **MATLAB Profiler** provides detailed timing information about your code’s execution. It shows where your code spends the most time, which functions are called the most, and which lines of code are the most time-consuming. This information helps pinpoint bottlenecks and enables you to streamline your serial code.

Once your serial code is optimized, you can further improve performance by leveraging additional computing resources. **MATLAB parallel computing tools** allow you to tap into the power of multicore processors, computer clusters, and GPUs to accelerate your workflows, ensuring that your code runs more efficiently even as your computational needs scale.

---

## Run MATLAB on multicore machines

{{< figure src=/notes/matlab-parallel-programming/img/matlab-multicore-machines.jpg >}}

MATLAB provides two main approaches to parallel computing: **implicit multithreading** and **explicit parallel computing**.

### Built-in Multithreading (Implicit)
MATLAB automatically enables implicit multithreading, where multiple threads operate within a single MATLAB computation engine. Functions such as `fft`, `eig`, `svd`, and `sort` are multithreaded, meaning they can perform operations using multiple cores without needing any special configuration. This feature leverages underlying multi-threaded libraries and is automatically applied in MATLAB, as well as in many toolboxes like the Image Processing Toolbox, which benefits from core MATLAB function support. However, this implicit support has limitations—some MATLAB functions do not support multithreading, and any performance gains are confined to the local workstation.

### Parallel Computing Using Explicit Techniques
For more control, MATLAB offers **explicit parallel computing**, which involves using multiple computation engines (workers) controlled by a single session. This allows you to explicitly distribute computations across multiple cores or even scale your applications to **clusters** and **clouds**. With tools like the **Parallel Computing Toolbox**, you can use syntax such as `parfor` to control how portions of your workflow are distributed to different cores. This explicit parallelism provides more flexibility and can be extended beyond a single machine to larger computing resources using **MATLAB Parallel Server**. Additionally, MATLAB enables parallel computing on **GPUs**, allowing for even greater computational power when needed.

In summary, implicit multithreading in MATLAB is automatically enabled for many core functions, while explicit parallel computing provides greater flexibility and scalability for large-scale applications across multiple cores, clusters, or clouds.
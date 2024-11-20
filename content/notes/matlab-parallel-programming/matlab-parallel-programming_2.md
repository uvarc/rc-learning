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

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new5.jpg >}}

MATLAB provides two main approaches to parallel computing: **implicit multithreading** and **explicit parallel computing**.

### Built-in Multithreading (Implicit)
MATLAB automatically enables implicit multithreading, where multiple threads operate within a single MATLAB computation engine. Functions such as `fft`, `eig`, `svd`, and `sort` are multithreaded, meaning they can perform operations using multiple cores without needing any special configuration. This feature leverages underlying multi-threaded libraries and is automatically applied in MATLAB, as well as in many toolboxes like the Image Processing Toolbox, which benefits from core MATLAB function support. However, this implicit support has limitations—some MATLAB functions do not support multithreading, and any performance gains are confined to the local workstation.

### Parallel Computing Using Explicit Techniques
For more control, MATLAB offers **explicit parallel computing**, which involves using multiple computation engines (workers) controlled by a single session. This allows you to explicitly distribute computations across multiple cores or even scale your applications to **clusters** and **clouds**. With tools like the **Parallel Computing Toolbox**, you can use syntax such as `parfor` to control how portions of your workflow are distributed to different cores. This explicit parallelism provides more flexibility and can be extended beyond a single machine to larger computing resources using **MATLAB Parallel Server**. Additionally, MATLAB enables parallel computing on **GPUs**, allowing for even greater computational power when needed.

In summary, implicit multithreading in MATLAB is automatically enabled for many core functions, while explicit parallel computing provides greater flexibility and scalability for large-scale applications across multiple cores, clusters, or clouds.


## Utilizing multiple cores on a desktop computer

### Parallel Computing Paradigm Multicore Desktops

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new6.png >}}

---

MathWorks offers two parallel computing tools. The first is Parallel Computing Toolbox which is our desktop solution that allows users to be more productive with their multicore processors by utilizing headless MATLAB sessions called workers to use the full potential of their hardware and speed up their workflow. The Toolbox provides easy to use syntax that allows users to stay in their familiar desktop environment while our solution takes care of the details of using multiple sessions in the background. We make the tough aspects of parallelism easy for users: GPU, Deep Learning, Big Data, Clusters, Clouds, etc.

Parallel ComputingToolbox also provides support for NVIDIA CUDA-based GPUs, and provides the syntax used by our second tool, MATLAB Parallel Server.

Under the hood the parallel computing products divide the tasks/computations/simulations and assigns them to these workers in the background - enabling the computations to execute in parallel.

A key concept for parallel computing with MATLAB is the ability to run multiple MATLAB computational engines that are controlled by a single MATLAB session and that are licensed as workers. A collection of these workers with inter-process communication is what we call a parallel pool. 

The parallel computing products provide features that can divide tasks/computations/simulations and assigns them to MATLAB computational engines in the background - enabling execution in parallel.
 
In general you should not run more MATLAB computational engines than the number of physical cores available.   Otherwise, you are likely to have resource contention.

Note that Parallel Computing Toolbox provides licensing for enough workers to cover all of the physical cores in a single machine.

See also:  https://www.mathworks.com/discovery/matlab-multicore.html 

## Accelerating MATLAB and Simulink Applications

{{< figure src=/notes/matlab-parallel-programming/img/image.png >}}

---

## Demo: Classification Learner AppAnalyze sensor data for human activity classification

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new7.png >}}

* Objective: visualize and classify cellphone sensor data of human activity
* Approach:
  * Load human activity dataset
  * Leverage multicore resources to train multiple classifiers in parallel

---

https://insidelabs-git.mathworks.com/ltc-ae/demos/HumanActivityRecognition

Let's open MATLAB and try out a demonstration of using built in parallel functionality invoked by setting a toggle to invoke parallel. 
In this demo, we are loading IOT sensor data from a mobile phone and then using this sensor data to classify the data into activity-standing, sitting, running, etc. 
The demo will utilize the multiple cores in my parallel pool to train multiple classifiers simultaneously. 


## Demo: Cell Phone Tower OptimizationUsing Parallel-Enabled Functions

* Parallel-enabled functions in Optimization Toolbox
* Set flags to run optimization in parallel
* Use pool of MATLAB workers to enable parallelism

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new8.png >}}

Parallel-enabled Toolboxes (MATLAB® Product Family)

*Enable parallel computing support by setting a flag or preference*

**Image Processing**

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new9.png >}}

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new10.png >}}

Batch Image Processor, Block Processing, GPU-enabled functions

---

Across a wide range of MATLAB toolboxes, all you have to do is indicate that you wish to use parallel using the UseParallel flag or via a toggle switch preference.

**Statistics and Machine Learning**

Resampling Methods, k-Means clustering, GPU-enabled functions

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new11.png >}}

Deep Learning, Neural Network training and simulation

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new12.jpg >}}

---

**Signal Processing and Communications**

GPU-enabled FFT filtering, cross correlation, BER simulations

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new13.png >}}

---

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new14.png >}}

Bag-of-words workflow, object detectors

**Optimization & Global Optimization**

Estimation of gradients, parallel search

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new15.png >}}

---

## Accelerating MATLAB and Simulink Applications

{{< figure src=/notes/matlab-parallel-programming/img/image_copy.png >}}

---


If you want a bit more control, then Parallel Computing Toolbox adds some parallel keywords into the MATLAB language.  An example of this is Parfor or batch commands.


## Explicit Parallelism: Independent Tasks or IterationsSimple programming constructs: parfor

Examples of this include parameter sweeps, and Monte Carlo simulations. There are no dependencies or communications between tasks.

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new16.png >}}

---

How do we express parallelism in code?

If you are dealing with problems that are computationally intensive and are just taking too long because there are multiple tasks/iterations/simulations you need to execute to gain deeper insight   these are ideal problems for parallel computing and the easiest way to address these challenges is to use parallel for loops.

Real-world examples of such problems are parameter sweeps or Monte Carlo simulations.

For example, lets say you have 5 tasks to be completed - If you run these in a FOR loop, they run serially one after the other - you wait for one to get done, then start the next iteration - However if they're all independent tasks with no dependencies or communication between individual iterations - you can distribute these tasks to the MATLAB workers we spoke about - multiple tasks can execute simultaneously  you'll maximize the utilization of the cores on your desktop machine and save up on a lot of time !
Requirements for parfor loops 
Task independent
Order independent

Constraints on the loop body
Cannot “introduce” variables (e.g. load, etc.)
Cannot contain break or return statements
Cannot contain another parfor loop


## Explicit Parallelism: Independent Tasks or Iterations

```
for i = 1:5
  y(i) = myFunc(myVar(i));
end
```

```
parfor i = 1:5
  y(i) = myFunc(myVar(i));
end
```

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new17.png >}}

---

For example, lets say you have 5 tasks to be completed - If you run these in a FOR loop, they run serially one after the other - you wait for one to get done, then start the next iteration- However if they're all independent tasks with no dependencies or communication between individual iterations - you can distribute these tasks to the MATLAB workers we spoke about - multiple tasks can execute simultaneously  you'll maximize the utilization of the cores on your desktop machine and save up on a lot of time !

## Mechanics of parfor Loops

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new18.png >}}

## Tips for Leveraging parfor

Consider creating smaller arrays on each worker versus one large array prior to the parfor loop

Take advantage of parallel.pool.Constant to establish variables on pool workers prior to the loop

Encapsulate blocks as functions when needed

## Accelerating MATLAB and Simulink Applications

{{< figure src=/notes/matlab-parallel-programming/img/image_copy_copy.png >}}

---

Finally, if you need lots of control in your program, then we have some advanced programming constructs that will let you do things like send messages between workers.

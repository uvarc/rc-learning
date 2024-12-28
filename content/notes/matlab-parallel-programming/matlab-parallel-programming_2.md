---
title: Utilizing multiple cores on a desktop computer
date: 2024-11-16-20:28:40Z
type: docs 
weight: 150
menu: 
    matlab-parallel-programming:
---

### Parallel Computing Paradigm Multicore Desktops

{{< figure src=/notes/matlab-parallel-programming/img/parallel-computing-toolbox.png >}}

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

{{< figure src=/notes/matlab-parallel-programming/img/explicit-parallelism.png >}}

---

## Demo: Classification Learner App

*Analyze sensor data for human activity classification*

{{< figure src="/notes/matlab-parallel-programming/img/classification-demo.png" height="200px" >}}

* Objective: visualize and classify cellphone sensor data of human activity
* Approach:
  * Load human activity dataset
  * Leverage multicore resources to train multiple classifiers in parallel

---

Check out more here: https://insidelabs-git.mathworks.com/ltc-ae/demos/HumanActivityRecognition

Let's open MATLAB and try out a demonstration of using built in parallel functionality invoked by setting a toggle to invoke parallel. 
In this demo, we are loading IOT sensor data from a mobile phone and then using this sensor data to classify the data into activity-standing, sitting, running, etc. 
The demo will utilize the multiple cores in my parallel pool to train multiple classifiers simultaneously. 


## Demo: Cell Phone Tower OptimizationUsing Parallel-Enabled Functions

* Parallel-enabled functions in Optimization Toolbox
* Set flags to run optimization in parallel
* Use pool of MATLAB workers to enable parallelism

{{< figure src="/notes/matlab-parallel-programming/img/cellphone-optimization-demo.png" height="200px" >}}

## Parallel-enabled Toolboxes (MATLAB® Product Family)

### *Enable parallel computing support by setting a flag or preference*

---

{{< figure src=/notes/matlab-parallel-programming/img/matlab-toolboxes.png >}}

---


If you want a bit more control, then Parallel Computing Toolbox adds some parallel keywords into the MATLAB language. An example of this is Parfor or batch commands.


## Explicit Parallelism: Independent Tasks or Iterations

*Simple programming constructs: parfor*

Examples of this include parameter sweeps, and Monte Carlo simulations. There are no dependencies or communications between tasks.

{{< figure src=/notes/matlab-parallel-programming/img/explicit-parallelism-diagram.png >}}

---

How do we express parallelism in code?

If you are dealing with problems that are computationally intensive and are just taking too long because there are multiple tasks/iterations/simulations you need to execute to gain deeper insight   these are ideal problems for parallel computing and the easiest way to address these challenges is to use parallel for loops.

Real-world examples of such problems are parameter sweeps or Monte Carlo simulations.

For example, lets say you have 5 tasks to be completed - If you run these in a FOR loop, they run serially one after the other - you wait for one to get done, then start the next iteration - However if they're all independent tasks with no dependencies or communication between individual iterations - you can distribute these tasks to the MATLAB workers we spoke about - multiple tasks can execute simultaneously  you'll maximize the utilization of the cores on your desktop machine and save up on a lot of time!

**Requirements for parfor loops:**
- Task independent
- Order independent

**Constraints on the loop body**
- Cannot “introduce” variables (e.g. load, etc.)
- Cannot contain break or return statements
- Cannot contain another parfor loop

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
---

For example, lets say you have 5 tasks to be completed - If you run these in a FOR loop, they run serially one after the other - you wait for one to get done, then start the next iteration- However if they're all independent tasks with no dependencies or communication between individual iterations - you can distribute these tasks to the MATLAB workers we spoke about - multiple tasks can execute simultaneously  you'll maximize the utilization of the cores on your desktop machine and save up on a lot of time !

## Mechanics of parfor Loops

{{< figure src=/notes/matlab-parallel-programming/img/parforloop-mechanics.png >}}

## Tips for Leveraging parfor

- Consider creating smaller arrays on each worker versus one large array prior to the parfor loop
- Take advantage of parallel.pool.Constant to establish variables on pool workers prior to the loop
- Encapsulate blocks as functions when needed

---

Finally, if you need lots of control in your program, then we have some advanced programming constructs that will let you do things like send messages between workers.

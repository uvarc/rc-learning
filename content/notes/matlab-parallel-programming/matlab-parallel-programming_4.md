---
title: Scaling up to cluster and cloud resources
date: 2024-11-16-20:28:40Z
type: docs 
weight: 200
menu: 
    matlab-parallel-programming:
---


## Take Advantage of Cluster Hardware

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new19.png >}}

There are several compelling reasons to offload your Parallel Computing Toolbox workflow from a desktop computer to a cluster. By doing so, you can **free up your desktop** for other tasks and take advantage of **more powerful computing resources** available on the cluster. For instance, you can submit jobs to the cluster and retrieve the results once they’re completed, allowing you to shut down your local computer while the job runs.

One key benefit of using a cluster is the ability to **scale speed-up**. By utilizing more cores, you can reduce computation time significantly—what would normally take hours can be reduced to just minutes. This enables faster iterations, allowing you to make updates to your code and run multiple experiments in a single day.

Additionally, clusters offer the ability to **scale memory**. If your array is too large to fit into your local computer's memory, you can use **distributed arrays** to split the data across multiple computers. Each computer stores only a portion of the array, making it possible to work with much larger datasets. Many MATLAB matrix operations and functions are enhanced to work with these distributed arrays, enabling you to operate on the entire array as a single entity within your desktop session, without needing to recode your algorithms. For more details on which functions have been enhanced for distributed arrays, refer to the **Release Notes** for recent updates to the Parallel Computing Toolbox.

## Parallel Computing ParadigmClusters and Clouds

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new20.png >}}

---

The problems or challenges you work on might need additional computational resources or memory than what is available on a single multicore desktop computer.

You can also scale up and access additional computational power or memory of multiple computers in a cluster in your organization or on the cloud. When we say scale up it essentially means that your pool or workers are now located on the cluster computers instead of the cores of your desktop computer. Irrespective of where your pool of workers are, your interface remains in the MATLAB Desktop. We've separated the algorithm from the infrastructure so you can write your code as you always do. 

You might want to perform computations on a cluster to free up your desktop computer for other work.   You can submit jobs to a cluster and retrieve the results when they're done.   You can even shut down your local computer while you wait.   

You can also use a cluster to scale an application that you've developed on your desktop.   Getting computations to take minutes rather than hours allows you to make updates to code and execute multiple runs all in the same day. 

---

## **batch Simplifies Offloading Serial Computations**  
### Submit jobs from MATLAB, free up MATLAB for other work, access results later  

To offload work from your MATLAB session and run in the background in another session, use the `batch` command inside a script.  
Jobs will run in the background, allowing you to access results later.  

#### Example Commands:  
1. Basic usage:  
   ```matlab
   job = batch('myfunc');
   ```  
2. With a parallel pool:  
   ```matlab
   job = batch('myfunc', 'Pool', 3);
   ```  

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new22.png width="500px" >}}

---

## Why parallel computing matters
### Scaling with a compute cluster

{{< figure src=/notes/matlab-parallel-programming/img/image_copy.png >}}

---

In this example, you see a parameter sweep in which we run up to 160,000 different configurations.
If my problem is well-sized, I can get more than a 90x speed-up with 100 workers.

Just adding more workers is not a guarantee of more speed-up, though.  Every application has its limits, and eventually overhead will dominate.

When choosing how many workers you should use, it's best to think about your overall needs,  in this case, even with 64 workers, I can go from 4 hours to just 4 minutes, which might be good enough.  

As mentioned previously, the sweet spot is ultimate execution  on the order of a few minutes.

## Parallel Computing with Matlab

### Useful Links:

**[Scale Up from Desktop to Cluster](https://www.mathworks.com/help/parallel-computing/scale-up-from-desktop-to-cluster.html)**

**[Choose a Parallel Computing Solution](https://www.mathworks.com/help/parallel-computing/choosing-a-parallel-computing-solution.html)**

**[Parallel Computing Toolbox Documentation](https://www.mathworks.com/help/parallel-computing/)**

**[Benchmarks for Matlab Parallel Code](https://www.mathworks.com/help/parallel-computing/benchmarks.html)**

**[Parallel Computing Toolbox Release Notes](https://www.mathworks.com/help/parallel-computing/release-notes.html)**

## Key functionality

|  | Description | Functionality | Ease of use | Control |
| :-: | :-: | :-: | :-: | :-: |
| **Parallel Enabled Toolboxes** | Ready to use parallelized functions in MathWorks tools | MATLAB and Simulink parallel enabled functions<br />Toolbox integration | Turnkey-automatic | Minimal (presets) |
| **Common programming constructs** | Constructs that enable you to easily parallelize your code | parfor<br />gpuArray<br />batch<br />distributed/tall<br />parsim<br />parfeval | Simple | Some |
| **Advanced programming constructs** | Advanced parallelization techniques | spmd<br />arrayfun/pagefun<br />CUDAKernel<br />MapReduce<br />MATLAB Spark API | Advanced | Extensive |

## Migrate to Cluster / Cloud

Use MATLAB Parallel Server

Change hardware without changing algorithm

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new25.png >}}

## Use Multiple Nodes to Quickly Find the Optimal Network

* Experiment Manager App
  * Manage experiments and reduce manual coding
  * Sweep through a range of hyperparameter values
  * Compare the results of using different data sets
  * Test different neural network architectures
* MATLAB Parallel Server
  * Enables multiple nodes to train networks in parallel -> greatly reduce testing time
  * Running many experiments to train networks and compare the results in parallel

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new26.png >}}

## Broad Range of Needs and Cloud Environments Supported

{{< figure src=/notes/matlab-parallel-programming/img/Matlab-Parallel-ProgrammingFall23_new28.png >}}

| Access requirements | Desktop in the cloud | Cluster in the cloud<br />(Client can be any cloud on on-premise desktop) |
| :-: | :-: | :-: |
| **Any user could set up** | NVIDIA GPU Cloud  | MathWorks Cloud Center |
| **Customizable template-based set up** |  MathWorks Cloud Reference Architecture | MathWorks Cloud Reference Architecture |
| **Full set-up in custom environment** | Custom installation - DIY | Custom installation - DIY |

Learn More: [Parallel Computing on the Cloud](http://www.mathworks.com/products/parallel-computing/parallel-computing-on-the-cloud/index.html)

---


Parallel Computing Toolbox and MATLAB Parallel Server allow you to easily extend your execution of  MATLAB and Simulink to more resources.

In the parallel computing workflow, you start with MATLAB on the desktop, and incorporate parallel features from Parallel Computing Toolbox to use more resources.   

You can use Parallel Computing Toolbox in a range of environments, from those which can be set up by users on their own laptop to cloud installations for enterprise deployment.

When you need to scale beyond the desktop, you will need access to a cluster that has MATLAB Parallel Server installed.  This might be a cluster managed by your IT department, or it could be a cloud cluster that you set up on your own with Cloud Center or a MathWorks reference architecture.

Again, there are a range of environments and ways you can access MATLAB Parallel Server, from self-serve options like Cloud Center and the customizable reference architectures to integration with your existing cluster and cloud infrastructure.

Once you have access to MATLAB Parallel Server, you can use parallel resources on the cluster in the same way you did on the desktop, without needing to re-code algorithms.

Note: NVIDIA GPU Cloud is actually a container, and can be run in the cloud or on-premise.

For MATLAB Parallel Server, see: https://www.mathworks.com/products/matlab-parallel-server/get-started.html
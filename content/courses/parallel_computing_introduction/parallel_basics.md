---
title: "Parallel Computing Fundamentals"
toc: true
type: docs
weight: 2
menu:
    parallel_programming:
        parent: Introduction to Parallel Programming
        weight: 2
---

Most computer programs are _serial_ (also called sequential).  One operation at a time is executed, one after the other in the sequence specified by the programmer.  The time for the execution of such a program is fixed for a given problem size.  
Serial programs have at least two disadvantages.  First is the time to solution.
There is no opportunity to obtain a result more quickly.  Second may be a limit on the problem size that can be attacked.  A serial program and all its data must fit together in the memory assigned to it by the operating system.  Parallel programming seeks to eliminate one or both of these issues.

Parallel Computing refers to executing more than one process at a time to solve the same problem.  
Processes may or may not need to communicate with one another while they are running.
The degree of communication required, the type of hardware available, and the organization of the code determines the type of parallelization chosen.  Parallel computing requires at minimum multiple cores on a computer, and may involve
multiple computers; in most cases it is carried out on large high-performance computing systems.

## Why Do We Need Parallel Computing

Programming in parallel is more difficult than serial coding.  What makes it worth the effort?  To understand this, we must understand a few things about how computers work.

Roughly speaking, a conventional computer consists of a central processing unit (CPU) and memory for temporary working storage.  Usually there is also some form of permanent storage such as a disk drive.  Newer computers may utilize advanced permanent memory for both temporary and permanent storage, but the basic model is still accurate.  Working storage is hierarchical, with speed and price decreasing the farther from the CPU.  A very small amount of very fast memory called _registers_ is part of the CPU.  Next is _cache_, usually in two to three levels, which is close to the CPU.  Main memory is usually RAM (random access memory); it is volatile, meaning that its contents disappear when power is removed, it is much larger than cache, and it is also much slower.  The CPU accesses RAM through an internal communication network.  The CPU also contains an input/output controller to manage devices such as hard drives.
RAM memory is many times slower than cache, up to 100 times, and hard drives are slower still, up to 10,000 times slower.  

The activities of the CPU's electronics are synchronized by a system clock; each instruction carried out by the CPU requires a certain number of _clock cycles_.  For example, some recent computer chips can do floating-point addition or multiplication in one or two clock cycles, whereas a division could take 20 or more cycles.  Since the speed of light ultimately limits signals, higher clock
speeds require denser electronic components.
For decades, improvements in computing performance were a result of ever-increasing clock speeds.  This followed two "laws" (both of which were actually observational):  Moore's law, which states that the density of the electronic components of the CPU would double every 18 to 24 months; and Dennard scaling, which posited that even as the components shrank, their power consumption per unit area remained constant.  

{{< figure src="/courses/parallel_computing_introduction/Moores_Law.jpg" caption="Copyright 2015 A. Ostendorf and K. König, published by De Gruyter.  Licensed under the Creative Commons Attribution-NonCommercial-NoDerivs 3.0 License." height=720 width=563 >}}

Moore's law is currently still approximately valid, though its end is in sight; but Dennard scaling broke down around 2005.  Further significant increases in clock speed would make it impossible to cool the chips adequately.  Manufacturers responded by keeping clock speeds nearly constant, but dividing their still-denser CPUs into separate execution units, usually called _cores_.  Because of this history there is still some inconsistency over what is called a "CPU" and what is called a "core."  Generally, cores are subunits of a CPU, but sometimes, especially in contexts such as the SLURM resource manager, a core is called a CPU.  Programmers should try not to become confused by this.

{{< figure src="/courses/parallel_computing_introduction/Dennard_Scaling.png" >}}

Most consumer computers have only one CPU, but many high-end servers have multiple CPUs, with each CPU representing several cores.  The CPU is physically plugged into a socket on the motherboard, so these CPU units may be referred to as "sockets."  Each CPU will have
its own pathway to memory, disk, and so forth.  

Computers can be connected to one another through some kind of _network_.  The best-known network is Ethernet. Networking allows multiple computers to be connected in physical proximity to one another, forming a _cluster_.  Each member computer in the cluster is usually
called a _node_.  Ethernet can be used for the interconnect, but Ethernet is a relatively slow network.  The time required for a message to be sent from one computer to another is the _latency_, whereas the amount of data that can be sent per unit time is the _bandwidth_.  Ethernet generally has fairly good bandwith but quite high latency.  Since fast communication between nodes is important in a
high-performance cluster, interconnects with much lower latency and higher bandwidth are generally provided for these systems, usually along with Ethernet for routine communication.  The most widely used of these fast networks is InfiniBand, now owned by NVIDIA.  

{{< figure src="/courses/parallel_computing_introduction/Frontera.jpg" height=720 width=563 caption="A portion of a large computing cluster." >}}

## Adapting to a Parallel Environment

Most programmers are taught only serial coding and are accustomed to laying out programs in a sequential manner.  Parallel computing brings new considerations that must be mastered.

### Types of Parallelism

For both shared and distributed memory programming, we must determine how we will spread the work among different processes.  There are two main ways to do this.

- Data parallelism

    In this case, we divide the data into subparts.  Each process works on its assigned part individually, then if necessary the results are collected and the program goes to next phase.
Independent tasks apply the same operation to different elements of a data set.

-  Task parallelism

    In task parallelism the processes perform multiple tasks at the same time on the same data. Independent tasks apply different operations to different data elements.

**Example**

The landscape service has several tasks to perform.   

- Turn off the security system to access the client's garage.

- Mow the lawn.

- Edge the lawn.

- Weed the garden.

- Turn on water to the sprinklers, check the timer attached to the hose.

- Turn the security system back on.

We can draw a graph to help us understand the sequence of events.

{{< diagram >}}
graph TD;
A(Turn off Security System) --> B(Edge Lawn)
A(Turn off Security System) --> C(Mow Lawn)
A(Turn off Security System) --> D(Weed Garden)
B(Edge Lawn) --> E(Set Sprinklers)
C(Mow Lawn) --> E(Set Sprinklers)
D(Weed Garden) --> E(Set Sprinklers)
E(Set Sprinklers) --> F(Turn on Security System)
{{< /diagram >}}
What can be done in parallel and what must be serial?
What portion is task parallelism?  Is there an opportunity for data parallelism? Assume sufficient staff and equipment are available.

## Developing Parallelization Strategies

### Dependencies

A _dependency_ occurs when the result of an operation relies on the result of a previous one.  Three types occur.

#### Data Dependency

A data dependency occurs when the ordering of calculations matters.  A "true" data dependency is called a _flow dependency_ and it cannot be eliminated without reworking the code.

```plaintext
X = 42
Y = X
Z = Y
```
Y cannot be computed until X has been set, and Z relies in turn on Y.

An _anti-dependency_ occurs when a calculation uses a value that is later modified.
```plaintext
X = 42
Y = 12
X = X+1
```
This type of dependency may be eliminated by introducing new variables:
```plaintext
X = 42
X2= X
Y = X2+1
X = X+1
```
However, we now have a flow dependency because X2 must be set before Y can be computed.

The final category of data dependency is the _output dependency_.  This does not necessary refer to printing but to the "output" value of a variable.  In this example we can assume we are mainly interested in some result from `myfunc` other than the return value `X`.
```plaintext
X = myfunc(Z)
X = X+1
```
As is the case for anti-dependencies, output dependencies can be corrected by renaming variables.
```plaintext
X2 = myfunc(Z)
X  = X+1
```

#### Control Dependency

A control dependency occurs when whether a statement is executed depends on the result of a previous statement.
```plaintext
if A>0 then
   X=sqrt(A)
end
Y=X+1
```

#### Managing Dependencies

Some dependencies are unavoidable; most programs will contain them.  However, sometimes a change of algorithm can reduce or eliminate some previously present.  For example, suppose we wish to solve the matrix equation
$$ Ax = b $$
where we will assume $A$ is a square matrix.  Many equations of this type that occur in scientific and engineering applications can be solved through an iterative method; this is generally faster and requires less memory than a direct method.  A popular iterative method is the _Gauss-Seidel_ method.  We will not go into details of the derivation; the result is that we write the corrected version of the solution as
$$ x_{i}^{k+1} = \frac{1}{a_{ii}}( b_i - \sum_{j=1}^{i-1}a_{ij}^{k+1} + \sum_{j=i+1}^{n}a_{ij}^{k+1}), i=1,2,...,n $$
where $k$ represents the iteration number and the matrix is $n \times n$.
In pseudocode this is expressed for each iteration as
```plaintext
for i in 1 to n
    t1 = 0
    for j in 1 to i-1
        t1 = t1+a[i,j]*x[j]
    end
    t2 = 0
    for j in i+1 to n
        t2 = t2 + a[i,j]*x[j]
    end
    x[i] = (b[i] - t1 - t2)/a[i,i]
```
This is repeated until some specified tolerance is achieved.  The solution vector `x` is continuously updated as we sweep through the rows.  Utilizing the new information increases the rate of convergence, but introduces a flow dependency.

An older method, called _Jacobi_ iteration, is nearly identical but utilizes two variables for the solution, representing $x^{k}$ and x^{k+1}.  
The pseudocode in this case is
```plaintext
for i in 1 to n
    t1 = 0
    for j in 1 to i-1
        t1 = t1+a[i,j]*x_old[j]
    end
    t2 = 0
    for j in i+1 to n
        t2 = t2 + a[i,j]*x_old[j]
    end
    x[i] = (b[i] - t1 - t2)/a[i,i]
x_old=x
```
This method is slower in serial, sometimes considerably so, but is easy to parallelize.

### Granularity

An important factor in parallelization strategies is the *granularity* of the work.
A coarse-grained distribution divides a large quantity of computation among the tasks, with relatively less communication or
synchronization required.  A fine-grained distribution assigns a smaller quantity of work to each task and does relatively more communication and synchronization.
The granularity determines whether special hardware might be required, and may drive a choice between shared or distributed memory programming.  Some programs contain both fine and coarse granularity and may be suited for a combination approach.  Often a program is
actually _medium grained_, intermediate between a truly fine-grained scenario and a coarse-grained approach.

Some algorithms are inherently fine-grained while others are coarse-grained.  In other situations, the programmer has control over the degree of granularity, through choices about data decomposition.

**Example**

A post-production company has several thousand frames of a movie to process.  Each frame consists of some number of pixels. 

- Fine grained
    Divide each frame into subunits.  Process each subunit's pixels as a separate task.  Reconstruct the finished image.
- Coarse grained
    Process each frame as a separate task.

### Load Balancing

In parallel codes, the runtime is determined by the slowest process
The computational load should be distributed so that each process does approximately the same share of the work.

**Example**

With a fixed grid size a general-circulation model must do more computations over land than over ocean.  A possible solution is to use smaller grid sizes over land, so that the amount of work per grid zone is roughly constant.

### Algorithm Selection

Some algorithms that are fast in serial cannot be parallelized efficiently or at all.
Conversely, some algorithms that are relatively slow in serial are easily parallelizable.
We have already seen one example of this when we looked at Gauss-Seidel iteration compared to Jacobi iteration.  In this case, the algorithm that is faster in
serial has a dependency which prevents parallelization.  
But even some algorithms that can be parallelized in principle perform poorly;
the inherent granularity and how well the load can be balanced can play key
roles in the success of a parallelization project.

## Performance Analysis

### Speedup and Efficiency

The purpose of making the effort to convert our programs to parallelism is a shorter time to solution.
Thus we are interested in the **speedup** of a parallel code relative to running
the same algorithm in serial.  Speedup is a simple computation: 

$$ Speedup = \frac{sequential\ execution\ time}{parallel\ execution\ time} $$

Let us consider a problem of size $n$, where $n$ represents a measure of the
amount of work to be done.  For example, it could be the first dimension of a 
square matrix to be inverted, or the number of pixels to analyze in an image. 
Now assume we will run this on $p$ processes.  Let
$$ \sigma (n) $$
be the part of the problem that is inherently sequential.  Let 
$$ \phi (n) $$
be the part that is potentially parallelizable.  Any parallelization will
require some communication among the processes; the impact of this will be a 
function of both $n$ and $p$.  Let us represent this as 
$$ \kappa (n,p) $$

On a single processor, the total time will depend on
$$ \sigma (n) + \phi (n) $$
On $p$ processors, on the other hand, the time will depend on
$$ \sigma (n) + \phi (n)/p + \kappa (n,p) $$
Therefore we can express the speedup as
$$ \psi(n,p) = \frac{\sigma(n)+\phi(n)}{\sigma(n)+\phi(n)/p+\kappa(n,p)} $$

{{< figure src="/courses/parallel_computing_introduction/parallel_speedup.png" caption="As the number of processes grows, overhead also grows and can become dominant." >}}

This simple formula shows that we would achieve the maximum possible speedup if $\sigma(n)$ and $\kappa(n,p)$ are negligible compared to $\phi(n)$.  In that case we obtain the approximation
$$ \psi(n,p)\approx \frac{\phi(n)}{\phi(n)/p} = p $$
Thus the ideal is that speedup should be
linear in the number of processes.  In some circumstances, the actual speedup
for at least a limited number of processes can exceed this.  This is called
_superlinear_ speedup.  One fairly common cause of this is _cache efficiency_.  When the problem is broken into smaller subdomains, more of the data may fit
into cache.  We have seen that cache is much faster than main memory, so 
considerable gains may be achieved with improved cache efficiency.  Some other
reasons include better performance of the algorithm over smaller data, or even
moving to an algorithm that is better but only in parallel, since speedup is computed 
relative to the best sequential algorithm. 

The **efficiency** is the ratio of the serial execution time to the parallel time multiplied by $p$.  Using the above notation we obtain

$$ \epsilon(n,p)=\frac{\sigma(n) + \phi(n)}{p\sigma(n) + \phi(n) + p\kappa(n,p}$$

From this it should be apparent that 
$$ 0 < \epsilon(n,p) \le 1 $$


### Amdahl's Law

Define
$$ f = \frac{\sigma(n)}{\sigma(n)+\phi(n)} $$
This is the fraction of the work that _must_ be done sequentially.  Perfect
parallelization is effectively impossible, so $f$ will never be zero.  
In the speedup formula, consider the limiting case for which $\kappa(n,p) \to 0$.
Thus
$$ \psi(n,p) \lt \frac{\sigma(n) + \phi(n)}{\sigma(n) + \phi(n)/p} $$
Rewriting in terms of $f$ gives

$$ \psi \le \frac{1}{f + (1-f)/p} $$

In the case that $p \to \infty$ this becomes

$$ \psi \lt \frac{1}{f} $$

Thus the irreducibly sequential portion of the code limits the theoretical speedup.  This is known as **Amdahl's Law**.

**Example**

Suppose 95% of a program’s execution time occurs inside a loop that can be executed in parallel, and this is the only parallelizable part of the code. What is the maximum speedup we should expect from a parallel version of the program executing on 8 CPUs?  What is the efficiency?  What is the maximum theoretical
speedup?

In this case
$$ f = 0.05 $$

So
$$ \psi \le \frac{1}{0.05+0.95/8} = 5.92 $$
$$ \epsilon = \frac{t_{seq}}{8t_{seq}/5.92} = 0.74 $$
$$ \psi_{inf} = \frac{1}{.05} = 20 $$

{{< figure src="/courses/parallel_computing_introduction/strong_speedup.png" caption="Speedup is very sensitive to the fraction of the program that can be parallelized." >}}

### Scalability

The Amdahl effect is the observation that as the problem size, represented by
$n$, increases, the computation tends to dominate the communication and other
overhead.  This suggests that more work per process is better, up to various
system limits such as memory available per core.

The degree to which efficiency is maintained as the number of processes increases
is the _scalability_ of the code.  Amdahl's Law demonstrates that a very high degree of parallelization is necessary in order for a code to scale to
a large number of processes for a fixed amount of work.  
This can be difficult to achieve.  However, parallelization has other
benefits.  Dividing work among more cores can reduce the memory footprint
per core to the point that a problem that could not be solved in serial can
be solved.  It can also 

#### Strong Scaling Vs. Weak Scaling

- Strong scaling: the same quantity of work is divided among an increasing number of processes.

- Weak scaling: the amount of work per process is fixed, while the number of processes increases.

Amdahl's law shows that strong scaling will always
be limited. We must achieve a parallel fraction of close to 99% in order for the
program to scale to more than around 100 processes.
Many parallel programs are intended to solve large-scale problems, such as
simulating atmospheric circulation or the dynamics of accretion disks around
black holes, so will require hundreds or even thousands of processes to
accomplish these goals; strong scaling is not feasible for these types of
problem.  

The counterpart to Amdahl's Law for weak scaling is **Gustafson's Law**.  
Here we fix not the size of the problem but the execution time.
Using the same notation as before, with $f$ the fraction of the workload
that must be executed sequentially, the sequential time that would 
be required for the problem size corresponding to $p$ processes will be
$$ t_s = ft + (1-f)pt $$
where $t$ is the fixed execution time.  
The speedup is $t_s/t$ which yields
$$ \psi = f + (1-f)p $$
The efficiency is 
$$ \epsilon = \frac{S}{p} = \frac{f}{p}+1-f $$
This expression ignores real-world complications such as communication and
other system overhead, but it provides a guide for understanding weak scaling.  
In the case of $p \to \infty$, $\epsilon \to 1-f$.  Therefore, as for Amdahl's law,
the sequential fraction will limit the speedup and efficiency possible. 
However, for weak scaling, lesser parallelization can still produce 
acceptable efficiencies.
{{< figure src="/courses/parallel_computing_introduction/weak_speedup.png" caption="Speedup for weak scaling." >}}

Weak scaling allows a much larger workload to be run in the same time.  A significant portion of 
scientific and engineering problems require a large to very large workload
for problems of interest to be solved.  For example, fluid dynamics is modeled
better at higher numerical resolutions, but each computational cell adds to 
the problem size.  Many problems, such as weather models, could not be solved at all at the resolution of interest without weak scaling.

## Parallelism and Computer Hardware

Our discussion so far has applied to an abstract computer with little 
reference to the physical machines.  We are running multiple "processes," vaguely defined, on some unspecified computer architecture.  However, the hardware
affects what types of parallelism we can apply and vice versa.

### High-Throughput Computing.

High-throughput computing (HTC) is also called "embarrassingly parallel" or "pleasingly parallel" computing  In this form of parallelism, independent processes with little or no need to communicate are run simultaneously.  This can involve data parallelism or task parallelism or both.  High-throughput workloads can
be run on any type of computer as long as multiple processes can be started 
on the system.  

Examples of HTC include

- Image processing, where the analysis of each frame is completely independent of all other frames.  

- Monte Carlo simulations.  Hundreds or thousands of models are run with randomized parameters.  

In principle, HTC is 100% efficient; in practice, some gathering and processing of results is necessary.
For instance, in the image-processing example the end product may be a movie,
in which case the frames will have to be encoded into a new file.  

### Shared-Memory Computing

Modern computers are essentially all multicore.  All cores share the same
main memory.  In most multicore systems, all cores have equal access to main
memory; hence these are called _symmetric multiprocessing_ (SMP) systems.   
They may not share the same cache, however.  Some hardware utilizes a variant
called NUMA (non-uniform memory access), particularly servers with more than
one CPU socket.  Graphical processing units (GPUs) are essentially shared-memory processors as well.  
Shared-memory parallel programs rely on internal
communication and coordination mechanisms, rather than external networks.  
SMP programs are implemented through libraries that communicate with the operating system to manage _threads_.  
For a parallel program, an _initial thread_ is created and subordinate threads
are generated as requested by the threading library.  
Popular threading libraries are OpenMP and pthreads (POSIX Threads).  OpenMP is a standard
that is implemented within the compiler. Pthreads is independent
of the compiler but is written in C; usually a version built with the
system libraries is available and can be used with most compilers.  
OpenACC, which is specific to GPUs, is similar to OpenMP in that it is
a compiler-provided library.

Since OpenMP is a standard and is provided by compiler vendors, it can
be used on any operating system for which the necessary compiler is installed; in particular it is available for Linux, Windows, and MacOS.  The version supported
and the quality of the implementation will depend on the compiler vendor.
Pthreads originated on Unix and can be used on MacOS, which is Unix-based, but it is not
native on Windows though a wrapper is available.

In shared-memory programming, the abstract "process" we have discussed 
corresponds to a thread.  It is generally desirable that each thread be
run on its own core, or logical core in the case of _hyperthreading_ hardware.
However, only a _single_ copy of the executable is run.  The initial thread
manages the others.  In OpenMP, data distribution among the threads is handled by
the OpenMP library, though the programmer must specify certain attributes in
most cases.  

{{< figure src="/courses/parallel_computing_introduction/SMP.png" caption="Schematic of an SMP system." >}}

### Distributed-Memory Computing

Large programs must generally be run on a cluster consisting of multiple
compute nodes.  Each node is an independent computer connected to the others
through an external network.  Because they are independent, one node has no
direct access to the memory of any other node.  Data must be transferred over
the network from one node to another.  This is accomplished by a library 
that manages the communication, invoked through an interface to the 
program.  By far the most widely used communication library is MPI, the
Message-Passing Interface.  When communication is required, one node 
sends a "message" to one or more other nodes.  The message consists of
the data to be exchanged, along with some information about source, 
destination, and other identifiers.  

In distributed-memory parallelism, each process corresponds to a _separate_
copy of the program's executable.  All processes are identical; different
behavior for communication purposes must be managed by conditionals in the
program.  Data decomposition is entirely the responsibility of the programmer.

{{< figure src="/courses/parallel_computing_introduction/DMP.png" caption="Schematic of a DMP system." >}}

### Hybrid Parallelism

Hybrid parallel programming is a combination of shared and distributed memory programming.
Tasks and possibly some of the data are divided among the distributed
executables that will run.  Each exectable also uses a threading library
to split the work among the cores available on the node where it is running.
Most commonly, this involves a combination of MPI for larger-scale data
distribution and OpenMP at a smaller scale, often at the level of loops over
data arrays.

### GPU Parallelism

GPUs are essentially SMP processors with a large number, hundreds or even thousands, of lightweight "cores"; each core has a limited set of instructions it can carry out.  
The GPU has its own onboard RAM memory but relatively little cache.  Efficient
GPU programming keeps the cores busy with computations and reduces the
number of times they must fetch data from or store data to their main memory.
General-purpose GPUs are functionally "coprocessors" and data must be moved
to and from the host to the GPU.  This communication can be quite expensive
and should also be minimized for efficient utilization of the system. GPUs
are highly optimized for data-parallel threaded coding.  Several libraries
are in use including CUDA for NVIDIA devices, OpenACC, and extensions to
OpenMP.  OpenCL is another popular library that aims to support a variety
of parallel architectures, including CPUs, GPUs, and FPGAs (field-programmable
gate arrays, customizable chips often used in embedded applications and artificial neural networks).

---
title: "Developing Parallelization Strategies"
toc: true
type: docs
weight: 3
menu:
    parallel_programming:
        parent: Introduction to Parallel Programming
        weight: 3
---

## Dependencies

A _dependency_ occurs when the result of an operation relies on the result of a previous one.  Three types occur.

### Data Dependency

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

The final category of data dependency is the _output dependency_.  This does not necessary refer to printing but to the "output" value of a variable.  
```plaintext
X = myfunc(Z)
X = X+1
```
As is the case for anti-dependencies, output dependencies can be corrected by renaming variables.
```plaintext
X2 = myfunc(Z)
X  = X+1
```

### Control Dependency

A control dependency occurs when whether a statement is executed depends on the result of a previous statement.
```plaintext
if A>0 then
   X=sqrt(A)
end
Y=X+1
```

### Managing Dependencies

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

## Granularity

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

## Load Balancing

In parallel codes, the runtime is determined by the slowest process
The computational load should be distributed so that each process does approximately the same share of the work.

**Example**

With a fixed grid size a general-circulation model must do more computations over land than over ocean.  A possible solution is to use smaller grid sizes over land, so that the amount of work per grid zone is roughly constant.

## Algorithm Selection

Some algorithms that are fast in serial cannot be parallelized efficiently or at all.
Conversely, some algorithms that are relatively slow in serial are easily parallelizable.
We have already seen one example of this when we looked at Gauss-Seidel iteration compared to Jacobi iteration.  In this case, the algorithm that is faster in
serial has a dependency which prevents parallelization.  
But even some algorithms that can be parallelized in principle perform poorly;
the inherent granularity and how well the load can be balanced can play key
roles in the success of a parallelization project.

---
title: "MPI Project Set 1"
toc: true
type: docs
weight: 59
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

## Project 1

A very inefficient way to compute $\pi$ is to use a Monte Carlo method. In this we randomly throw “darts” at a circle of radius 1 within a square with sides of length 2. The ratio of the areas is thus $\pi \div 4$. If we compute the ratio of “hits” that land within the circle to the total number of throws, then our estimate of $\pi$ is $4 \times \mathrm{ratio}$. 

 Write a program that computes pi by this method. Start with a serial code and test it for a relatively small number of “dart throws.” You will find it very inaccurate for a small number of throws but should be able to obtain an estimate at least close to 3.  Once you are satisfied that your program works in serial, parallelize it.  Distribute the throws over N processes and use a reduction to get the totals for your overall estimate of pi.  Test your parallel code with 8 processes and 1e9 throws.  Hint: C++ programmers should use _long long_ for the number of throws and any corresponding loop counter.  Fortran programmers should use an 8-byte integer.  (See the C++ and Fortran random-walk examples.)  Python automatically handles large integers.  

 Note that the formula is the same if one considers only the upper right quadrant of the figure.

 {{< figure src="/courses/parallel-computing-introduction/img/MCPi.png" caption="Monte Carlo computation of pi." >}}

 Fortran programmers: you may wish to extract the random module from mpirandomwalk.f90 into a separate file random.f90, since it is used frequently.  Remember that modules must be compiled before any program units that USE them.

Try to write the serial code before peeking at the examples.

#### Serial Codes
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/montecarlo_pi.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/montecarlo_pi.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/montecarlo_pi.py" lang="python" >}}
{{< /spoiler >}}

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpimontecarlo_pi.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpimontecarlo_pi.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpimontecarlo_pi.py" lang="python" >}}
{{< /spoiler >}}

## Project 2

The _trapezoid rule_ is a simple method for numerical quadrature (integration).  Given a function $f(x)$, the integral from $x=a$ to $x=b$ is 
$$ I = \int^b_a f(x)dx $$
We can approximate the area under the curve as a trapezoid with area
$$ I_{trap} = \frac{1}{2}(b-a)(f(b)+f(a) $$
However, a single trapezoid isn't a very good approximation.  We can improve our result by dividing the area under the curve into many trapezoids:
$$ I_{trap} = \sum^n_{i=1} \frac{f(x_{i+1})+f(x_i)}{2} \Delta x_i $$
where
$$ \Delta x_i = x_{i+1}-x_i $$
The larger the number of trapezoids, the more accurate our approximation to the integral will be.  Typically $\Delta x_i$ is a constant usually represented by $h$, but that is not a requirement.

{{< figure src="/courses/parallel-computing-introduction/img/trapezoid_rule.png" caption="Illustration of the trapezoid rule." >}}

It should be clear that the trapezoid rule is very easy to parallelize.  The interval on the independent variable should be split up among the ranks.  Each rank then carries out the summation over its part.  The complete integral is the sume of all ranks' results.

Starting from the serial code in your language of choice, parallelize the trapezoid rule.  Only rank 0 should read the input data; then it should broadcast appropriately to the other processes.   Test for four processes.  Note that the integral of the sine from 0 to $\pi$ has an exact value of 2, making it a good test case.

C++ and Fortran programmers: if you are not familiar with passing subprogram names as dummy variables, refer to our courses for [Fortran](/courses/fortran_introduction/subprogram_args) or [C++](/courses/cpp_introduction/subprogram_args).

#### Serial Codes
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/trap.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/trap.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/trap.py" lang="python" >}}
{{< /spoiler >}}

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpitrap.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpitrap.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpitrap.py" lang="python" >}}
{{< /spoiler >}}

## Project 3

We wish to find the maximum of a 3-d surface described by a relatively complicated function.  We can use a "brute force" method, which evaluates a large number of $x$ and $y$ values randomly distributed over the independent variables.  This method of optimization is easily parallelized and can often be faster than older methods even when those are parallelized.

The surface is defined by the following rules:

$$ \mu_1 = \sqrt{2}\ ,\ \mu_2 = \sqrt{\pi}\ ,\ \sigma_1 = 3.1\ ,\ \sigma_2 = 1.4 $$
$$ z_1 = 0.1 \sin(x) \sin(y) $$
$$ a = \frac{(x - \mu_1)^2}{2\sigma_1^2}\ ,\  b = \frac{(y - \mu_2)^2}{2\sigma_2^2} $$
$$ z_2 = \frac{e^{(a+b)}}{\sigma_1 \sigma_2 \sqrt{2\pi}} $$
$$ z = z_1 + z_2 $$

We consider the ranges
$$ -10\pi \le x \le 10\pi\ ,\ -10\pi \le y \le 10\pi $$

Generate a list of N random values for each of x and y over the above range. For testing you can use N=100000 for compiled languages and N=10000 for Python. Be sure to measure the time. Use appropriate MPI gathers to obtain not just the maximum value of $z$ from each process, but the corresponding $x$ and $y$ values.  Remember that gather collects items in strict rank order. Use one or more built-ins for your language (maxloc for Fortran, np.argmax for Python) or loops (C/C++) to find the $x$, $y$, and $z$ for the maximum.

{{< figure src="/courses/parallel-computing-introduction/img/find_max_function.png" caption="Find the maximum of this jagged function." >}}

#### Serial Codes
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/find_max.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/find_max.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/find_max.py" lang="python" >}}
{{< /spoiler >}}

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpifind_max.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpifind_max.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpifind_max.py" lang="python" >}}
{{< /spoiler >}}

---
title: Exercises
date: 2026-08-21:26:29Z
type: book
weight: 2070
menu:
    parallel_programming:
        parent: Multithreaded Programming
---

### Exercise 1

Write a multiprocessing program that computes the sum of the cubes of the numbers from 1.0 to 1000.0 by increments of 0.1

## Exercise 2

We are going to find the maximum of a 3-d surface by “brute force” evaluation of x, y, z values. This is a method of optimization that has become increasingly popular since it is easily parallelizable.  The surface is defined by the following rules:

$$ \mu_1 = \sqrt{2} $$ 
$$ \mu_2 = \sqrt{\pi} $$
$$ \sigma_1=3.1 $$
$$\sigma_2=1.4 $$
$$ a=(x-\mu_1)^2 \div (2 \sig_1^2) $$
$$ b=(x-\mu_2)^2 \div (2 \sig_2^2) $$
$$ z_1=0.1 \sin(x) \sin(xy) $$
$$ z_2=\exp(-(a+b)) \div (\sig_1 \sig_2 \sqrt(2\pi) $$
$$ z=z_1+z_2 $$

The surface is defined over the ranges
$$ −10\pi \le x \le 10\pi $$
$$ −10\pi \le y \le 10\pi $$

Generate a list of N random values for each of x and y over the above range. For testing you can use N=800000. Be sure to measure the time.

Hints: use Numpy. Look up numpy.random.uniform(arglist) to find out how to generate the values. Another hint: for best performance use the numpy built-in `array_split` to divide an array among the processes.
Print the final value of the maximum you found. If you have time, you can go back and figure out how to return the corresponding x and y values as well.

Add the timing routines to compare the parallel
and serial times.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mp_findmax.py" lang="python" >}}
{{< /spoiler >}}


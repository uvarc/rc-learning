---
title: Cache Memory
date: 2026-07-27T17:26:29Z
type: book 
weight: 1020
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Modern CPUs are able to operate much faster than main (RAM) memory and internal bus transfers. Hardware designers solved this problem by providing processing units access to smaller quantities of faster memory. Modern architectures have multiple layers of memory, starting with _registers_, which can hold only a few values but provide extremely fast access.  The next layer is called _cache_. Cache is the main "working memory" of the core, containing the variables it is currently using or expects to use.  

When a variable is requested by the running program, the system performs a _fetch_ to bring it from main memory into cache. However, it does not single out an individual value; it copies a chunk of memory starting from that variable's address.  This is frequently called a _cache line_.  If the next variable requested is in the chunk, no internal traffic is required,  However, if a variable distant in memory is requested, the system dumps the entire line and reloads a new one.If we then request a variable not in the current line, that is called a _cache miss_.  Reloading cache unnecessarily can slow a program significantly.

For example, if the code is working with a two-dimensional array A and we start at A[0][0], the next element in C (or Python) will be A[0][1] whereas in Fortran it will be A[1][0]. This is why for/do loops should be organized to step through an array in the appropriate array order.  Traversing a multidimensional array in the wrong order can be up to a factor of 50 to 100 slower than working in the right order, due to repeated cache misses.

C++
An array in C++ will be optimized for cache if its elements are stored contiguously in memory. Our "traditional" method for allocating two-d arrays in C++ may not guarantee this, but there are other options if better optimization is required. Regardless, loops should always be written to traverse the array in the correct order.

```c++
//correct order (index left to right)
for (i=0;i<N;i++) {
    for (j=0;j<M,j++) {
        A[i][j]=value;
    }
}
```
Fortran 
Multidimensional arrays in Fortran are always stored contiguously.
```fortran
!correct order (index right to left)
do (j=1,M)
    do (i=1,N)
        A(i,j)=value
    enddo
enddo
!Even better for a constant assignment
A=value
```

Python
For greatest efficiency, NumPy arrays should be used whenever possible, and NumPy operations should be applied to them in lieu of loops.
```python
A=np.zeros((N,M))
A=value
```

Nearly all chip designs now in production have multiple levels of cache, often three.  As time passes without access to a line of cache, the data will migrate from lower to higher levels of cache and take longer to access, eventually being returned to main memory. Typically, Level 1 cache is on-chip, i.e. it is located within the circuitry of each core, whereas Level 1 and Level 3 are shared by multiple or all cores of the CPU.  

{{< figure src="/courses/parallel-computing-introduction/img/cache.png" alt="Rough illustration of layout for three cache levels." caption="Rough schematic of a typical cache layout." >}}

Cache that is only available to the core is often called _private cache_.  The other levels in this example are _shared cache_.

## Cache Coherence

In a typical cache layout, each core has exclusive access to its private cache. If it loads a variable from main memory and changes it, other cores will not be aware of the change. This can create conflicts in general, but is especially critical for shared-memory programs in which multiple cores may access the same shared variable. Modern systems implement a protocol for _cache coherence_ to be certain that cache values for the same variable remain coordinated. Such a protocol introduces some overhead to the system operation, but the advantage of cache outweigh it.

### NUMA

As we have seen, NUMA systems generally have distinct CPUs. In a cache-based system, which as stated all modern systems are, not only Level 1 but also higher-level cache must be coordinated. The requirements for cache coherence in these systems creates additional complexity and is one of the limiting factors in the number of CPUs or cores a NUMA system can support.


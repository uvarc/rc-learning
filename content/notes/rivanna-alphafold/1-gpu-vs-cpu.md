---
title: CPU vs. GPU
date: 2025-05-20-00:23:54Z
type: docs 
weight: 200
menu: 
    rivanna-alphafold:
---

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_2.jpg >}}

- The **CPU (Central Processing Unit)** is the general-purpose processor responsible for handling most computing tasks in a system.
- The **GPU (Graphics Processing Unit)** is now widely used for accelerating scientific and high-performance computing tasks.
- **GPU computing** uses the GPU as a co-processor to accelerate CPU tasks by offloading compute-intensive, time-consuming portions of code onto the GPU.

{{< table >}}
| CPU                                                             | GPU                                                                 |
|------------------------------------------------------------------|----------------------------------------------------------------------|
| 4–8 cores (on laptop/workstation PC)                             | 100s–1000s of cores                                                 |
| Up to 48 cores (on Rivanna)                                      | High throughput                                                     |
| Low latency                                                      | Good for parallel processing                                        |
| Good for serial processing                                       | Breaks jobs into separate tasks to process simultaneously           |
| Quickly process interactive tasks                                | Requires additional software to convert CPU functions to GPU functions for parallel execution |
| Traditionally sequential execution                               | Typically parallel execution                                        |
{{< /table >}}


## Metaphor: GPU vs CPU 

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_3.png >}}

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_4.png >}}

- **CPUs are like scooters**: simple, efficient, and great for quickly handling one task or a few tasks at a time.
- **GPUs are like sports cars**: powerful, fast, and ideal for processing many tasks in parallel.
- When you have **many packages to deliver**, a fleet of scooters (parallel delivery) can outperform one fast car — just like GPUs excel at parallel processing.
- When you need to **deliver one package as fast as possible**, a sports car is the better choice — similar to how CPUs can be better for low-latency or interactive tasks.

**The best option depends on your use case!**

**AlphaFold** is designed for GPU computing, making it an ideal workload for systems like Rivanna that support GPU acceleration.

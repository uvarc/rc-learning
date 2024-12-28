---
title: Parallel Matlab on Rivanna
date: 2024-11-16-20:28:40Z
type: docs 
weight: 100
menu: 
    matlab-parallel-programming:
---

## Why parallel computing?

**Parallel computing** offers a powerful solution to tackle increasingly complex problems while saving valuable time. By utilizing available compute cores and GPUs, parallel computing **reduces computation time** significantly.

### Why parallel computing with MATLAB and Simulink?

- **Accelerated workflows** with minimal to no code changes to your original code  
- **Scalable computations** to clusters and cloud infrastructures  
- **Focus on engineering and research**, not the computation  

Parallel computing is essential because the **size of the problems** we need to solve is increasing, and there’s a growing demand to get products to market faster. This need spans industries and applications, from engineering to research. As hardware capabilities expand, modern computers—including laptops—are increasingly designed with parallel architectures, featuring multiple processors and cores. Additionally, access to GPUs, computer clusters, and cloud computing infrastructure is becoming common.

However, the challenge lies in effectively utilizing this powerful hardware, which is where **parallel computing expertise** becomes essential.

### How do MathWorks Parallel Computing Tools help?

- **Leverage available hardware** without needing to be a parallel computing expert  
- **Accelerate workflows** with minimal changes to existing code  
- **Scale applications** seamlessly from desktop to clusters or cloud for more computational power and memory  

{{< figure src=/notes/matlab-parallel-programming/img/matlab-gpu-multicore-cpu.png >}}


Let's look at some examples of customer across multiple industries who have been using MathWorks parallel computing tools and why they chose parallel computing 

## Benefits of parallel computing

---

| {{< figure src=/notes/matlab-parallel-programming/img/automotive-test-analysis.png width="300px" >}} | {{< figure src=/notes/matlab-parallel-programming/img/discrete-model-fleet-performance.png width="300px" >}} |
|:-----------------------------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------:|
| **Automotive Test Analysis and Visualization**                                                                   | **Discrete-Event Model of Fleet Performance**                                                                    |
| - 3–4 months of development time saved                                                                           | - Simulation time reduced from months to hours                                                                   |
| - Validation time sped up 2X                                                                                     | - Simulation time sped up 20X                                                                                    |

| {{< figure src=/notes/matlab-parallel-programming/img/heart-transplant-studies.png width="300px" >}} | {{< figure src=/notes/matlab-parallel-programming/img/derived-market-data.jpg width="300px" >}} |
|:-----------------------------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------:|
| **Heart Transplant Studies**                                                                                     | **Calculating Derived Market Data**                                                                              |
| - 4 weeks reduced to 5 days                                                                                      | - Implementation time reduced by months                                                                          |
| - Process time sped up 6X                                                                                        | - Updates sped up 8X                                                                                             |

---
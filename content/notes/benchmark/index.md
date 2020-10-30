---
title: "Benchmarking Parallel Programs"
type: article 
toc: true
date: 2020-10-29T00:00:00-05:00

---

# Motivation

## Terms and Implications

Denote the number of cores (or nodes or GPU devices) as N and the walltime as t. The basis of comparison is the serial job where N=1 with a walltime of t1.

**Speedup** is defined as s=t1/t. For example, a job that finishes in half the time has a speedup factor of 2.

**Perfect scaling** is achieved when N=s. If you manage to halve the time (s=2) by doubling the resources (N=2), you achieve perfect scaling.

On Rivanna, the **SU** (service unit) charge rate is defined as
```text
SU = (#core + 2#gpu) x walltime
```

We can define a **relative SU**, i.e. the SU of a parallel job relative to that of its serial reference.
```text
SU     N x t      N
--- = -------- = ---
SU1    1 x t1     s
```
In the case of perfect scaling, N=s and so the relative SU is 1, which means you spend the same amount of SUs for the parallel job as for its serial reference. Since superlinear scaling (s>N) almost never occurs, the implication is that you need to pay an extra price for parallelization. For example, if you double the amount of cores (N=2) but only reduced the walltime by one-third (s=1.5), then the relative SU is equal to N/s=1.33, which means you spend 33% more SUs than a serial job. Whether this is worth it will of course depend on the actual value of s (and your deadline).

# Examples

## PyTorch DistributedDataParallel (DDP)

PyTorch can make use of multiple GPU devices through DDP. This example is based on [our PyTorch 1.7 container](https://hub.docker.com/r/uvarc/pytorch) using the Python script provided by [PyTorch Lightning](https://pypi.org/project/pytorch-lightning) with minor modifications.

### Setup

- Download the container. The following command will create a Singularity container `pytorch_1.7.0.sif`.
```
module load singularity/3.6.1
singularity pull docker://uvarc/pytorch:1.7.0
```

- 

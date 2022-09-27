---
title: "GPU-Enabled Applications on Rivanna"
type: article 
toc: true
date: 2022-10-20T00:00:00-05:00

---

In this workshop participants are introduced to the gpu computing resources on Rivanna.

---

# Introduction to GPU

<https://www.docker.com/resources/what-container>

# GPU Modules on Rivanna

Provided by two toolchains `nvompic` (compiled) and `singularity` (container).

## `nvompic`

Stands for:

- NVIDIA compilers (`nv`)
- OpenMPI (`ompi`)
- CUDA (`c`)

```bash
$ ms nvompic

-----------------------------------------------------------------------------------
  nvompic: nvompic/21.9_3.1.6_11.4.2
-----------------------------------------------------------------------------------
    Description:
      NVHPC Compiler including OpenMPI for MPI support.


    This module can be loaded directly: module load nvompic/21.9_3.1.6_11.4.2
```

The toolchain version consists of three subversions joined by `_`, corresponding to the version of `nvhpc`, `openmpi`, and `cuda`, respectively.

```bash
$ module load nvompic
$ module avail
------------- /apps/modulefiles/standard/mpi/nvhpc/21.9/openmpi/3.1.6 -------------
   berkeleygw/3.0.1    fftw/3.3.10 (D)    quantumespresso/7.0    yambo/5.0.4
   elpa/2021.05.001    hdf5/1.12.1 (D)    scalapack/2.1.0

----------------- /apps/modulefiles/standard/compiler/nvhpc/21.9 ------------------
   hdf5/1.12.1    openblas/0.3.17 (D)    openmpi/3.1.6 (L,D)
```

## `singularity`

The popular deep learning frameworks, TensorFlow and PyTorch, are backed by containers. (To learn more about containers, see [Using Containers on Rivanna](/workshop/using-containers).)

```bash
module load singularity tensorflow
```

On JupyterLab, you may conveniently select the kernel of the desired framework and version.

---

**Congratulations - you have completed this workshop!**

---

# References

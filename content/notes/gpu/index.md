---
title: "GPU-Enabled Applications on Rivanna"
type: article 
toc: true
date: 2022-09-27T00:00:00-05:00

---

In this workshop participants are introduced to the gpu computing resources on Rivanna.

---

# Introduction to GPU


# GPUs on Rivanna

Go to https://www.rc.virginia.edu/userinfo/rivanna/overview/#system-details and click on "Hardware Configuration". GPUs are indicated by "GPU" under the specialty hardware column. 

Command to check the current status of GPU nodes:

```bash
$ qlist -p gpu

STATE    NODE           CPUS(A/I/O/T) TOTALMEM(MB)  ALLOCMEM(MB)  AVAILMEM(MB)  GRES(M:T:A)               JOBS
==============================================================================================================
mix      udc-an28-1     8/120/0/128   1000000       40960         959040        gpu:a100:8(S:0-7):1         1
mix      udc-an28-7     28/100/0/128  1000000       680960        319040        gpu:a100:8(S:0-7):6         6
mix      udc-an33-37    12/24/0/36    384000        384000        0             gpu:v100:4(S:0-1):3         3
...
```

Important things to note:

- CPU memory is not GPU memory
- Each **GPU node** contains multiple **GPU devices**
- Different GPU types have different specs (GPU memory, CPU cores, etc.)
- In descending order of performance: A100, V100, P100, K80

# GPU-Enabled Applications on Rivanna

## Modules
Provided by three toolchains `goolfc`, `nvompic` (compiled languages), and `singularity` (container).

### `goolfc`

Stands for:

- GCC compilers `g`
- OpenMPI (`o`)
- OpenBLAS (`o`)
- ScaLAPACK (`l`)
- FFTW (`f`)
- CUDA (`c`)

```bash
--------------------------------------------------------------------------------
  goolfc: goolfc/9.2.0_3.1.6_11.0.228
--------------------------------------------------------------------------------
    Description:
      GNU Compiler Collection (GCC) based compiler toolchain along with CUDA
      toolkit, including OpenMPI for MPI support with CUDA features enabled,
      OpenBLAS (BLAS and LAPACK support), FFTW and ScaLAPACK with CUDA features
      enabled.


    This module can be loaded directly: module load goolfc/9.2.0_3.1.6_11.0.228
```

The toolchain version consists of three subversions joined by `_`, corresponding to the version of `gcc`, `openmpi`, and `cuda`, respectively.

```bash
$ module load goolfc
$ module avail

------- /apps/modulefiles/standard/mpi/gcc-cuda/9.2.0-11.0.228/openmpi/3.1.6 -------
   fftw/3.3.8     (L,D)    hoomd/2.9.6     python/3.8.8    (D)
   gromacs/2021.2          python/3.7.7    scalapack/2.1.0 (L)

----------- /apps/modulefiles/standard/compiler/gcc-cuda/9.2.0-11.0.228 ------------
   gpunufft/2.1.0    mumax3/3.10    nccl/2.7.8    openmpi/3.1.6 (L,D)
```

### `nvompic`

Stands for:

- NVIDIA compilers (`nv`)
- OpenMPI (`ompi`)
- CUDA (`c`)

```bash
$ module spider nvompic

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

### `singularity`

The popular deep learning frameworks, TensorFlow and PyTorch, are backed by containers. (To learn more about containers, see [Using Containers on Rivanna](/workshop/using-containers).)

```bash
module load singularity tensorflow
```

On JupyterLab, you may conveniently select the kernel of the desired framework and version.

## Slurm job

To request a GPU in a Slurm job, your Slurm script must contain these lines:

```bash
#SBATCH -p gpu
#SBATCH --gres=gpu
```

See [here](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#gpu-computations) for further information.


# Demo: RAPIDS

---

**Congratulations - you have completed this workshop!**

---

# References

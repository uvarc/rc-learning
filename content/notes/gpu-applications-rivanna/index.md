---
title: "GPU-Enabled Applications on Rivanna"
type: article 
toc: true
date: 2022-10-19T00:00:00-05:00

---

In this workshop participants are introduced to the gpu computing resources on Rivanna.

---

## Introduction to GPU

The graphics processing unit was invented specifically for graphics rendering. Nowadays they are also used as accelerators for parallel computing; you may also hear the term "general-purpose GPU" (GPGPU).

{{< table >}}
|Property|CPU|GPU|
|---|---|---|
|Number of cores|$10^{0-1}$ | $10^{3-4}$ |
|Throughput | Low | High |
|Per-core performance | High | Low |
|Workload type| Generic | Specific (e.g. rendering, deep learning)|
|Memory on Rivanna| up to 1.5 TB per node | up to 80 GB per device |
{{< /table >}}

### Integrated vs discrete GPU

Integrated GPUs are mostly for graphics rendering and light gaming. They are integrated on the CPU motherboard to achieve more compact systems.

**Discrete (or dedicated) GPUs are designed for resource-intensive computations.**

### GPU vendors and types

**NVIDIA**, AMD, Intel

- Datacenter: H100, **A100**, **V100**, P100, K80
- Workstation: A6000, Quadro
- Gaming: GeForce RTX 40xx, **30xx**, **20xx**

(**bold** means available on Rivanna)

### Myths

- *GPUs are better than CPUs and will eventually replace them.*  
    CPU and GPU complement each other. GPU will not replace CPU.
- *If I run my CPU code on a GPU, it'll be way faster.*  
    This depends on whether your code can run on a GPU at all. Even so, if the computation is not resource-intensive enough, there will be no acceleration. In fact, your code may even be slower on a GPU.
- *Running a GPU program on two GPU devices will be twice as fast as running it on one.*  
    Again, this depends on whether your program can run on multiple GPU devices and the computation intensity.
- *GPU acceleration only applies to data science and machine/deep learning.*  
    Many scientific codes are making use of GPU acceleration: VASP, QuantumEspresso, GROMACS, ... See [here](https://www.nvidia.com/en-us/gpu-accelerated-applications/) for a list compiled in 2018.

## GPUs on Rivanna

Go to [this page](https://www.rc.virginia.edu/userinfo/rivanna/overview/?showHardware#system-details). GPUs are indicated by "GPU" under the specialty hardware column. 

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

Popular GPU applications on Rivanna at a glance

{{< table >}}
|`nvhpc`|`gcc`/`goolf`|`nvompic`|`singularity`|Jupyter kernels|
|---|---|---|---|---|
|(User code)|`gromacs` |`quantumespresso`|`pytorch`   |PyTorch|
|           |`gpunufft`|`berkeleygw`     |`tensorflow`|TensorFlow|
|           |`mumax3`  |`yambo`          |`rapidsai`  |RAPIDS|
|           |          |                 |`amptorch`  |AMPTorch|
|           |          |                 |`alphafold` ||
|           |          |                 |`deeplabcut`||
|           |          |                 |`isaacgym`  ||
{{< /table >}}


### Modules

The `nvhpc` module (NVIDIA HPC SDK) provides these libraries and tools:
- Compilers (`nvc`, `nvc++`, `nvfortran`)
- CUDA
- Mathematical libraries: cuBLAS, cuRAND, cuFFT, cuSPARSE, cuTENSOR, cuSOLVER
- Communication libraries: NVSHMEM, NCCL
- Tools: CUDA-GDB, Nsight System

In addition, applications are installed under three toolchains `goolfc`, `nvompic` (compiled languages), and `singularity` (container).

#### `goolfc`

Stands for:

- GCC compilers (`g`)
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

##### Usage instructions
- [GROMACS](https://www.rc.virginia.edu/userinfo/rivanna/software/gromacs/)

#### `nvompic`

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

##### Usage Instructions

- [BerkeleyGW](https://www.rc.virginia.edu/userinfo/rivanna/software/berkeleygw/)
- [QuantumEspresso](https://www.rc.virginia.edu/userinfo/rivanna/software/quantumespresso/)

#### `singularity`

The popular deep learning frameworks, TensorFlow and PyTorch, are backed by containers. (To learn more about containers, see [Using Containers on Rivanna](/workshops/using-containers).)

```bash
module load singularity tensorflow
```

On JupyterLab, you may conveniently select the kernel of the desired framework and version.

##### Usage instructions

- [TensorFlow](https://www.rc.virginia.edu/userinfo/rivanna/software/tensorflow/)
- [PyTorch](https://www.rc.virginia.edu/userinfo/rivanna/software/pytorch/)
- [RAPIDS](https://www.rc.virginia.edu/userinfo/rivanna/software/rapidsai/) (also see [workshop](/workshops/rapids))

### Jupyter kernels

- TensorFlow
- PyTorch
- RAPIDS

### Requesting a GPU

#### Open OnDemand

Select the `gpu` partition. If you need a specific GPU type, select from the dropdown menu. `Default` will assign you the first available GPU.

#### Slurm script

Your Slurm script must contain these lines:

```bash
#SBATCH -p gpu
#SBATCH --gres=gpu
```

See [here](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#gpu-computations) for further information.

# Demo (Python & Matlab)

---

**Congratulations - you have completed this tutorial!**

---

## References

- [CPU vs GPU: What's the Difference](https://www.intel.com/content/www/us/en/products/docs/processors/cpu-vs-gpu.html)
- [NVIDIA HPC SDK documentation](https://docs.nvidia.com/hpc-sdk/index.html)
- ["That Was Fast: GPUs Now Accelerate Almost 600 HPC Apps"](https://blogs.nvidia.com/blog/2018/11/16/gpus-now-accelerate-almost-600-hpc-apps/)

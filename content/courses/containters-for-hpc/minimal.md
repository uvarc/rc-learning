---
title: Minimal Containers
toc: true
type: book
weight: 11

---

The industry standard of restricting containers to just the application and its dependencies often results in better security and smaller size. See how the use of multi-stage builds and scratch/distroless base images can reduce the image size by as much as 99% in real applications.

Prerequisites:
- Building Containers [Docker]

---

## Review of Best Practices

In the previous chapter, we worked through an example of `lolcow` and saw how following best practices can reduce the image size drastically.

### 0. Package manager cache busting
```bash
apt-get update && apt-get install ...
```

### 1. Clean up
- `apt`
```bash
rm -rf /var/lib/apt/lists/*
```
- `conda`
```bash
conda clean -ya
```
- `pip` (no separate clean up command)
```bash
pip install --no-cache-dir ...
```
- Must occur in the same `RUN` statement as the installation step

### 2. Only install what's needed
```bash
--no-install-recommends
```

### 3. Base image
- For OS and common tools (e.g. python/conda, GCC) start from [official image](https://docs.docker.com/docker-hub/official_images/)
    - Do not reinvent the wheel
- Know which variant to use (e.g. `devel` vs `runtime`, `slim`)
    - Read their overview

## Exercise: QIIME 2
QIIME 2 is a popular bioinformatics software. Can you suggest any potential improvement to the [Dockerfile](https://github.com/qiime2/vm-playbooks/blob/0fda9dce42802596756986e2f80c38437872c66e/docker/Dockerfile)? 

<details><summary>Answer</summary>

With our [Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/qiime2/2020.8/Dockerfile), we managed to reduce the image size by half.

</details>

---

## Multi-Stage Build

The objective is to minimize image size without loss of functionality. What's needed to build/install an application is not always needed at runtime. By separating the buildtime and runtime stages, we'll see how to achieve up to 99% reduction in image size in real applications.

### "Disk space is cheap so why should I care?"
- Minimize vulnerabilities/attack surface
- Fewer things to maintain
- Reduce overhead

### Buildtime $\neq$ runtime dependency

- Multiple `FROM` statements (each defines a **stage**)
- Build stage:
    - install buildtime dependencies
    - install software
- Production stage:
    - use `base`/`runtime` base image instead of `devel` (`jre` vs `jdk`)
    - only install runtime dependencies (e.g. `cmake`, `go` not needed)
    - copy software from build stage

Reference: [_Use multi-stage builds_](https://docs.docker.com/develop/develop-images/multistage-build/)

### Multi-stage Dockerfile template

```dockerfile
FROM base1 AS build
# install buildtime dependencies
# install software

FROM base2
COPY --from=build /path/to/file/in/base1 /path/to/file/in/base2
# install runtime dependencies

ENV PATH=/path/to/binary:$PATH
ENTRYPOINT ["binary"]
```

- `base1` and `base2` may or may not be the same
- Use `AS <stage>` to name a stage
- Use `COPY --from=<stage>` to copy from a particular stage
- You can have more than 2 stages

## Exercise: LightGBM
LightGBM is a gradient boosting framework that uses tree based learning algorithms. It is an open source project by Microsoft.

1. Examine the [official Dockerfile](https://github.com/microsoft/LightGBM/blob/master/docker/gpu/dockerfile.gpu). Can you identify any problems?

    <details><summary>Answer</summary>

    - `nvidia/cuda:cudnn-devel` as [base image](https://hub.docker.com/r/nvidia/cuda/tags)  (>1 GB)
    - Clean up in separate `RUN` statements

    </details>
    <br>

2. Let's try to build an image of the command line interface (CLI) alone. Copy the Dockerfile. Remove the Tini, Conda, Jupyter sections and everything related to python/conda. Build the image and note the image size.

    <details><summary>Answer</summary>
    
    ```dockerfile
    FROM nvidia/cuda:8.0-cudnn5-devel

    #################################################################################################################
    #           Global
    #################################################################################################################
    # apt-get to skip any interactive post-install configuration steps with DEBIAN_FRONTEND=noninteractive and apt-get install -y

    ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
    ARG DEBIAN_FRONTEND=noninteractive

    #################################################################################################################
    #           Global Path Setting
    #################################################################################################################

    ENV CUDA_HOME /usr/local/cuda
    ENV LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${CUDA_HOME}/lib64
    ENV LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/local/lib

    ENV OPENCL_LIBRARIES /usr/local/cuda/lib64
    ENV OPENCL_INCLUDE_DIR /usr/local/cuda/include

    #################################################################################################################
    #           SYSTEM
    #################################################################################################################
    # update: downloads the package lists from the repositories and "updates" them to get information on the newest versions of packages and their
    # dependencies. It will do this for all repositories and PPAs.

    RUN apt-get update && \
        apt-get install -y --no-install-recommends \
        build-essential \
        curl \
        wget \
        bzip2 \
        ca-certificates \
        libglib2.0-0 \
        libxext6 \
        libsm6 \
        libxrender1 \
        git \
        vim \
        mercurial \
        subversion \
        cmake \
        libboost-dev \
        libboost-system-dev \
        libboost-filesystem-dev \
        gcc \
        g++

    # Add OpenCL ICD files for LightGBM
    RUN mkdir -p /etc/OpenCL/vendors && \
        echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd

    #################################################################################################################
    #           LightGBM
    #################################################################################################################

    RUN cd /usr/local/src && mkdir lightgbm && cd lightgbm && \
        git clone --recursive --branch stable --depth 1 https://github.com/microsoft/LightGBM && \
        cd LightGBM && mkdir build && cd build && \
        cmake -DUSE_GPU=1 -DOpenCL_LIBRARY=/usr/local/cuda/lib64/libOpenCL.so -DOpenCL_INCLUDE_DIR=/usr/local/cuda/include/ .. && \
        make OPENCL_HEADERS=/usr/local/cuda-8.0/targets/x86_64-linux/include LIBOPENCL=/usr/local/cuda-8.0/targets/x86_64-linux/lib

    ENV PATH /usr/local/src/lightgbm/LightGBM:${PATH}

    #################################################################################################################
    #           System CleanUp
    #################################################################################################################
    # apt-get autoremove: used to remove packages that were automatically installed to satisfy dependencies for some package and that are no more needed.
    # apt-get clean: removes the aptitude cache in /var/cache/apt/archives. You'd be amazed how much is in there! the only drawback is that the packages
    # have to be downloaded again if you reinstall them.

    RUN apt-get autoremove -y && apt-get clean && \
        rm -rf /var/lib/apt/lists/*
    ```

    2.24 GB

    </details>
    <br>

3. Rewrite the Dockerfile using a multi-stage build based on [OpenCL](https://hub.docker.com/r/nvidia/cuda/tags).
    - Use `nvidia/opencl:devel` as the build stage
        - Remove everything related to CUDA since it is not relevant
        - Redefine the OpenCL environment variables as:
            ```dockerfile
            ENV OPENCL_LIBRARIES=/usr/lib/x86_64-linux-gnu \
                OPENCL_INCLUDE_DIR=/usr/include/CL
            ```
        - Keep the same dependencies
        - Remember to clean up at the right place
        - Ignore the command under `# Add OpenCL ICD files for LightGBM`
        - Instead of `mkdir foo && cd foo` use `WORKDIR foo` (see [documentation](https://docs.docker.com/engine/reference/builder/#workdir))
        - Use the same command to install LightGBM, but replace the paths with the corresponding OpenCL environment variables
        - Add an `ENTRYPOINT`
    - Use `nvidia/opencl:runtime` as the production stage
        - The runtime dependencies are `libxext6 libsm6 libxrender1 libboost-system-dev libboost-filesystem-dev gcc g++`
        - Remember to copy from the build stage
    - Build the image and compare the image size with step 2.

    <details><summary>Answer</summary>

    ```dockerfile
    FROM nvidia/opencl:devel AS build
    
    ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
        OPENCL_LIBRARIES=/usr/lib/x86_64-linux-gnu
        OPENCL_INCLUDE_DIR=/usr/include/CL
    ARG DEBIAN_FRONTEND=noninteractive

    RUN apt-get update && \
        apt-get install -y --no-install-recommends \
        build-essential \
        wget \
        ca-certificates \
        libglib2.0-0 \
        libxext6 \
        libsm6 \
        libxrender1 \
        git \
        cmake \
        libboost-dev \
        libboost-system-dev \
        libboost-filesystem-dev \
        gcc \
        g++ && \
        rm -rf /var/lib/apt/lists/*

    WORKDIR /usr/local/src/lightgbm
    RUN git clone --recursive --branch stable --depth 1 https://github.com/microsoft/LightGBM && \
        cd LightGBM && mkdir build && cd build && \
        cmake -DUSE_GPU=1 -DOpenCL_LIBRARY=${OPENCL_LIBRARIES}/libOpenCL.so -DOpenCL_INCLUDE_DIR=${OPENCL_INCLUDE_DIR} .. && \
        make OPENCL_HEADERS=${OPENCL_INCLUDE_DIR} LIBOPENCL=${OPENCL_LIBRARIES}

    FROM nvidia/opencl:runtime
    RUN apt-get update && \
        apt-get install -y --no-install-recommends \
        libxext6 libsm6 libxrender1 libboost-system-dev libboost-filesystem-dev gcc g++ && \
        rm -rf /var/lib/apt/lists/*
        
    COPY --from=build /usr/local/src/lightgbm/LightGBM/lightgbm /lightgbm

    ENTRYPOINT ["/lightgbm"]
    ```

    374 MB (84% reduction)

    </details>
    <br>

4. Challenge: Verify that the two containers have the same performance on Rivanna's GPU node. Follow the [tutorial example](https://lightgbm.readthedocs.io/en/latest/GPU-Tutorial.html#dataset-preparation). Run the same job without using GPU. How much faster is it with GPU?

---

## Base Images Without OS

### Do we really need an operating system?

- Not always!
- No `/bin/sh`, `ls`, `cat`, ...
    - **Shell-less containers are supported by Singularity 3.6+**
- No package manager
- In production stage, typically there's no `RUN`; just `COPY`
- Workflow:
    - Target a specific stage to build: `docker build --target=build .`
    - Find shared libraries of binary: `ldd`
    - Copy binary and libraries to production stage
    - Build production

### Example base images 
- [Scratch](https://hub.docker.com/_/scratch)
    - Literally start from scratch!
    - A "no-op" in the Dockerfile, meaning no extra layer in image
    - Build other base images (beyond scope of this workshop)
    - Minimal image with a single application
- [Distroless](https://github.com/GoogleContainerTools/distroless)
    - Derived from Debian
    - Support for C/C++, Go, Rust, Java, Node.js, Python

---

## Exercise: `fortune` from scratch

This exercise illustrates how we can cherrypick files from the package manager that are essential to the application.

1. The Ubuntu base image shall be our basis of comparison. Copy the Dockerfile and build the image.

    ```dockerfile
    FROM ubuntu:16.04

    RUN apt-get update && apt-get install -y --no-install-recommends \
            fortune fortunes-min && \
        rm -rf /var/lib/apt/lists/*

    ENV PATH=/usr/games:${PATH}

    ENTRYPOINT ["fortune"]
    ```

2. Find the dependencies for `fortune`:
    - `docker run --rm -it --entrypoint=bash <img>`
    - `ldd /usr/games/fortune`

    <br>

    For your reference, the content of the packages can be found here:
    - fortune-mod [package list](https://packages.ubuntu.com/xenial/all/fortune-mod/filelist)
    - fortunes-min [package list](https://packages.ubuntu.com/xenial/all/fortunes-min/filelist)

    <br>

3. Having identified the necessary files to copy, add a second stage `FROM scratch` to your Dockerfile. Only `COPY` what's necessary. Build and compare image sizes.

    <details><summary>Answer</summary>

    ```dockerfile
    FROM ubuntu:16.04 AS build

    RUN apt-get update && apt-get install -y --no-install-recommends \
            fortune fortunes-min && \
        rm -rf /var/lib/apt/lists/*

    FROM scratch

    # fortune
    COPY --from=build /usr/games/fortune /usr/games/fortune
    COPY --from=build /usr/lib/x86_64-linux-gnu/librecode.so.0 /usr/lib/x86_64-linux-gnu/librecode.so.0
    COPY --from=build /lib/x86_64-linux-gnu/libc.so.6 /lib/x86_64-linux-gnu/libc.so.6
    COPY --from=build /lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2

    # fortunes-min
    COPY --from=build /usr/share/doc/fortunes-min/ /usr/share/doc/fortunes-min/
    COPY --from=build /usr/share/games/fortunes/ /usr/share/games/fortunes/

    ENV PATH=/usr/games:${PATH}

    ENTRYPOINT ["fortune"]
    ```

    The image size comparison is 130 MB vs 4 MB, a 97% reduction.
    </details>

## Exercise: (Trick) Question

Can you build an image for `lolcow` (equivalent to `fortune|cowsay|lolcat`; see previous workshop for details) from scratch/distroless? 

## Exercise: LightGBM from scratch

Revisit the LightGBM Dockerfile you prepared previously. Enter the image layer of the build stage and run `ldd` to find the libraries needed by LightGBM. In the production stage, use `scratch` as the base image. Do not use any `RUN` statements in the production stage. You must include this line:
```dockerfile
COPY --from=build /etc/OpenCL/vendors/nvidia.icd /etc/OpenCL/vendors/nvidia.icd
```

Build the image and compare image sizes.

<details><summary>Answer</summary>

```dockerfile
FROM nvidia/opencl:devel AS build

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
    OPENCL_LIBRARIES=/usr/lib/x86_64-linux-gnu
    OPENCL_INCLUDE_DIR=/usr/include/CL
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    wget \
    ca-certificates \
    libglib2.0-0 \
    libxext6 \
    libsm6 \
    libxrender1 \
    git \
    cmake \
    libboost-dev \
    libboost-system-dev \
    libboost-filesystem-dev \
    gcc \
    g++ && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /usr/local/src/lightgbm
RUN git clone --recursive --branch stable --depth 1 https://github.com/microsoft/LightGBM && \
    cd LightGBM && mkdir build && cd build && \
    cmake -DUSE_GPU=1 -DOpenCL_LIBRARY=${OPENCL_LIBRARIES}/libOpenCL.so -DOpenCL_INCLUDE_DIR=${OPENCL_INCLUDE_DIR} .. && \
    make OPENCL_HEADERS=${OPENCL_INCLUDE_DIR} LIBOPENCL=${OPENCL_LIBRARIES}

FROM scratch

COPY --from=build /usr/local/src/lightgbm/LightGBM/lightgbm /lightgbm
COPY --from=build \
    /lib/x86_64-linux-gnu/libc.so.6 \
    /lib/x86_64-linux-gnu/libdl.so.2 \
    /lib/x86_64-linux-gnu/libgcc_s.so.1 \
    /lib/x86_64-linux-gnu/libm.so.6 \
    /lib/x86_64-linux-gnu/libpthread.so.0 \
    /lib/x86_64-linux-gnu/

COPY --from=build \
    /usr/lib/x86_64-linux-gnu/libOpenCL.so.1 \
    /usr/lib/x86_64-linux-gnu/libboost_filesystem.so.1.65.1 \
    /usr/lib/x86_64-linux-gnu/libboost_system.so.1.65.1 \
    /usr/lib/x86_64-linux-gnu/libgomp.so.1 \
    /usr/lib/x86_64-linux-gnu/libstdc++.so.6 \
    /usr/lib/x86_64-linux-gnu/

COPY --from=build /lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2

COPY --from=build /etc/OpenCL/vendors/nvidia.icd /etc/OpenCL/vendors/nvidia.icd

ENTRYPOINT ["/lightgbm"]
```

The image size is merely **10.7 MB**, 99.5% smaller than what we started with. There is no loss in functionality or performance.

</details>

We submitted a [pull request](https://github.com/microsoft/LightGBM/pull/3408) that has been [merged](https://github.com/microsoft/LightGBM/tree/master/docker/gpu).

---

## Example: TensorFlow distroless

TensorFlow is a popular platform for machine learning. It is an open source project by Google.

The TF 2.3 container that you used in the previous workshop is actually based on distroless, which is why you were not able to run `ls` inside the container.

- [Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/tensorflow/2.3.0/Dockerfile.distroless)
- 18% image size reduction
- [PR](https://github.com/tensorflow/build/pull/13) approved and [merged](https://github.com/tensorflow/build)

---

## Dynamic vs Static Linking

The above procedure, while impressive, may be tedious for the average user. All the examples so far are based on [dynamic linking](https://en.wikipedia.org/wiki/Dynamic_linker), where the shared libraries of an executable are stored separately. If you are compiling code from source, you may choose to build a static binary (e.g. `-static` in GCC) so that all the necessary libraries are built into the binary.

## Exercise: Linking against OpenBLAS

OpenBLAS is a linear algebra library. The code in this exercise is taken from its [user manual](https://github.com/xianyi/OpenBLAS/wiki/User-Manual#code-examples). It is based on `dgemm` which performs the matrix operation:

$$ \alpha A B + \beta C, $$

where $A_{mk}, B_{kn}, C_{mn}$ are matrices and $\alpha, \beta$ are constants. For details please visit the [Intel tutorial page](https://software.intel.com/content/www/us/en/develop/documentation/mkl-tutorial-c/top/multiplying-matrices-using-dgemm.html).

1. Select an appropriate base image. (Hint: You will be compiling C++ code.)

1. Install `libopenblas-dev` via the package manager.

1. Copy the code to the same directory as your Dockerfile.

    ```
    #include <cblas.h>
    #include <stdio.h>

    void main()
    {
        int i=0;
        double A[6] = {1.0,2.0,1.0,-3.0,4.0,-1.0};         
        double B[6] = {1.0,2.0,1.0,-3.0,4.0,-1.0};  
        double C[9] = {.5,.5,.5,.5,.5,.5,.5,.5,.5}; 
        cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans,3,3,2,1,A, 3, B, 3,2,C,3);c

        for(i=0; i<9; i++)
            printf("%lf ", C[i]);
        printf("\n");
    }
    ```

    To copy it into the Docker image, add these lines:
    ```dockerfile
    WORKDIR /opt
    COPY cblas_dgemm.c ./
    ```

1. Compile the code with this command:
    ```
    gcc -o cblas_dgemm cblas_dgemm.c -lopenblas -lpthread
    ```

1. Build the image and note the image size. You should get this output:
    ```
    11.000000 -9.000000 5.000000 -9.000000 21.000000 -1.000000 5.000000 -1.000000 3.000000
    ```

    (Optional) Read the Intel tutorial to figure out what the matrices $A, B, C$ are. Do the math and verify that you get the same result.

1. Find the necessary libraries and add a second stage from scratch. Compare the image size between the two stages.

    <details><summary>Answer</summary>

    ```dockerfile
    FROM gcc:10.2 AS build
    RUN apt-get update && apt-get install -y --no-install-recommends libopenblas-dev && \
        rm -rf /var/lib/apt/lists/*

    WORKDIR /opt
    COPY cblas_dgemm.c ./
    RUN gcc -o cblas_dgemm cblas_dgemm.c -lopenblas -lpthread

    FROM scratch
    COPY --from=build /opt/cblas_dgemm /cblas_dgemm

    COPY --from=build \
        /lib/x86_64-linux-gnu/libc.so.6 \
        /lib/x86_64-linux-gnu/libm.so.6 \
        /lib/x86_64-linux-gnu/libpthread.so.0 \
        /lib/x86_64-linux-gnu/

    COPY --from=build /usr/lib/x86_64-linux-gnu/libopenblas.so.0 /usr/lib/x86_64-linux-gnu/libopenblas.so.0

    COPY --from=build \
        /usr/local/lib64/libgcc_s.so.1 \
        /usr/local/lib64/libquadmath.so.0 \
        /usr/local/lib64/libgfortran.so.5 \
        /usr/local/lib64/

    COPY --from=build /lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2

    ENV LD_LIBRARY_PATH=/usr/local/lib64:$LD_LIBRARY_PATH

    ENTRYPOINT ["/cblas_dgemm"]
    ```

    1.29 GB vs 42.9 MB (97% reduction).

    </details>
    <br>

1. Re-compile the code with static linking by adding a `-static` flag. In the production stage simply copy the binary. Compare image sizes.

    <details><summary>Answer</summary>

    ```dockerfile
    FROM gcc:10.2 AS build
    RUN apt-get update && apt-get install -y --no-install-recommends libopenblas-dev && \
        rm -rf /var/lib/apt/lists/*

    WORKDIR /opt
    COPY cblas_dgemm.c ./
    RUN gcc -o cblas_dgemm cblas_dgemm.c -lopenblas -lpthread -static

    FROM scratch
    COPY --from=build /opt/cblas_dgemm /cblas_dgemm
    ENTRYPOINT [ "/cblas_dgemm" ]
    ```

    26.6 MB (98% reduction).

    </details>

This exercise illustrates that it is easier to build a minimal container of a static binary.

## Exercise: Linking against LibTorch
LibTorch is the C++ frontend of PyTorch. This exericse is based on the ["Writing a Basic Application"](https://pytorch.org/tutorials/advanced/cpp_frontend.html#writing-a-basic-application) section of the PyTorch tutorial.

1. Select an appropriate base image. (Hint: You will be compiling C++ code.)

1. You will need these additional packages:
    - Build tools: `build-essential cmake`
    - Download and decompress: `wget ca-certificates unzip`

1. Find the download link for LibTorch under the "Install PyTorch" section at https://pytorch.org/. Select "None" for CUDA. Download the file to `/opt` in the image. Hints:
    - In your `wget` command, you may want to rename the output file using `-O libtorch.zip`.
    - Remember to decompress.

1. Copy these two files to the same directory as your Dockerfile.
    - `dcgan.cpp`

    ```
    #include <torch/torch.h>
    #include <iostream>

    int main() {
        torch::Tensor tensor = torch::eye(3);
        std::cout << tensor << std::endl;
    }
    ```

    - `CMakeLists.txt`

    ```
    cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
    project(dcgan)

    list(APPEND CMAKE_PREFIX_PATH "/opt/libtorch/share/cmake/Torch")
    find_package(Torch REQUIRED)

    add_executable(dcgan dcgan.cpp)
    target_link_libraries(dcgan "${TORCH_LIBRARIES}")
    set_property(TARGET dcgan PROPERTY CXX_STANDARD 14)
    ```

    To copy them into the Docker image, add these lines:
    ```dockerfile
    WORKDIR /opt/dcgan
    COPY dcgan.cpp CMakeLists.txt ./
    ```

1. Build the code. The typical procedure is:
    ```
    mkdir build
    cd build
    cmake ..
    make
    ```

1. Find the necessary libraries and add a second stage from scratch. Compare the image size between the two stages.

    <details><summary>Answer</summary>

    ```dockerfile
    FROM gcc:10.2 AS build

    RUN apt-get update && apt-get install -y --no-install-recommends \
            build-essential cmake \
            wget ca-certificates unzip && \
        rm -rf /var/lib/apt/lists/*

    WORKDIR /opt

    RUN wget -q https://download.pytorch.org/libtorch/cpu/libtorch-cxx11-abi-shared-with-deps-1.7.1%2Bcpu.zip -O libtorch.zip && \
        unzip libtorch.zip && rm libtorch.zip

    WORKDIR /opt/dcgan
    COPY dcgan.cpp CMakeLists.txt ./

    RUN mkdir build && cd build && cmake .. && make

    FROM scratch
    COPY --from=build /opt/dcgan/build/dcgan /dcgan

    COPY --from=build \
        /lib/x86_64-linux-gnu/libc.so.6 \
        /lib/x86_64-linux-gnu/libdl.so.2 \
        /lib/x86_64-linux-gnu/libm.so.6 \
        /lib/x86_64-linux-gnu/libpthread.so.0 \
        /lib/x86_64-linux-gnu/librt.so.1 \
        /lib/x86_64-linux-gnu/

    COPY --from=build \
        /opt/libtorch/lib/libc10.so \
        /opt/libtorch/lib/libgomp-75eea7e8.so.1 \
        /opt/libtorch/lib/libtorch.so \
        /opt/libtorch/lib/libtorch_cpu.so \
        /opt/libtorch/lib/

    COPY --from=build \
        /usr/local/lib64/libgcc_s.so.1 \
        /usr/local/lib64/libstdc++.so.6 \
        /usr/local/lib64/

    COPY --from=build /lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2

    ENV LD_LIBRARY_PATH=/usr/local/lib64:$LD_LIBRARY_PATH
    ENTRYPOINT ["/dcgan"]
    ```

    1.98 GB for build stage vs 314 MB for production stage (85% reduction).

    </details>
    <br>

1. Challenge: The above image cannot make use of GPU. Build an image for GPU. Hints:
    - You do not need a physical GPU to build an image for GPU.
    - Pick a `nvidia/cuda` base image. Read their overview page on Docker Hub to decide which flavor to use.
    - Choose a CUDA version to get the download link for LibTorch on the PyTorch webpage.

    <br>

1. Challenge: Can you build `dcgan` on Rivanna without using a container? Why (not)?

1. Challenge: Can you build a static binary of `dcgan`? Why (not)?

---

## Summary

{{< table >}}
| App | Single-stage image size | Final image size | Reduction (%) |
|---|---:|---:|---:|
| `fortune` | 130 MB | 4 MB | 97.0 |
| LightGBM | 2.24 GB | 10.7 MB | 99.5 |
| `dgemm` | 1.29 GB | 26.6 MB | 98.0 |
| `dcgan` | 1.98 GB | 314 MB | 85.5 |
{{< /table >}}

---

## References

- [UVA Rivanna-Docker GitHub](https://github.com/uvarc/rivanna-docker)
    - Dockerfiles by UVA Research Computing
    - [Tips](https://github.com/uvarc/rivanna-docker/wiki/Tips)
- [_Best practices for writing Dockerfiles_](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [_Use multi-stage builds_](https://docs.docker.com/develop/develop-images/multistage-build/)
- [Google Distroless GitHub](https://github.com/GoogleContainerTools/distroless)
- [Matthew Moore, _Distroless Docker: Containerizing Apps, not VMs_, swampUP (2017)](https://youtu.be/lviLZFciDv4)

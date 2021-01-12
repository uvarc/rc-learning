---
title: "Minimal Containers"
type: article 
toc: true
date: 2021-02-24T00:00:00-05:00

---

The industry standard of restricting containers to just the application and its dependencies often results in better security and smaller size. See how the use of multi-stage builds and scratch/distroless base images can reduce the image size by as much as 99% in real applications. This is a continuation of the Building Containers for Rivanna workshop.

# Prerequisites
- [Building Containers for Rivanna](/workshops/building-containers)
- Install Docker on your computer

---

# Review of Best Practices

In the previous workshop, we worked through an example of `lolcow` and saw how following best practices can reduce the image size drastically.

## 0. Package manager cache busting
```bash
apt-get update && apt-get install ...
```

## 1. Clean up
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

## 2. Only install what's needed
```bash
--no-install-recommends
```

## 3. Base image
- For OS and common tools (e.g. python/conda, GCC) start from [official image](https://docs.docker.com/docker-hub/official_images/)
    - Do not reinvent the wheel
- Know which variant to use (e.g. `devel` vs `runtime`, `slim`)
    - Read their overview

## Exercise: QIIME 2
QIIME 2 is a popular bioinformatics software. Can you suggest any potential improvement to the [Dockerfile](https://github.com/qiime2/vm-playbooks/blob/0fda9dce42802596756986e2f80c38437872c66e/docker/Dockerfile)? (Result: We managed to reduce the image size by 50%.)

---

# Multi-Stage Build

The objective is to minimize image size without loss of functionality. What's needed to build/install an application is not always needed at runtime. By separating the buildtime and runtime stages, we'll see how to achieve up to 99% reduction in image size in real applications.

## "Disk space is cheap so why should I care?"
- Minimize vulnerabilities/attack surface
- Fewer things to maintain
- Reduce overhead

## Buildtime $\neq$ runtime dependency

- Multiple `FROM` statements (each defines a **stage**)
- Build stage:
    - install buildtime dependencies
    - install software
- Production stage:
    - use `base`/`runtime` base image instead of `devel` (`jre` vs `jdk`)
    - only install runtime dependencies (e.g. `cmake`, `go` not needed)
    - copy software from build stage

Reference: [_Use multi-stage builds_](https://docs.docker.com/develop/develop-images/multistage-build/)

## Multi-stage Dockerfile template

```dockerfile
FROM base1 AS build
# install buildtime dependencies
# install software

FROM base2
COPY --from=build /path/to/file/in/base1 /path/to/file/in/base2
# install runtime dependencies

ENV PATH /path/to/binary:$PATH
ENTRYPOINT ["binary"]
```

- `base1` and `base2` may or may not be the same
- Use `AS <stage>` to name a stage
- Use `COPY --from=<stage>` to copy from a particular stage
- You can have more than 2 stages

## Exercise: LightGBM
LightGBM is a gradient boosting framework that uses tree based learning algorithms. It is an open source project by Microsoft.

1. Examine the [official Dockerfile](https://github.com/microsoft/LightGBM/blob/master/docker/gpu/dockerfile.gpu). Ignore the "Conda" and "Jupyter" sections. Can you identify any problems?

    <details><summary>Answer</summary>

    - `cuda:cudnn-devel` as [base image](https://hub.docker.com/r/nvidia/cuda/tags)  (>1 GB)
    - Clean up in separate `RUN` statements

    </details>

2. Copy and paste the Dockerfile. Remove the "Conda" and "Jupyter" sections. Build the image and note the image size.

3. Rewrite the Dockerfile using a multi-stage build based on [OpenCL](https://hub.docker.com/r/nvidia/cuda/tags).
    - Use `opencl:devel` as the build stage
        - Keep the same dependencies
        - Remember to add an appropriate cleanup command
        - Ignore the command under `# Add OpenCL ICD files for LightGBM`
        - Instead of `mkdir foo && cd foo` use `WORKDIR foo` (see [documentation](https://docs.docker.com/engine/reference/builder/#workdir))
        - Use the same command to install LightGBM
    - Use `opencl:runtime` as the production stage
        - The runtime dependencies are `libxext6 libsm6 libxrender1 libboost-system-dev libboost-filesystem-dev gcc g++`
        - Remember to copy from the build stage
    - Build the image and compare the image size with step 2.

    <details><summary>Answer</summary>

    Our [Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/lightgbm/2.3.1/Dockerfile) results in an image size of 105 MB. It has the same performance using a [tutorial example](https://lightgbm.readthedocs.io/en/latest/GPU-Tutorial.html#dataset-preparation) as the benchmark.

    </details>

---

# Base Images Without OS

## Do we really need an operating system?

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

## Example base images 
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

ENV PATH /usr/games:${PATH}

ENTRYPOINT ["fortune"]
```

2. Find the dependencies for `fortune`:
    - `docker run --rm -it --entrypoint=bash <img>`
    - Find library dependencies: `ldd /usr/games/fortune`

    For your reference, the content of the packages can be found here:
    - fortune-mod [package list](https://packages.ubuntu.com/xenial/all/fortune-mod/filelist)
    - fortunes-min [package list](https://packages.ubuntu.com/xenial/all/fortunes-min/filelist)

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

ENV PATH /usr/games:${PATH}

ENTRYPOINT ["fortune"]
```

The image size comparison is 130 MB vs 4 MB, a 97% reduction.
</details>

## Exercise: (Trick) Question

Can you build an image for `lolcow` (equivalent to `fortune|cowsay|lolcat`; see previous workshop for details) from scratch/distroless? 

## Exercise: LightGBM distroless

Revisit the LightGBM Dockerfile you prepared previously. Use `ldd` to find the libraries needed in the build stage. In the production stage, use `gcr.io/distroless/cc-debian10` as the base image. Do not use any `RUN` statements in the production stage. You must include this line:
```dockerfile
COPY --from=build /etc/OpenCL/vendors/nvidia.icd /etc/OpenCL/vendors/nvidia.icd
```

Build the image and compare image sizes.

<details><summary>Answer</summary>

[More involved Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/lightgbm/2.3.1/Dockerfile.distroless)

The image size is merely **14 MB**, only 1% of what we started with. There is no loss in functionality or performance.

</details>

We submitted a [pull request](https://github.com/microsoft/LightGBM/pull/3408) that has been [merged](https://github.com/microsoft/LightGBM/tree/master/docker/gpu).

---

## Example: TensorFlow distroless

TensorFlow is a popular platform for machine learning. It is an open source project by Google.

The TF 2.3 container that you used in the previous workshop is actually based on distroless, which is why you were not able to run `ls` inside the container.

- [Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/tensorflow/2.3.0/Dockerfile.distroless)
- 18% image size reduction
- [PR](https://github.com/tensorflow/build/pull/13) approved and [merged](https://github.com/tensorflow/build/tree/master/images#distroless-images)

---

# Dynamic vs Static Linking

The above procedure, while impressive, may be tedious for the average user. All the examples so far are based on [dynamic linking](https://en.wikipedia.org/wiki/Dynamic_linker), where the shared libraries of an executable are stored separately. If you are compiling code from source, you may choose to build a static binary (e.g. `-static` in GCC) so that all the necessary libraries are built into the binary.

---

**Congratulations - you have completed this workshop!**

---

# References

- [UVA Rivanna-Docker GitHub](https://github.com/uvarc/rivanna-docker)
    - Dockerfiles by UVA Research Computing
    - [Tips](https://github.com/uvarc/rivanna-docker/wiki/Tips)
- [_Best practices for writing Dockerfiles_](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [_Use multi-stage builds_](https://docs.docker.com/develop/develop-images/multistage-build/)
- [Google Distroless GitHub](https://github.com/GoogleContainerTools/distroless)
- [Matthew Moore, _Distroless Docker: Containerizing Apps, not VMs_, swampUP (2017)](https://youtu.be/lviLZFciDv4)

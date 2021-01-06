---
title: "Building Containers for Rivanna"
type: article 
toc: true
date: 2021-02-17T00:00:00-05:00

---

In this workshop you will learn how to build Docker containers and run them on Rivanna. For a general introduction on software containers, please refer to the "Introduction" section in our [Singularity workshop](/workshops/singularity). 

# Prerequisites

# Multi-Stage Build

_Minimize image size without loss of functionality_  
$\rightarrow$ _Up to 99% reduction in image size_

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
COPY --from=build /path/to/binary /path/to/binary
# install runtime dependencies

ENV PATH /path/to/binary:$PATH
ENTRYPOINT ["binary"]
```

- `base1` and `base2` may or may not be the same
- Use `AS <stage>` to name a stage
- Use `COPY --from=<stage>` to copy from a particular stage
- You can have more than 2 stages

## Case study - LightGBM

- Problems with [official Dockerfile](https://github.com/microsoft/LightGBM/blob/master/docker/gpu/dockerfile.gpu)
    - `cuda:cudnn-devel` as [base image](https://hub.docker.com/r/nvidia/cuda/tags)  (>1 GB)
    - Clean up in separate `RUN` statements
- Our [Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/lightgbm/2.3.1/Dockerfile)
    - `opencl:devel` as build; `opencl:runtime` (42 MB) as production [base image](https://hub.docker.com/r/nvidia/cuda/tags) 
    - Image size 105 MB; 90% reduction
    - [Benchmark](https://lightgbm.readthedocs.io/en/latest/GPU-Tutorial.html#dataset-preparation)
    - Same performance

## Base images without OS

### Do we really need an operating system?

- Not always!
- No `/bin/sh`, `ls`, `cat`, ...
    - **Shell-less containers are supported by Singularity 3.6+**
- No package manager
- In production stage, typically there's no `RUN`; just `COPY`
- Workflow:
    - Target build: `docker build --target=build .`
    - Find shared libraries of binary: `ldd`
    - Copy binary and libraries to production stage
    - Build production

### Example base images 
- [Scratch](https://hub.docker.com/_/scratch)
    - Literally start from scratch!
- [Distroless](https://github.com/GoogleContainerTools/distroless)
    - Derived from Debian
    - Support for C/C++, Go, Rust, Java, Node.js, Python

### Scratch demo: `fortune`

Let's build an image of `fortune` from scratch. The Ubuntu base image shall be our basis of comparison.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y --no-install-recommends \
        fortune fortunes-min && \
    rm -rf /var/lib/apt/lists/*

ENV PATH /usr/games:${PATH}

ENTRYPOINT ["fortune"]
```

Build this image and find the dependencies for `fortune`:
- `docker run --rm -it --entrypoint=bash <img>`
- Find library dependencies: `ldd /usr/games/fortune`
- fortune-mod [package list](https://packages.ubuntu.com/xenial/all/fortune-mod/filelist)
- fortunes-min [package list](https://packages.ubuntu.com/xenial/all/fortunes-min/filelist)

Having identified the necessary files to copy, add a second stage to your Dockerfile.

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

Compare the image size: 130 MB vs 4 MB; 97% reduction.

#### (Trick) Question

Can you build an image for lolcow from scratch/distroless?

### Case study revisited - LightGBM distroless

- [More involved Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/lightgbm/2.3.1/Dockerfile.distroless)
- Image size: **14 MB**
- 1 GB $\rightarrow$ 0.1 GB $\rightarrow$ 0.01 GB; overall **99%** reduction
- Same performance
- [PR](https://github.com/microsoft/LightGBM/pull/3408) approved and [merged](https://github.com/microsoft/LightGBM/tree/master/docker/gpu)

### TensorFlow distroless

- TF 2.3 that you used earlier is actually distroless
- [Dockerfile](https://github.com/uvarc/rivanna-docker/blob/master/tensorflow/2.3.0/Dockerfile.distroless)
- 18% image size reduction
- [PR](https://github.com/tensorflow/build/pull/13) approved and [merged](https://github.com/tensorflow/build/tree/master/images#distroless-images)

---

**Congratulations - you have completed this workshop!**

---

# References

- [UVA Rivanna-Docker GitHub](https://github.com/uvarc/rivanna-docker)
    - Dockerfiles by UVA Research Computing
    - [Tips](https://github.com/uvarc/rivanna-docker/wiki/Tips)
- [_Best practices for writing Dockerfiles_](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [_Use multi-stage builds_](https://docs.docker.com/develop/develop-images/multistage-build/)
- [Natanael Copa, _Small, Simple, and Secure: Alpine Linux under the Microscope_, DockerCon EU (2017)](https://youtu.be/sIG2P9k6EjA)
- [Google Distroless GitHub](https://github.com/GoogleContainerTools/distroless)
- [Matthew Moore, _Distroless Docker: Containerizing Apps, not VMs_, swampUP (2017)](https://youtu.be/lviLZFciDv4)

---
title: "Building Containers for Rivanna"
type: article 
toc: true
date: 2020-10-13T00:00:00-05:00

---

In this workshop you will learn how to build Docker containers and run them on Rivanna. For a general introduction on software containers, please refer to the "Introduction" section in our [Singularity workshop](/workshops/singularity). 

# Prerequisites

1. Command line (Unix)

2. Install Docker

    To follow along on your own computer, please install Docker Desktop and register for a free account on Docker Hub. Both can be found [here](https://www.docker.com/get-started).
    - Windows: https://docs.docker.com/docker-for-windows/install/
    - Mac: https://docs.docker.com/docker-for-mac/install/
    - Linux: Install "Docker Engine" https://docs.docker.com/engine/
      Depending on your distro, you may be able to install it from the package manager.

    After the installation, open a terminal ("cmd" on Windows) and make sure you can execute the command `docker run hello-world` successfully.

3. Set `BUILDKIT_PROGRESS=plain` for plain output (or remember to run `docker build --progress=plain ...`).

---

# Introduction to Containers

<https://www.docker.com/resources/what-container>

# Building Containers

_Docker, Dockerfile, Docker Hub_

## Building _for_ (not _on_) Rivanna

- **Docker**
    - No Docker on Rivanna
    - Docker Hub
    - Can be converted into Singularity
- Singularity
    - Users cannot build on Rivanna (needs `sudo` privilege)
    - Singularity Library/Hub (more limitations)
    - Refer to [workshop](https://learning.rc.virginia.edu/workshops/singularity/) in Spring 2020

## Intro to Dockerfile: lolcow

`fortune | cowsay | lolcat`

- fortune cookie
- talking cow
- rainbow color

Steps:
1. Choose a base image
2. Install software dependencies (if any)
3. Install software

### Step 1: Choose a base image

Use `FROM` to specify the base image. In this example, we'll use Ubuntu 16.04. You do not need to install this on your computer - Docker will pull from Docker Hub when you build it.

```dockerfile
FROM ubuntu:16.04
```

- OS: `ubuntu`, `debian`, `centos`, ...
- Doesn't have to be a bare OS
    - `python`, `continuumio/miniconda3`, `node`, `nvidia/cuda`, etc.

[Dockerfile reference](https://docs.docker.com/engine/reference/builder/#format)

### Steps 2 & 3: Install software

Use `RUN` to specify the actual commands to be executed (as if you were to type them on the command line).

```dockerfile
FROM ubuntu:16.04

RUN apt-get install fortune cowsay lolcat
```

Save this file as `Dockerfile` and run `docker build .` Does it work?

We need to update our package list. Let's modify our Dockerfile and build again.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install fortune cowsay lolcat
```

This time it still failed due to the prompt for confirmation. To pass "yes" automatically, add `-y`.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y fortune cowsay lolcat
```

This finally works. It returns an image ID that we can call to run it:

```bash
docker run --rm -it <img>
```

But it only returns a shell prompt where `fortune`, `cowsay`, `lolcat` don't seem to work. What's wrong?

#### Summary so far

- Build:
    - Update package manager
    - Automatic yes to prompt
- Run:
    - Use `--rm` to remove container after it exits
    - Use `-it` for interactive processes (e.g. shell)
- Problems:
    - User needs to know path to executable
    - User just wants to run "lolcow"

### Use `ENV` to set environment variable

This is equivalent to `export PATH=/usr/games:${PATH}` but it is preserved at runtime. In doing so we can execute `fortune`, `cowsay`, and `lolcat` directly without specifying the full path.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y fortune cowsay lolcat

ENV PATH=/usr/games:${PATH}
```

### Use `ENTRYPOINT` to set default command

```dockerfile
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y fortune cowsay lolcat

ENV PATH=/usr/games:${PATH}

ENTRYPOINT fortune | cowsay | lolcat
```

Finally, we can simply run `docker run --rm -it <img>` to get the desired behavior. You now know how to build a working Docker container.

## 4 Best Practices

While our container is functional, there is a lot of room for improvement. We shall look at some important best practices for writing Dockerfiles.

### 0. Package manager cache busting

The idea of "cache busting" is to force `update` whenever a change is made to `install`. This ensures we get the latest packages (especially critical security updates) should we make changes and rebuild the image in the future.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
        fortune cowsay lolcat

ENV PATH=/usr/games:${PATH}

ENTRYPOINT fortune | cowsay | lolcat
```

- Save this as `Dockerfile0`, which will be the basis for comparison
- For consistency we shall use the same tag as the number

`docker build -t <user>/lolcow:0 -f Dockerfile0 .`

### 1. Clean up

Almost all package managers leave behind some cache files after installation that can be safely removed. Dependending on your application, they can easily accumulate up to several GBs. Let's see what happens if we try to clean up the cache in a separate `RUN` statement.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
        fortune cowsay lolcat
RUN rm -rf /var/lib/apt/lists/*  # clean up command for apt

ENV PATH=/usr/games:${PATH}

ENTRYPOINT fortune | cowsay | lolcat
```

```bash
docker build -t <user>/lolcow:0.5 -f Dockerfile0.5 .
docker images | grep lolcow
```

You should see that there is no difference in the image size. Why?

- Each statement creates an image layer.
- If you try to remove a file from a previous layer, Docker will make a "whiteout" so that you can't see it, but the file is still there.
- The file can be retrieved.
- This is not just a size issue but also a security pitfall.

**Very important!** You must remove files in the same `RUN` statement as they are added.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
        fortune cowsay lolcat && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=/usr/games:${PATH}

ENTRYPOINT fortune | cowsay | lolcat
```

```bash
docker build -t <user>/lolcow:1 -f Dockerfile1 .
docker images | grep lolcow
```

Now you should see that the clean-up is effective.

### 2. Only install what's needed

The `apt` package manager often recommends related packages that are not really necessary. To disable recommendation, use `--no-install-recommends`.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y --no-install-recommends \
        fortune fortunes-min cowsay lolcat && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=/usr/games:${PATH}

ENTRYPOINT fortune | cowsay | lolcat
```

- You may need to specify extra packages
    - `fortune` itself provides the executable without the message database
    - `fortunes-min` contains the message database
- See [how Ubuntu reduced image size by 60%](https://ubuntu.com/blog/we-reduced-our-docker-images-by-60-with-no-install-recommends)

### 3. Use a smaller base image

For installation of common packages, you may consider Alpine.

- BusyBox + package manager + musl libc (beware of compatibility issues)
- [Presentation](https://youtu.be/sIG2P9k6EjA) on Alpine Linux from DockerCon EU 17 

Look for `slim` variants (e.g. `debian:buster-slim`) of a base image, if any.

```dockerfile
FROM alpine:3.12

RUN echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    apk add --no-cache fortune cowsay@testing lolcat@testing

ENTRYPOINT fortune | cowsay | lolcat
```

Note: An `ENV` statement is not needed here because the executables are installed under `/usr/bin`.

### Image size comparison

```bash
$ docker images | grep lolcow | sort -nk 2 | awk '{print $1, $2, $NF}'
<user>/lolcow    0      242MB
<user>/lolcow    0.5    242MB
<user>/lolcow    1      211MB
<user>/lolcow    2      193MB
<user>/lolcow    3       43MB
```

<style scoped>table { font-size: 65%; }</style>

| Version | Description | Reduction (MB) | % |
|---|---|---:|---:|
|0  |(Basis of comparison) | - | - |
|0.5|Clean up in separate `RUN`  | 0 | 0 |
|1  |Clean up in same `RUN`      |31 | 13 |
|-  |Install only what's needed  |18 | 7 |
|2  |Combination of previous two |49 | 20 |
|3  |Alpine base image           |199| 82 |

Reference: [_Best practices for writing Dockerfiles_](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)

### Summary

1. Choose a base image (`FROM`)
2. Install software dependencies (`RUN`)
3. Install software (`RUN`)
4. Clean up (in same `RUN` statement as installation)
5. Define environment variables (`ENV`)
6. Define default command (`ENTRYPOINT`)

## Push to Docker Hub

You can push your image to Docker Hub easily. First, let's set our lolcow version 3 as the latest.

```bash
docker tag <user>/lolcow:3 <user>/lolcow:latest
```

Then sign in to Docker Hub and push as follows:

```bash
docker login
docker push <user>/lolcow:latest
```

### Docker Hub interface

In your browser, go to `https://hub.docker.com/r/<user>/lolcow`.

- Overview: 
    - Sync with GitHub to update `README.md`; or
    - Use [docker-pushrm](https://github.com/christian-korneck/docker-pushrm)
- Tags:
    - List all versions
    - View image history if Dockerfile not provided
    - Compressed size is much smaller than size on disk

---

# Using Containers on Rivanna

_Singularity on HPC_

Do the following:

- Connect to Rivanna
    - SSH client or FastX Web
- Run `hdquota`
    - Make sure you have a few GBs of free space
- Run `allocations`
    - Check if you have `rivanna-training`

## Singularity is designed for HPC

- Does not require sudo privilege to run (unlike Docker)
- Interoperates well with HPC resource managers in multi-node environments
- Easily makes use of GPUs, high speed networks, parallel filesystems
- Able to convert Docker images into Singularity

---

## Pull

To download a container hosted on a registry, use the `pull` command. Docker images are automatically converted into Singularity format.

`singularity pull [<SIF>] <URI>`

- `<URI>` (Unified resource identifiers)
    - `[library|docker|shub]://[<user>/]<repo>[:<tag>] `
    - Default prefix: `library` ([Singularity Library](https://cloud.sylabs.io/library))
    - `user`: optional; may be empty (e.g. `singularity pull ubuntu`)
    - `tag`: optional; default: `latest`
- `<SIF>` (Singularity image format)
    - Optional
    - Rename image; default: `<repo>_<tag>.sif`

### Pull your lolcow from Docker Hub

```bash
module spider singularity
module load singularity
singularity pull docker://<user>/lolcow
```

## Inspect

Inspect an image before running it via `inspect`.

`singularity inspect <SIF>`

```bash
$ singularity inspect lolcow_latest.sif 
WARNING: No SIF metadata partition, searching in container...
org.label-schema.build-date: Wednesday_14_October_2020_14:30:00_EDT
org.label-schema.schema-version: 1.0
org.label-schema.usage.singularity.deffile.bootstrap: docker
org.label-schema.usage.singularity.deffile.from: <user>/lolcow
org.label-schema.usage.singularity.version: 3.6.1
```

### Inspect runscript

The Docker entrypoint is preserved as Singularity runscript.

`singularity inspect --runscript <SIF>`

```bash
$ singularity inspect --runscript lolcow_latest.sif 
#!/bin/sh
OCI_ENTRYPOINT='"/bin/sh" "-c" "fortune | cowsay | lolcat"'
...
```

## Run

There are 3 ways to run a container: `run`, `shell`, `exec`.

### `run`

Execute the default command in `inspect --runscript`.

CPU: `singularity run <SIF>` = `./<SIF>`

GPU: `singularity run --nv <SIF>` (later)

```bash
./lolcow_latest.sif
```

Treat container like an executable:

```bash
singularity pull lolcow docker://<user>/lolcow
./lolcow
```

### `shell`

Start a Singularity container interactively in its shell.

`singularity shell <SIF>`

```bash
$ singularity shell lolcow_latest.sif
Singularity>
```

The change in prompt indicates you are now inside the container.

To exit the container shell, type `exit`.

### `exec`

Execute custom commands without shelling into the container.

`singularity exec <SIF> <command>`

```bash
$ singularity exec lolcow_latest.sif which fortune
```

## Bind mount

- Singularity bind mounts these host directories at runtime:
    - Personal directories: `/home`, `/scratch`
    - Leased storage shared by your research group: `/project`, `/nv`
    - Some system directories: `/tmp`, `/sys`, `/proc`, `/dev`, `/usr`
    - Your current working directory
- Other directories inside the container are owned by root
- To bind mount additional host directories/files, use `--bind`/`-B`:

```bash
singularity run|shell|exec -B <host_path>[:<container_path>] <SIF>
```

---

## Exercises

1. For each of the 3 executables `fortune`, `cowsay`, `lolcat`, run `which` both inside and outside the container.
1. a) Run `ls -l` for your home directory both inside and outside the container. Verify that you get the same result. b) To disable all bind mounting, use `run|shell|exec -c`. Verify that `$HOME` is now empty.
1. View the content of `/etc/os-release` both inside and outside the container. Are they the same or different? Why?
1. (Advanced) Let's see if we can run the host `gcc` inside the lolcow container. First load the module: `module load gcc`
    - Verify that the path to `gcc` (hint: `which`) is equal to `$EBROOTGCC/bin`.
    - Verify that `$EBROOTGCC/bin` is in your `PATH`.
    - Now shell into the container (hint: `-B /apps`) and examine the environment variables `$EBROOTGCC` and `$PATH`. Are they the same as those on the host? Why (not)?
    - In the container, add `$EBROOTGCC/bin` to `PATH` (hint: `export`). Is it detectable by `which`? Can you launch `gcc`? Why (not)?

---

## TensorFlow on GPU through Slurm

- Computationally intensive tasks must be performed on compute nodes
- Job submission to Slurm

Copy these files:

```bash
cp /share/resources/tutorials/singularity_ws/tensorflow-2.3.0.slurm .
cp /share/resources/tutorials/singularity_ws/mnist_example.{ipynb,py} .


Examine Slurm script:

```bash
#!/bin/bash
#SBATCH -A rivanna-training           # account name
#SBATCH -p gpu                        # partition/queue
#SBATCH --gres=gpu:1                  # request 1 gpu
#SBATCH -c 1                          # request 1 cpu core
#SBATCH -t 00:05:00                   # time limit: 5 min
#SBATCH -J tftest                     # job name
#SBATCH -o tftest-%A.out              # output file
#SBATCH -e tftest-%A.err              # error file

module purge                          # start with clean environment
module load singularity

singularity run --nv \
/share/resources/tutorials/singularity_ws/tensorflow-2.3.0.sif mnist_example.py
```

Submit job:

```bash
sbatch tensorflow-2.3.0.slurm
```

### What does `--nv` do?

See [Singularity GPU user guide](https://sylabs.io/guides/3.7/user-guide/gpu.html#nvidia-gpus-cuda).

```bash
$ singularity shell tensorflow-2.3.0.sif
Singularity> python
>>> import os
>>> os.listdir('/.singularity.d/libs')
[]
```

Why can't I `ls`? See "Minimal Containers" workshop.

---

# Custom Jupyter Kernel

_You can install your own kernel_

## "Can I use that TF 2.3 container on JupyterLab?"

First, note we do not have `tensorflow/2.3.0` as a module:

```bash
module spider tensorflow
```

## Installation

### Manual
1. Create kernel directory

```bash
DIR=~/.local/share/jupyter/kernels/tensorflow-2.3.0
mkdir -p $DIR
cd $DIR
```

2. Write `kernel.json`

```
{
 "argv": [
  "/home/<user>/.local/share/jupyter/kernels/tensorflow-2.3.0/init.sh",
  "-f",
  "{connection_file}"
 ],
 "display_name": "tf2.3",
 "language": "python"
}
```

3. Write `init.sh`

```bash
#!/bin/bash
module load singularity
singularity exec --nv /path/to/sif python -m ipykernel $@
```

4. Change `init.sh` into an executable
```bash
chmod +x init.sh
```

**Easy to automate!**

### JKRollout

This tool is currently limited to Python. The container must have the `ipykernel` Python package.

```text
Usage: jkrollout sif display_name [gpu]
    sif          = file name of *.sif
    display_name = name of Jupyter kernel
    gpu          = enable gpu (default: false)
```

```bash
jkrollout /share/resources/tutorials/singularity_ws/tensorflow-2.3.0.sif "tf2.3" gpu
```

## Test your new kernel

- Go to https://rivanna-portal.hpc.virginia.edu
- Select JupyterLab
    - Rivanna Partition: GPU
    - Work Directory: (location of your `mnist_example.ipynb`)
    - Allocation: `rivanna-training`
- Select the new TensorFlow 2.3 kernel
- Run `mnist_example.ipynb`

## Remove a custom kernel

```bash
rm -rf ~/.local/share/jupyter/kernels/tensorflow-2.3.0
```

---

**Congratulations - you have completed this workshop!**

---

# References

- [UVA Rivanna-Docker GitHub](https://github.com/uvarc/rivanna-docker)
    - Dockerfiles by UVA Research Computing
    - [Tips](https://github.com/uvarc/rivanna-docker/wiki/Tips)
- [_Best practices for writing Dockerfiles_](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [Natanael Copa, _Small, Simple, and Secure: Alpine Linux under the Microscope_, DockerCon EU (2017)](https://youtu.be/sIG2P9k6EjA)

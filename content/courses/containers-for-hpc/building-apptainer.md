---
title: "Building Containers on HPC [Apptainer]"
toc: true
type: book
weight: 4

---

## Introduction

### Apptainer vs Singularity

Apptainer is a continuation of the Singularity project. Since our migration to Apptainer on Dec 18, 2023, users can now build containers natively on HPC.

Previous workflow:
- build Docker container on personal computer
- upload (push) to a registry
- download (pull) from registry

### Motivation

Containerization provides an isolated environment, which can be useful in these cases:

- application and/or its dependencies are incompatible with system/module libraries
- preserve environment independently of the host OS and software stack
- bypass software installation request ticket
- customization for self/lab

### Example: lolcow

```bash
$ apptainer run lolcow.sif
 ___________________________________
< Beware of low-flying butterflies. >
 -----------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

What command is actually being executed?
```bash
$ apptainer inspect --runscript lolcow.sif
#!/bin/sh

    fortune | cowsay | lolcat
```

{{< warning >}}
Inspect the runscript before running an image!
{{< /warning >}}

## Setup

1. (Optional) Cache
    
    The default cache directory is `~/.apptainer`. If you are an active container user it can quickly fill up your home. You can define it to your scratch:
    {{< code-snippet >}}export APPTAINER_CACHEDIR=/scratch/$USER/.apptainer{{< /code-snippet >}}
    or remember to clean up periodically.
1. We have suppressed non-error output from the `apptainer` command. To see the complete output, type `\apptainer`.
1. Load the Apptainer module: `module load apptainer`

## Definition File

The definition file is a set of instructions that is used to build an Apptainer container:

- base OS or base container
- files to add from the host system
- software to install
- environment variables to set at runtime
- container metadata

This is a skeleton:

{{< code-snippet >}}Bootstrap: ...   # "Header"
From: ...        #

%files           # "Section"
    ...          #

%post
    ...

%environment
    ...

%runscript
    ...

%labels
    ...

%help
    ...
{{< /code-snippet >}}

### Header

- At the top of the def file
- Sets the base OS or base container

#### `Bootstrap` (mandatory)
This is the very first entry. It defines the bootstrap agent:

- `docker`
- `library`
- `shub`
- and [many more](https://apptainer.org/docs/user/latest/definition_files.html#preferred-bootstrap-agents)

#### `From` (mandatory)
Define the base container.

```
From: [<collection>/]<container>[:<tag>]
```

### Section
Each section starts with `%`. All sections are optional.

#### `%files`

Copy files into the container.

```
%files
    <source1> [<destination1>]
    <source2> [<destination2>]
    ...
```

- always copied before the `%post` section

#### `%post`

Installation commands. Example:

```
%post
    apt-get update && apt-get -y install lolcat
```

#### `%environment`

Define environment variables (set at runtime). Not available at build time. Example:

```
%environment
    export LC_ALL=C
```

#### `%runscript`

List of commands to be executed upon `apptainer run`.

#### `%labels`

Add metadata in the form of key-value pairs. Example:

```
%labels
    Author Ruoshi Sun
```

#### `%help`

Text to be displayed upon `apptainer run-help`.

## Hands-on exercise: lolcow

`fortune | cowsay | lolcat`

- fortune cookie
- talking cow
- rainbow color

Steps:
1. Choose a base image
2. Install software dependencies (if any)
3. Install software

### Step 1: Choose a base image

Use `Bootstrap` and `From` to specify the base image. In this example, we'll use Ubuntu 22.04. You do not need to install this on your computer - Apptainer will pull from Docker Hub when you build it.

```bash
Bootstrap: docker
From: ubuntu:22.04
```

- OS: `ubuntu`, `debian`, `centos`, ...
- Doesn't have to be a bare OS
    - `python`, `continuumio/miniconda3`, `node`, `nvidia/cuda`, etc.

### Steps 2 & 3: Install software

{{< info >}}
The package manager will take care of the dependencies for us.
{{< /info >}}

In `%post` specify the actual commands to be executed (as if you were to type them on the command line).

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get install fortune cowsay lolcat
{{< /code-snippet >}}

Save this file as `lolcow.def` and run `apptainer build lolcow.sif lolcow.def`. Does it work?

We need to update our package list. Let's modify our definition file and build again.

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install fortune cowsay lolcat
{{< /code-snippet >}}

This time it still failed due to the prompt for confirmation. To pass "yes" automatically, add `-y`.

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat
{{< /code-snippet >}}

This finally works.

```
$ apptainer build lolcow.sif lolcow.def
$ apptainer run lolcow.sif
```

But it only returns a shell prompt where `fortune`, `cowsay`, `lolcat` don't seem to work. What's wrong?

#### Summary so far

- Build:
    - Update package manager
    - Automatic yes to prompt
- Problems:
    - User needs to know path to executable
    - User just wants to run "lolcow"

### Use `%environment` to set environment variable

This is equivalent to `export PATH=/usr/games:${PATH}` but it is preserved at runtime. In doing so we can execute `fortune`, `cowsay`, and `lolcat` directly without specifying the full path.

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat

%environment
    export PATH=/usr/games:${PATH}
    export LC_ALL=C
{{< /code-snippet >}}

### Use `%runscript` to set default command

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat

%environment
    export PATH=/usr/games:${PATH}
    export LC_ALL=C

%runscript
    fortune | cowsay | lolcat
{{< /code-snippet >}}

Save this as `lolcow_0.def`, which will be the basis for comparison.

## 2 Best Practices

While our container is functional, there is room for improvement. We shall look at some important best practices.

### 1. Clean up

Almost all package managers leave behind some cache files after installation that can be safely removed. Dependending on your application, they can easily accumulate up to several GBs. Let's see what happens if we try to clean up the cache in a separate `RUN` statement.

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat
    rm -rf /var/lib/apt/lists/*  # clean up command for apt

%environment
    export PATH=/usr/games:${PATH}
    export LC_ALL=C

%runscript
    fortune | cowsay | lolcat
{{< /code-snippet >}}

### 2. Only install what's needed

The `apt` package manager often recommends related packages that are not really necessary. To disable recommendation, use `--no-install-recommends`.

{{< code-snippet >}}Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y --no-install-recommends fortune fortunes-min cowsay lolcat
    rm -rf /var/lib/apt/lists/*  # clean up command for apt

%environment
    export PATH=/usr/games:${PATH}
    export LC_ALL=C

%runscript
    fortune | cowsay | lolcat
{{< /code-snippet >}}

- You may need to specify extra packages
    - `fortune` itself provides the executable without the message database
    - `fortunes-min` contains the message database
- See [how Ubuntu reduced image size by 60%](https://ubuntu.com/blog/we-reduced-our-docker-images-by-60-with-no-install-recommends)

### Image size comparison

```bash
$ ll -h lolcow*.sif
... 86M ... lolcow_0.sif
... 54M ... lolcow_1.sif
... 48M ... lolcow_2.sif
```

<style scoped>table { font-size: 65%; }</style>

| Version | Description | Reduction (MB) | % |
|---|---|---:|---:|
|0  |(Basis of comparison) | - | - |
|1  |Clean up              |32 | 37 |
|-  |Install only what's needed  |6 | 7 |
|2  |Combination of previous two |38 | 44 |

## Sandbox

As we have experienced from the previous section, we may need to iteratively troubleshoot the container build process and, unlike Docker which caches the image layers that finished successfully, Apptainer goes through the whole process every time. So during the exploratory phase, we adopt a more "interactive" approach via sandbox.

- Create and use a writable directory
- Useful for debugging container build process

```bash
$ apptainer build --sandbox <directory> <URI/DEF>
$ apptainer shell -w --fakeroot <directory>
Apptainer> ... (installation commands) ...
```

{{< warning >}}
Technically you can build the sandbox into a container, but this is not recommended. Write down all the commands in the right order into a definition file for reproducibility.
{{< /warning >}}

### Exception: Alpine

Alpine is a Linux distribution with a very slim base image. However, due to a known bug you cannot build it through a definition file in Apptainer.

```bash
/.singularity.d/libs/fakeroot: eval: line 140: /.singularity.d/libs/faked: not found
fakeroot: error while starting the `faked' daemon.
sh: you need to specify whom to kill
FATAL:   While performing build: while running engine: exit status 1
```

Let's try to build a lolcow container in Alpine via a sandbox.

```bash
$ apptainer build --sandbox alpine docker://alpine:3.17
$ apptainer shell -w alpine
Apptainer> echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
Apptainer> apk add fortune cowsay@testing lolcat@testing
Apptainer> rm /var/cache/apk/*
Apptainer> exit
$ apptainer build lolcow_3.sif alpine
$ apptainer exec lolcow_3.sif sh -c "fortune|cowsay|lolcat"
 ________________________________________
/ "All my life I wanted to be someone; I \
| guess I should have been more          |
| specific."                             |
|                                        |
\ -- Jane Wagner                         /
 ----------------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

Note the container size - only 14MB! This is 84% smaller than what we had before.

{{< info >}}
Why can't you run "apptainer exec lolcow_3.sif fortune|cowsay|lolcat"?
{{< /info >}}

## Registry

A container registry is a repository for container images. Here we examine two popular choices.

### Docker Hub

The Apptainer/Singularity SIF is supported by Docker Hub. Register for a free account.

{{< info >}}Replace "myname" with your actual user name in the following commands.{{< /info >}}

Login:
```bash
$ apptainer remote login --username myname docker://docker.io

$ apptainer remote list
...
Authenticated Logins
=================================

URI                 INSECURE
docker://docker.io  NO
```

Push:
```bash
$ apptainer push lolcow_0.sif oras://docker.io/myname/lolcow:0
```

Check:
`https://hub.docker.com/r/myname/lolcow/tags`

{{< info >}}Best practice: Sign your containers; see [here](https://apptainer.org/docs/user/latest/signNverify.html).{{< /info >}}

{{< warning >}}While Apptainer can convert a Docker image into SIF, you cannot run SIF with Docker. You are simply using Docker Hub to host your SIF - it is not converted into Docker.{{< /warning >}}

### GitHub Packages

1. [Create a personal access token](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#authenticating-with-a-personal-access-token-classic)
1. Login
    ```bash
    apptainer remote login --username myname docker://ghcr.io
    [paste your token]
    ```
1. Push
    ```bash
    apptainer push lolcow_0.sif oras://ghcr.io/myname/lolcow
    ```
1. Check `https://github.com/users/myname/packages/container/package/lolcow`

## Case studies

### Python

While you can create a conda environment locally, you cannot directly migrate it onto another machine. How can you ensure an identical environment with a one-time installation?

{{< info >}}Unless you need the entire Anaconda distribution you should opt for a slimer base image (e.g. miniconda, micromamba) to create your own environment.{{< /info >}}

### R

Rocker provides many base images for all R versions (see [here](https://rocker-project.org/images/)):
- `rocker/r-ver`: basic R installation
- `rocker/rstudio`: with RStudio Server
- `rocker/tidyverse`: plus tidyverse and dependencies
- `rocker/shiny`: shiny server

If you want to build a custom R container start with one of the Rocker images. Building R, RStudio Server, etc. from source can be very tedious!

---

## References
- [Apptainer User Guide](https://apptainer.org/docs/user/latest)
- [Definition File](https://apptainer.org/docs/user/latest/definition_files.html)
- [Definition File vs Dockerfile](https://apptainer.org/docs/user/latest/docker_and_oci.html#apptainer-definition-file-vs-dockerfile)
- [GitHub Packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry)

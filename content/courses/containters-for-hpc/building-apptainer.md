---
title: "Building Containers [Apptainer]"
toc: true
type: book
weight: 4

---

## Introduction

### Apptainer vs Singularity

Apptainer is a continuation of the Singularity project. Since our migration to Apptainer on Dec 18, 2023, users can now build containers natively on Rivanna.

Previous workflow:
- build Docker container on personal computer
- upload (push) to a registry
- download (pull) from registry onto Rivanna

### Motivation

Containerization provides an isolated environment, which can be useful in these cases:

- application and/or its dependencies are incompatible with system/module libraries
- preserve environment independently of the host OS and software stack
- bypass software installation request ticket
- customization for self/lab

### Example: lolcow

```
$ apptainer run lolcow_latest.sif
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
```
$ apptainer inspect --runscript lolcow_latest.sif
#!/bin/sh

    fortune | cowsay | lolcat
```

**Inspect the runscript before running an image!**

## Setup

1. (Optional) Cache
    The default cache directory is `~/.apptainer`. If you are an active container user it can quickly fill up your home. You can define it to your scratch:
    ```
    export APPTAINER_CACHEDIR=/scratch/$USER/.apptainer
    ```
    or remember to clean up periodically.
1. We have suppressed non-error output from the `apptainer` command. To see the complete output, type `\apptainer`.

## Definition File

The definition file is a set of instructions that is used to build an Apptainer container:

- base OS or base container
- files to add from the host system
- software to install
- environment variables to set at runtime
- container metadata

This is a skeleton:

```
Bootstrap: ...   # "Header"
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
```

### Header

- At the top of the def file
- Sets the base OS or base container

#### `Bootstrap` (mandatory)
This is the very first entry. It defines the bootstrap agent:

- `library`
- `docker`
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

## Example: lolcow

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

In `%post` specify the actual commands to be executed (as if you were to type them on the command line).

```dockerfile
Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get install fortune cowsay lolcat
```

Save this file as `lolcow.def` and run `apptainer build lolcow.sif lolcow.def`. Does it work?

We need to update our package list. Let's modify our definition file and build again.

```dockerfile
Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install fortune cowsay lolcat
```

This time it still failed due to the prompt for confirmation. To pass "yes" automatically, add `-y`.

```bash
Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat
```

This finally works.

```bash
apptainer run lolcow.sif
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

```bash
Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat

%environment
    export PATH=/usr/games:${PATH}
```

### Use `%runscript` to set default command

```bash
Bootstrap: docker
From: ubuntu:22.04

%post
    apt-get update
    apt-get install -y fortune cowsay lolcat

%environment
    export PATH=/usr/games:${PATH}

%runscript
    fortune | cowsay | lolcat
```

## Other Features

### Sandbox

- Create and use a writable directory
- Useful for debugging definition file

```
apptainer build --sandbox <directory> <URI/DEF>
apptainer shell -w --fakeroot <directory>
```

### Add environment variables in `%post` section

```
%post
    <some step that determines x>  # cannot determine x before build
    echo 'export VARIABLE_NAME=x' >>$APPTAINER_ENVIRONMENT
```

### Remote

- Host your own repository
- Push image to repository
- Remote build

---

## References
- [Apptainer User Guide](https://apptainer.org/docs/user/latest)
- [Definition File](https://apptainer.org/docs/user/latest/definition_files.html)

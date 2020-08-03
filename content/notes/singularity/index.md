---
title: "Singularity Containers for HPC Environments"
type: docs 
toc: true
menu:
    singularity:
       name: Singularity Containers for HPC Environments
       weight: 1
---

In this workshop you will learn how to create and use software containers within an HPC environment (Rivanna). We will discuss Docker (briefly) and Singularity (in more depth).

# Introduction
## What is a software container?

> A container is a standard unit of software that packages up code and all its dependencies so the application runs quickly and reliably from one computing environment to another. -- <cite>Docker</cite>

A container bundles an **application**, the **libraries** and other **executables** that are needed, and even the **data** used with the application, into a *portable*, *self-contained* file called **image**.

<br>

## Why use software containers?
#### **Simple**
Containers simplify software installation and management.
#### **Portable** 
You can build an image on one machine and run it on another.
#### **Reproducible** 
Versioning and freezing of containers enable data reproducibility.

<br>

## Why use Singularity?
It is designed for **HPC environments**:

- Does not require sudo privilege to run (unlike Docker)
- Interoperates well with HPC resource managers in multi-node environments
- Easily makes use of GPUs, high speed networks, parallel filesystems on a cluster or server by default
- Able to convert Docker images into Singularity

<br>

## How do containers work?

![Docker-vs-VM](https://www.docker.com/sites/default/files/d8/2018-11/docker-containerized-and-vm-transparent-bg.png)

(Taken from <cite>["What is a Container?"](https://www.docker.com/resources/what-container)</cite> by Docker)

A container:

- shares the OS kernel of the host
- virtualizes the OS instead of hardware
- has much less overhead and faster deployment than a VM

---

<br>

# Rivanna Crash Course
It is assumed that participants of this workshop:

- are comfortable with the Linux command line
- have access to Rivanna (in order to use `singularity` and the `rivanna-training` allocation)

## Access

1. Terminal (e.g., Mac Terminal, [MobaXterm](https://mobaxterm.mobatek.net/download-home-edition.html), [PuTTY](https://www.chiark.greenend.org.uk/~sgtatham/putty/))<br/>```ssh <your_user_id>@rivanna.hpc.virginia.edu```

2. [FastX Web](https://rivanna-portal.hpc.virginia.edu)

See [here](https://www.rc.virginia.edu/userinfo/rivanna/login/) for more information.

## Commands

### Quota

Please make sure you have a few GB of disk space.

```
$ hdquota
Type             Location         Name                   Size Used Avail Use%
===============================================================================
home             /home            rs7wz                   50G   45G  5.7G  89%

Location        Age_Limit(Days)       Disk_Limit(GB)         Use(GB)             File_Limit          Use
=============================================================================================================
/scratch/rs7wz         90                  10240                529                 350000          122383
```

### SLURM allocation
For workshop allocations, use **rivanna-training**.

```
$ allocations

Allocations available to Ruoshi_Sun (rs7wz):

 * arcs_admin: less than 500 service-units remaining
 * ds5559: less than 25,000 service-units remaining
 * hpc_build: less than 203,417 service-units remaining
 * rivanna-training: less than 20,000 service-units remaining
```

### Module
To use software installed on Rivanna, use **module**.
```
$ module spider singularity

--------------------------------------------------------------------------------------------------------
  singularity:
--------------------------------------------------------------------------------------------------------
    Description:
      Singularity enables users to have full control of their environment. Singularity containers can
      be used to package entire scientific workflows, software and libraries, and even data.

     Versions:
        singularity/2.6.1
        singularity/3.1.1
        singularity/3.5.2
```

Earlier versions will be decommissioned, so please use Singularity **version 3.5.2**.
```
$ module load singularity/3.5.2
$ module avail

------------------------ /apps/modulefiles/standard/container/singularity/3.5.2 -------------------------
   caffe2/0.8.0              cp-analyst/2.2.1    inkscape/0.92.3           tensorflow/1.12.0-py36
   cellprofiler/2.2.0        danpos/2.2.2        patric/1.026              tensorflow/2.0.0-py36
   cellprofiler/3.1.8 (D)    hydrator/0.0.2      tensorflow/1.12.0-py27    tensorflow/2.1.0-py37  (D)
```

The above modules can now be loaded. For example:
```
$ module load tensorflow/2.0.0-py36 
Before using this container for the FIRST time, copy it to your personal home (or /scratch) directory using this command:
cp $CONTAINERDIR/tensorflow-2.0.0-py36.sif /home/$USER

To execute the default application inside the container, run this command:
singularity run --nv /home/$USER/tensorflow-2.0.0-py36.sif
```

---

<br>

# Singularity Overview

### Create
1. Download from a repository: `pull`/`build`
2. Convert from Docker: `pull`/`build`
3. Build from definition file: `build`

### Inspect
1. Show metadata: `inspect`
2. Show user-defined help: `run-help`
3. Verify cryptographic signatures: `verify` 

### Run
1. Run the user-defined default command: `run`
2. Run a shell interactively within the container: `shell`
3. Run a command within the container: `exec`

<br>

# Hands-On 1: Create and run image from Singularity Library
---
*Objectives:*

- URIs supported by Singularity and syntax
- Pull an image from Singularity Library
- Verify, inspect, and run an image

---

Command to download an image:
```
singularity pull [<SIF>] <URI>
singularity build <SIF> <URI>
```

<br>

#### Unified resource identifiers (URIs) supported by Singularity

| Repository | URI prefix |
|------------|------------|
| [Singularity Library](https://cloud.sylabs.io/library) | `library://` |
| [Docker Hub](https://hub.docker.com) | `docker://` |
| [Singularity Hub](https://singularity-hub.org) | `shub://` |

<br>

Syntax for `<URI>`:
```
<library|docker|shub>://<user>/<container>[:<tag>]
```

<br>

#### Singularity Image Format (SIF)

- Singularity images use the `*.sif` suffix
- Earlier versions use the `*.simg` suffix

<br>

#### Differences between `pull` and `build`

|`pull`|`build`|
|------|-------|
| downloads a pre-built image | converts image to the latest Singularity image format after downloading it |
| does not require output image name <SIF> | requires output image name <SIF> |

<br>

### Your first container

```
$ singularity pull library://godlovedc/demo/lolcow
INFO:    Downloading library image
 89.24 MiB / 89.24 MiB [==============================================================] 100.00% 60.03 MiB/s 1s
WARNING: Container might not be trusted; run 'singularity verify lolcow_latest.sif' to show who signed it
INFO:    Download complete: lolcow_latest.sif
```
Let's examine this image:
```
$ singularity verify lolcow_latest.sif
Container is signed by 1 key(s):

Verifying partition: FS:
D1799BF5746175CE62EA43A379411A81F643F561
[REMOTE]  Dave Godlove <d@sylabs.io>
[OK]      Data integrity verified

INFO:    Container verified: lolcow_latest.sif
```
```
$ singularity inspect lolcow_latest.sif
WARNING: No SIF metadata partition, searching in container...
org.label-schema.build-date: Thursday_25_October_2018_17:45:4_UTC
org.label-schema.schema-version: 1.0
org.label-schema.usage.singularity.deffile.bootstrap: library
org.label-schema.usage.singularity.deffile.from: ubuntu:16.04
org.label-schema.usage.singularity.version: 3.0.0-218.g093d5f68
```
```
$ singularity run-help lolcow_latest.sif
No help sections were defined for this image
```

Now let's run the image:
```
singularity run <SIF>
./<SIF>
```
(These two commands are equivalent.)

```
$ singularity run lolcow_latest.sif
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
$ singularity inspect --runscript lolcow_latest.sif
#!/bin/sh

    fortune | cowsay | lolcat
```

**Inspect the runscript before running an image!**

<br>

# Hands-On 2: Convert image from Docker

---
*Objectives:*

- Download and convert a Docker image from Docker Hub
- Convert a local Docker image

---

### Pulling from Docker Hub
> Pulling Docker images reduces reproducibility. If you were to pull a Docker image today and then wait six months and pull again, you are not guaranteed to get the same image. If any of the source layers has changed the image will be altered. -- <cite>Singularity 3.5 User Guide</cite>

```
$ singularity pull lolcow_docker.sif docker://godlovedc/lolcow
```
The `lolcow_docker.sif` image name is optional, but here we specify this to avoid overwriting the existing `lolcow_latest.sif`.


<br>

The `verify` command does not work for SIF files built from Docker images:
```
$ singularity verify lolcow_docker.sif
FATAL:   Failed to verify: lolcow_docker.sif: error while searching for signature blocks: no signatures found for system partition
```


### Converting from local Docker cache (not on Rivanna)

(Taken from Singularity 3.5 User Guide)

You can convert local Docker images into Singularity (e.g. on your personal computer).
Suppose you have the `godlovedc/lolcow:latest` image cached by Docker:
```
$ sudo docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
godlovedc/lolcow    latest              577c1fe8e6d8        16 months ago       241MB
```
You can build a Singularity image from it via:
```
$ sudo singularity build lolcow_from_docker_cache.sif docker-daemon://godlovedc/lolcow:latest
INFO:    Starting build...
Getting image source signatures
Copying blob sha256:a2022691bf950a72f9d2d84d557183cb9eee07c065a76485f1695784855c5193
 119.83 MiB / 119.83 MiB [==================================================] 6s
Copying blob sha256:ae620432889d2553535199dbdd8ba5a264ce85fcdcd5a430974d81fc27c02b45
 15.50 KiB / 15.50 KiB [====================================================] 0s
Copying blob sha256:c561538251751e3685c7c6e7479d488745455ad7f84e842019dcb452c7b6fecc
 14.50 KiB / 14.50 KiB [====================================================] 0s
Copying blob sha256:f96e6b25195f1b36ad02598b5d4381e41997c93ce6170cab1b81d9c68c514db0
 5.50 KiB / 5.50 KiB [======================================================] 0s
Copying blob sha256:7f7a065d245a6501a782bf674f4d7e9d0a62fa6bd212edbf1f17bad0d5cd0bfc
 3.00 KiB / 3.00 KiB [======================================================] 0s
Copying blob sha256:70ca7d49f8e9c44705431e3dade0636a2156300ae646ff4f09c904c138728839
 116.56 MiB / 116.56 MiB [==================================================] 6s
Copying config sha256:73d5b1025fbfa138f2cacf45bbf3f61f7de891559fa25b28ab365c7d9c3cbd82
 3.33 KiB / 3.33 KiB [======================================================] 0s
Writing manifest to image destination
Storing signatures
INFO:    Creating SIF file...
INFO:    Build complete: lolcow_from_docker_cache.sif
```

Note that this requires `sudo` privilege.

<br>

# Hands-On 3: Interactive shell

---
*Objectives:*

- Use a Singularity image interactively in its shell
- File system
- Bind paths

---

You can run a shell within a container:

```
$ singularity shell lolcow_latest.sif
Singularity>
```

Note the change in the shell prompt. You are now inside the container. Let's take a look at the file system.

```
Singularity> ls -l /
total 565
drwxr-xr-x    2 root  root      1279 Oct 25  2018 bin
drwxr-xr-x    2 root  root         3 Oct 25  2018 boot
drwxr-xr-x   22 root  root      3380 Jan  3 00:28 dev
lrwxrwxrwx    1 root  root        36 Jan 23 13:57 environment -> .singularity.d/env/90-environment.sh
drwxr-xr-x   51 root  root      1642 Oct 25  2018 etc
drwxr-xr-x    1 rs7wz users       60 Jan 23 13:58 home
drwxr-xr-x    9 root  root       145 Oct 25  2018 lib
drwxr-xr-x    2 root  root        43 Oct 25  2018 lib64
drwxr-xr-x    2 root  root         3 Oct 25  2018 media
drwxr-xr-x    2 root  root         3 Oct 25  2018 mnt
drwxr-xr-x   14 root  root         0 Jan 23 13:46 nv
drwxr-xr-x    2 root  root         3 Oct 25  2018 opt
dr-xr-xr-x  838 root  root         0 Oct 31 05:16 proc
dr-xr-xr-x   89 root  nogroup  45056 Jan 22 09:05 project
drwx------    2 root  root        46 Oct 25  2018 root
drwxr-xr-x    5 root  root        67 Oct 25  2018 run
drwxr-xr-x    2 root  root      1505 Oct 25  2018 sbin
drwxr-xr-x 4314 root  root    565760 Jan 23 11:20 scratch
drwxr-xr-x    3 rs7wz users       60 Jan 23 13:58 sfs
drwxr-xr-x    3 rs7wz users       60 Jan 23 13:58 share
lrwxrwxrwx    1 root  root        24 Jan 23 13:57 singularity -> .singularity.d/runscript
drwxr-xr-x    2 root  root         3 Oct 25  2018 srv
dr-xr-xr-x   13 root  root         0 Nov 13 09:41 sys
drwxrwxrwt   94 root  root      5100 Jan 23 13:58 tmp
drwxr-xr-x    1 rs7wz users       60 Jan 23 13:58 usr
drwxr-xr-x   11 root  root       160 Oct 25  2018 var
```

On Rivanna, the following host directories are accessible inside the container:

- Personal directories: `/home`, `/scratch`
- Leased storage shared by your research group: `/project`, `/nv`
- Some system directories: `/tmp`, `/sys`, `/proc`, `/dev`, `/usr`
- Your current working directory (if it is not a system directory)

Singularity bind mounts these directories at runtime. 
Other directories inside the container are owned by `root` and you cannot modify them.

<br>

**Exercise:** Run `ls -l` for your home directory both inside and outside the container. Verify that you see the same results. To exit the container shell, type `exit`.

<details><summary>Show command</summary>
<p>
```
$ singularity shell lolcow_latest.sif
Singularity> ls /home/$USER   # or ls ~
...
Singularity> exit
$ ls /home/$USER              # or ls ~
```
</p>
</details>

<br>

**Exercise:** View the content of `/etc/os-release` both inside and outside the container. Are they the same or different? Why?

<details><summary>Show command</summary>
<p>
```
$ singularity shell lolcow_latest.sif
Singularity> cat /etc/os-release
NAME="Ubuntu"
VERSION="16.04.5 LTS (Xenial Xerus)"
ID=ubuntu
ID_LIKE=debian
PRETTY_NAME="Ubuntu 16.04.5 LTS"
VERSION_ID="16.04"
HOME_URL="http://www.ubuntu.com/"
SUPPORT_URL="http://help.ubuntu.com/"
BUG_REPORT_URL="http://bugs.launchpad.net/ubuntu/"
VERSION_CODENAME=xenial
UBUNTU_CODENAME=xenial
Singularity> exit

$ cat /etc/os-release
NAME="CentOS Linux"
VERSION="7 (Core)"
ID="centos"
ID_LIKE="rhel fedora"
VERSION_ID="7"
PRETTY_NAME="CentOS Linux 7 (Core)"
ANSI_COLOR="0;31"
CPE_NAME="cpe:/o:centos:centos:7"
HOME_URL="https://www.centos.org/"
BUG_REPORT_URL="https://bugs.centos.org/"

CENTOS_MANTISBT_PROJECT="CentOS-7"
CENTOS_MANTISBT_PROJECT_VERSION="7"
REDHAT_SUPPORT_PRODUCT="centos"
REDHAT_SUPPORT_PRODUCT_VERSION="7"
```
</p>
</details>

<br>

To bind mount additional directories/files from the host, use `--bind` or `-B`:

```
$ singularity <run|shell|exec> -B <host_path>:<container_path> <SIF>
```

<br>

**Exercise:** Bind your scratch directory `/scratch/$USER` to `/scratch` and verify the contents.

<details><summary>Show command</summary>
```
$ singularity shell -B /scratch/$USER:/scratch lolcow_latest.sif
Singularity> ls /scratch
...
Singularity> exit
$ ls /scratch/$USER
```
</p>
</details>

<br>

To disable the default binding:
```
$ singularity shell -c lolcow_latest.sif
Singularity> ls /home/$USER
Singularity>
```

<br>

# Hands-On 4: Executing custom commands

---
*Objectives:*

- Run a custom command
- Run a custom script

---

Using the `exec` command you can run custom commands without shelling into the container:

```
$ singularity exec <SIF> <COMMAND>
```

**Exercise:** If you run `singularity exec lolcow_latest.sif cat /etc/os-release`, do you expect to see the host's OS info or the container's? Think for a moment before running this command. Did you guess correctly?

<details><summary>Show explanation</summary>
Although you did not shell into the container explicitly, the command is still executed within the container, where it does not have access to the host `/etc` directory. Therefore, the `/etc/os-release` of the container is shown.
</details>

### What about running a group of commands?

You can gather a list of commands into a "script" and pass that to the container via `exec`. Examples:
```
singularity exec <SIF> /bin/bash my_bash_script.sh
singularity exec <SIF> python my_python_script.py
```

<br>

**Exercise:** The `fortune` command prints a random message. Here are two ways to run `fortune` 5 times via the container.

Method 1: Put the for loop within the script executed by the container.
```
$ singularity exec lolcow_latest.sif /bin/bash my_bash_script.sh
```
```
#!/bin/bash
for i in {1..5}; do
    fortune
done
```

Method 2: Put the for loop outside the singularity command.
```
$ for i in {1..5}; do singularity exec lolcow_latest.sif fortune; done
```

Which method is better? Check by using the `time` command to time the total duration. Can you explain why?

<details><summary>Show command </summary>
```
$ time singularity exec lolcow_latest.sif my_bash_script.sh
...
real	0m0.189s
user	0m0.057s
sys	0m0.103s
```

```
$ time for i in {1..5}; do singularity exec lolcow_latest.sif fortune; done
...
real	0m0.804s
user	0m0.263s
sys	0m0.424s
```
</details>


<br>

# Advanced: The Definition File

The definition file is a set of instructions that is used to build a Singularity container:

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
- and [many more](https://sylabs.io/guides/3.5/user-guide/appendix.html#buildmodules)

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

Files in the `%files` section are always copied before the `%post` section is executed so that they are available during the build and configuration process.

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

List of commands to be executed upon `singularity run`.

#### `%labels`

Add metadata in the form of key-value pairs. Example:

```
%labels
    Author Ruoshi Sun
```

#### `%help`

Text to be displayed upon `singularity run-help`.

<br>

# Hands-On 5: Convert a Dockerfile

---
*Objectives:*

- Practice writing a basic definition file
- Convert a Dockerfile into a definition file
- Build a container from scratch

---

The `lolcow` container that we used before was prepared using this Dockerfile ([source](https://github.com/GodloveD/lolcow/blob/master/Dockerfile)):

```
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y fortune cowsay lolcat

ENV PATH /usr/games:${PATH}
ENV LC_ALL=C

ENTRYPOINT fortune | cowsay | lolcat
```

**Challenge:** Write the corresponding Singularity definition file.

Hints:

- Refer to the [skeleton](#advanced-the-definition-file) of a Singularity definition file.
- Use `docker` as the bootstrap agent.
- `RUN` commands are installation commands.
- `ENV` commands set environment variables. You'll need to use `export <env>=...`.
- `ENTRYPOINT` commands are executed at runtime.
- You will not be able to build this on Rivanna!

(Demo: Build container from scratch)

<br>

# Hands-On 6: Run a container on a GPU node

---
*Objectives:*

- Load singularity and tensorflow container on Rivanna
- SLURM script
- Submit a job to run the container on a GPU

---

First let's load singularity and tensorflow:

```
$ module load singularity/3.5.2
$ module avail

------------------------ /apps/modulefiles/standard/container/singularity/3.5.2 -------------------------
   caffe2/0.8.0              cp-analyst/2.2.1    inkscape/0.92.3           tensorflow/1.12.0-py36
   cellprofiler/2.2.0        danpos/2.2.2        patric/1.026              tensorflow/2.0.0-py36
   cellprofiler/3.1.8 (D)    hydrator/0.0.2      tensorflow/1.12.0-py27    tensorflow/2.1.0-py37  (D)

$ module load tensorflow/2.0.0-py36 
Before using this container for the FIRST time, copy it to your personal home (or /scratch) directory using this command:
cp $CONTAINERDIR/tensorflow-2.0.0-py36.sif /home/$USER

To execute the default application inside the container, run this command:
singularity run --nv /home/$USER/tensorflow-2.0.0-py36.sif
```

Copy the container:

```
$ cp $CONTAINERDIR/tensorflow-2.0.0-py36.sif /home/$USER
$ ls -l ~/tensorflow-2.0.0-py36.sif
-rwxr-xr-x 1 rs7wz users 1994727424 Jan 24 14:22 tensorflow-2.0.0-py36.sif
```

**Exercise:**

1. Inspect the runscript and help file of the tensorflow container.
2. Find the Python version within the container. Is it the same as that of the host?
3. Is Numpy available in the container? What command would you run to get its version?

<details><summary>Show command 1</summary>
```
$ singularity inspect --runscript tensorflow-2.0.0-py36.sif
$ singularity run-help tensorflow-2.0.0-py36.sif
```
</details>

<details><summary>Show command 2</summary>
```
$ singularity exec tensorflow-2.0.0-py36.sif python -V
$ python -V
```
</details>

<details><summary>Show command 3</summary>
Method 1 (interactive):
```
$ singularity shell tensorflow-2.0.0-py36.sif
Singularity> python
Python 3.6.8 (default, Aug 20 2019, 17:12:48)
[GCC 8.3.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import numpy
>>> numpy.__version__
'1.17.2'
```

Method 2:

```
$ singularity exec tensorflow-2.0.0-py36.sif pip list|grep numpy
numpy                1.17.2
```
</details>

### SLURM 
Computationally intensive tasks must be performed on compute nodes. This is accomplished via job submission to Simple Linux Utility Resource Manger (SLURM).

Copy the sample SLURM script to your home directory:
```
$ cp /share/resources/tutorials/singularity_ws/tensorflow-2.0.0.slurm ~
```

```
#!/usr/bin/env bash
#SBATCH -J tftest                     # job name
#SBATCH -o tftest-%A.out              # output file
#SBATCH -e tftest-%A.err              # error file
#SBATCH -p gpu                        # partition/queue
#SBATCH --gres=gpu:1                  # request 1 gpu
#SBATCH -c 1                          # request 1 cpu core
#SBATCH -t 00:01:00                   # time limit: 1 min
#SBATCH -A rivanna-training           # account name for workshop

module purge                          # start with clean environment
module load singularity/3.5.2

singularity exec --nv tensorflow-2.0.0-py36.sif python tf2test.py
```

Copy the Python script to your home directory:
```
$ cp /share/resources/tutorials/singularity_ws/tf2test.py ~
```

```
#!/usr/bin/env python

import tensorflow as tf

print(tf.test.is_gpu_available())
output = tf.constant('Testing Tensorflow')
print (output)
```

Submit the job:
```
$ sbatch tensorflow-2.0.0.slurm
Submitted batch job 8388125
```

Check your job status:
```
$ squeue -u $USER
             JOBID PARTITION     NAME     USER ST       TIME  NODES NODELIST(REASON)
           8388125       gpu   tftest    rs7wz  R       0:01      1 udc-ba25-28
```

`tftest-<JOBID>.out`:
```
True
tf.Tensor(b'Testing Tensorflow', shape=(), dtype=string)
```

`tftest-<JOBID>.err`:

<details><summary>Show content</summary>
```
2020-01-27 10:34:02.517153: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcuda.so.1
2020-01-27 10:34:02.603723: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1618] Found device 0 with properties:
name: Tesla K80 major: 3 minor: 7 memoryClockRate(GHz): 0.8235
pciBusID: 0000:89:00.0
2020-01-27 10:34:02.655229: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcudart.so.10.0
2020-01-27 10:34:02.897823: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcublas.so.10.0
2020-01-27 10:34:03.006773: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcufft.so.10.0
2020-01-27 10:34:03.103227: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcurand.so.10.0
2020-01-27 10:34:03.524056: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcusolver.so.10.0
2020-01-27 10:34:03.912433: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcusparse.so.10.0
2020-01-27 10:34:04.230756: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcudnn.so.7
2020-01-27 10:34:04.234378: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1746] Adding visible gpu devices: 0
2020-01-27 10:34:04.235694: I tensorflow/core/platform/cpu_feature_guard.cc:142] Your CPU supports instructions that this TensorFlow binary was not compiled to use: AVX2 FMA
2020-01-27 10:34:04.257015: I tensorflow/core/platform/profile_utils/cpu_utils.cc:94] CPU Frequency: 2399895000 Hz
2020-01-27 10:34:04.259193: I tensorflow/compiler/xla/service/service.cc:168] XLA service 0x4e0d4b0 executing computations on platform Host. Devices:
2020-01-27 10:34:04.259208: I tensorflow/compiler/xla/service/service.cc:175]   StreamExecutor device (0): Host, Default Version
2020-01-27 10:34:04.339984: I tensorflow/compiler/xla/service/service.cc:168] XLA service 0x4e0f350 executing computations on platform CUDA. Devices:
2020-01-27 10:34:04.340009: I tensorflow/compiler/xla/service/service.cc:175]   StreamExecutor device (0): Tesla K80, Compute Capability 3.7
2020-01-27 10:34:04.341724: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1618] Found device 0 with properties:
name: Tesla K80 major: 3 minor: 7 memoryClockRate(GHz): 0.8235
pciBusID: 0000:89:00.0
2020-01-27 10:34:04.341766: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcudart.so.10.0
2020-01-27 10:34:04.341780: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcublas.so.10.0
2020-01-27 10:34:04.341792: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcufft.so.10.0
2020-01-27 10:34:04.341803: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcurand.so.10.0
2020-01-27 10:34:04.341814: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcusolver.so.10.0
2020-01-27 10:34:04.341825: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcusparse.so.10.0
2020-01-27 10:34:04.341837: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcudnn.so.7
2020-01-27 10:34:04.345062: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1746] Adding visible gpu devices: 0
2020-01-27 10:34:04.346938: I tensorflow/stream_executor/platform/default/dso_loader.cc:44] Successfully opened dynamic library libcudart.so.10.0
2020-01-27 10:34:04.349478: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1159] Device interconnect StreamExecutor with strength 1 edge matrix:
2020-01-27 10:34:04.349495: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1165]      0
2020-01-27 10:34:04.349501: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1178] 0:   N
2020-01-27 10:34:04.353349: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1304] Created TensorFlow device (/job:localhost/replica:0/task:0/device:GPU:0 with 10794 MB memory) -> physical GPU (device: 0, name: Tesla K80, pci bus id: 0000:89:00.0, compute capability: 3.7)
```
</details>

(Demo: Go over definition file for this container)

<br>

# Other Features

### Sandbox

- Create and use a writable directory
- Useful for debugging definition file

```
singularity build --sandbox <directory> <URI/DEF>
singularity shell --writable <directory>
```

### Add environment variables in `%post` section

```
%post
    <some step that determines x>  # cannot determine x before build
    echo 'export VARIABLE_NAME=x' >>$SINGULARITY_ENVIRONMENT
```

### Remote

- Host your own repository
- Push image to repository
- Remote build

See [here](https://sylabs.io/guides/3.5/user-guide/endpoint.html)

<br>

**Congratulations - you have completed this workshop!**

---

# Next Steps

- [Request an allocation](https://www.rc.virginia.edu/userinfo/rivanna/allocations/) (students must do so through PI or staff)
- Search containers on repositories
- Ask us to prepare a container ([submit a ticket](https://www.rc.virginia.edu/support/))
- Start doing your research!

# References

- [Singularity User Guide](https://sylabs.io/guides/3.5/user-guide)
- [Comparison between Singularity and Docker] (https://sylabs.io/guides/3.5/user-guide/singularity_and_docker.html)
- Container repositories:
    - [Singularity Library](https://cloud.sylabs.io/library)
    - [Docker Hub](https://hub.docker.com)
    - [Singularity Hub](https://singularity-hub.org)
- [SLURM on Rivanna](https://www.rc.virginia.edu/userinfo/rivanna/slurm/)

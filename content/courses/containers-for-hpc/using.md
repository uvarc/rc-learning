---
title: Using Containers on Rivanna [Apptainer]
toc: true
type: book
weight: 3

---

Logging in to Rivanna:

- Connect to Rivanna
    - SSH client or FastX Web
- Run `hdquota`
    - Make sure you have a few GBs of free space
- Run `allocations`
    - Check if you have `rivanna-training`

---

## Basic Apptainer commands

### Pull

To download a container hosted on a registry, use the `pull` command. Docker images are automatically converted into Apptainer format.

`apptainer pull [<SIF>] <URI>`

- `<URI>` (Unified resource identifiers)
    - `[library|docker|shub]://[<user>/]<repo>[:<tag>] `
    - Default prefix: `library` ([Singularity Library](https://cloud.sylabs.io/library))
    - `user`: optional; may be empty (e.g. `apptainer pull ubuntu`)
    - `tag`: optional; default: `latest`
- `<SIF>` (Singularity image format)
    - Optional
    - Rename image; default: `<repo>_<tag>.sif`

#### Pull lolcow from Docker Hub

```bash
apptainer pull docker://rsdmse/lolcow
```

### Inspect

Inspect an image before running it via `inspect`.

`apptainer inspect <SIF>`

```bash
$ apptainer inspect lolcow_latest.sif 
org.label-schema.build-arch: amd64
org.label-schema.build-date: Monday_8_January_2024_10:21:0_EST
org.label-schema.schema-version: 1.0
org.label-schema.usage.apptainer.version: 1.2.2
org.label-schema.usage.singularity.deffile.bootstrap: docker
org.label-schema.usage.singularity.deffile.from: rsdmse/lolcow
```

#### Inspect runscript

This is the default command of the container. (Docker `ENTRYPOINT` is preserved.)

`apptainer inspect --runscript <SIF>`

```bash
$ apptainer inspect --runscript lolcow_latest.sif 
#!/bin/sh
OCI_ENTRYPOINT='"/bin/sh" "-c" "fortune | cowsay | lolcat"'
...
```

### Run

There are three ways to run a container: `run`, `shell`, `exec`.

#### `run`

Execute the default command in `inspect --runscript`.

CPU: `apptainer run <SIF>` = `./<SIF>`

GPU: `apptainer run --nv <SIF>` (later)

```bash
./lolcow_latest.sif
```

#### `shell`

Start a Apptainer container interactively in its shell.

`apptainer shell <SIF>`

```bash
$ apptainer shell lolcow_latest.sif
Apptainer>
```

The change in prompt indicates you are now inside the container.

To exit the container shell, type `exit`.

#### `exec`

Execute custom commands without shelling into the container.

`apptainer exec <SIF> <command>`

```bash
$ apptainer exec lolcow_latest.sif which fortune
/usr/bin/fortune
```

### Bind mount

- Apptainer bind mounts these host directories at runtime:
    - Personal directories: `/home`, `/scratch`
    - Leased storage shared by your research group: `/project`, `/standard`, `/nv`
    - Your current working directory
- To bind mount additional host directories/files, use `--bind`/`-B`:

```bash
apptainer run|shell|exec -B <host_path>[:<container_path>] <SIF>
```

---

## Exercises

1. For each of the three executables `fortune`, `cowsay`, `lolcat`, run `which` both inside and outside the `lolcow` container. Which one exists on both the host and the container?
1. a) Run `ls -l` for your home directory both inside and outside the container. Verify that you get the same result. b) To disable all bind mounting, use `run|shell|exec -c`. Verify that `$HOME` is now empty.
1. View the content of `/etc/os-release` both inside and outside the container. Are they the same or different? Why?
1. (Advanced) Let's see if we can run the host `gcc` inside the lolcow container. First load the module: `module load gcc`
    - Verify that the path to `gcc` (hint: `which`) is equal to `$EBROOTGCC/bin`.
    - Verify that `$EBROOTGCC/bin` is in your `PATH`.
    - Now shell into the container (hint: `-B /apps`) and examine the environment variables `$EBROOTGCC` and `$PATH`. Are they the same as those on the host? Why (not)?
    - In the container, add `$EBROOTGCC/bin` to `PATH` (hint: `export`). Is it detectable by `which`? Can you launch `gcc`? Why (not)?

---

## Container Modules

### Apptainer module

On Rivanna, the `apptainer` module serves as a "toolchain" that will activate container modules. **You must load `apptainer` before loading container modules.**

See what modules are available by default:
```bash
module purge
module avail
```

Check the module version of Apptainer:
```bash
module spider apptainer
```

Load the Apptainer module and check what modules are available:
```bash
module load apptainer
module avail
```

You can now load container modules.

### Container modules under apptainer toolchain

The corresponding `run` command is displayed upon loading a module.

```bash
$ module load tensorflow
To execute the default application inside the container, run:
apptainer run --nv $CONTAINERDIR/tensorflow-2.10.0.sif

$ module list
Currently Loaded Modules:
  1) apptainer/1.2.2   2) tensorflow/2.10.0
```

- `$CONTAINERDIR` is an environment variable. It is the directory where containers are stored on Rivanna.
- After old container module versions are deprecated, the corresponding containers are placed in `$CONTAINERDIR/archive`. These are inaccessible through the module system, but you are welcome to use them if necessary.

---

## Exercise

1. What happens if you load a container module without loading Apptainer first?
    ```bash
    module purge
    module list
    module load tensorflow
    ```
1. Check the versions of tensorflow via `module spider tensorflow`. How would you load a non-default version?
1. What is the default command of the tensorflow container? Where was it pulled from?

---

## Container Slurm job (TensorFlow on GPU)

- Computationally intensive tasks must be performed on compute nodes.
- Slurm is Rivanna's resource manager.
- Prepare a Slurm script to submit a job.

Copy these files:

```bash
cp /share/resources/tutorials/apptainer_ws/tensorflow-2.10.0.slurm .
cp /share/resources/tutorials/apptainer_ws/mnist_example.{ipynb,py} .
```

Examine Slurm script:

```bash
#!/bin/bash
#SBATCH -A rivanna-training      # account name
#SBATCH -p gpu                   # partition/queue
#SBATCH --gres=gpu:1             # request 1 gpu
#SBATCH -c 1                     # request 1 cpu core
#SBATCH -t 00:05:00              # time limit: 5 min
#SBATCH -J tftest                # job name
#SBATCH -o tftest-%A.out         # output file
#SBATCH -e tftest-%A.err         # error file

# start with clean environment
module purge
module load apptainer tensorflow/2.10.0

apptainer run --nv $CONTAINERDIR/tensorflow-2.10.0.sif mnist_example.py
```

Submit job:

```bash
sbatch tensorflow-2.10.0.slurm
```

#### What does `--nv` do?

See [Apptainer GPU user guide](https://apptainer.org/user-docs/master/gpu.html#nvidia-gpus-cuda-standard)

```bash
$ apptainer shell $CONTAINERDIR/tensorflow-2.10.0.sif
Apptainer> ls /.apptainer.d/libs

$ apptainer shell --nv $CONTAINERDIR/tensorflow-2.10.0.sif
Apptainer> ls /.apptainer.d/libs
libEGL.so		  libGLX.so.0		       libnvidia-cfg.so			  libnvidia-ifr.so
libEGL.so.1		  libGLX_nvidia.so.0	       libnvidia-cfg.so.1		  libnvidia-ifr.so.1
...
```

---

## Custom Jupyter Kernel

### "Can I use my own container on JupyterLab?"

Suppose you need to use TensorFlow 2.11.0 on JupyterLab. First, note we do not have `tensorflow/2.11.0` as a module:

```bash
module spider tensorflow
```

Go to [TensorFlow's Docker Hub page](https://hub.docker.com/r/tensorflow/tensorflow/tags?page=1&name=2.11.0) and search for the tag (i.e. version). You'll want to use one that has the `-gpu-jupyter` suffix. Pull the container in your Rivanna account.

### Installation

#### Manual
1. Create kernel directory

```bash
DIR=~/.local/share/jupyter/kernels/tensorflow-2.11.0
mkdir -p $DIR
cd $DIR
```

2. Write `kernel.json`

```
{
 "argv": [
  "/home/<user>/.local/share/jupyter/kernels/tensorflow-2.11.0/init.sh",
  "-f",
  "{connection_file}"
 ],
 "display_name": "Tensorflow 2.11",
 "language": "python"
}
```

3. Write `init.sh`

```bash
#!/bin/bash
module load apptainer
apptainer exec --nv /path/to/sif python -m ipykernel $@
```

4. Change `init.sh` into an executable
```bash
chmod +x init.sh
```

**Easy to automate!**

#### JKRollout

This tool is currently limited to Python. The container must have the `ipykernel` Python package.

```text
Usage: jkrollout sif display_name [gpu]
    sif          = file name of *.sif
    display_name = name of Jupyter kernel
    gpu          = enable gpu (default: false)
```

```bash
jkrollout /path/to/sif "Tensorflow 2.11" gpu
```

### Test your new kernel

- Go to https://rivanna-portal.hpc.virginia.edu
- Select JupyterLab
    - Rivanna Partition: GPU
    - Work Directory: (location of your `mnist_example.ipynb`)
    - Allocation: `rivanna-training`
- Select the new "TensorFlow 2.11" kernel
- Run `mnist_example.ipynb`

### Remove a custom kernel

```bash
rm -rf ~/.local/share/jupyter/kernels/tensorflow-2.11.0
```

---

## References

- [Apptainer User Guide](https://apptainer.org/docs/user/latest/)
    - [Overview](https://apptainer.org/docs/user/latest/quick_start.html)
    - [Bind Path and Mounts](https://apptainer.org/docs/user/latest/bind_paths_and_mounts.html)

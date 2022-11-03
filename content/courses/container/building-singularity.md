---
title: Appendix: Building Containers [Singularity]
toc: true
type: book
weight: 10

---
# Singularity Commands Overview

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

# Example: lolcow

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

# Pulling from Docker Hub
> Pulling Docker images reduces reproducibility. If you were to pull a Docker image today and then wait six months and pull again, you are not guaranteed to get the same image. If any of the source layers has changed the image will be altered. -- <cite>Singularity 3.5 User Guide</cite>

```
$ singularity pull lolcow_docker.sif docker://godlovedc/lolcow
```
The `lolcow_docker.sif` image name is optional, but here we specify this to avoid overwriting the existing `lolcow_latest.sif`.


The `verify` command does not work for SIF files built from Docker images:
```
$ singularity verify lolcow_docker.sif
FATAL:   Failed to verify: lolcow_docker.sif: error while searching for signature blocks: no signatures found for system partition
```


# Converting from local Docker cache (not on Rivanna)

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

# Singularity Definition File

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

## Header

- At the top of the def file
- Sets the base OS or base container

### `Bootstrap` (mandatory)
This is the very first entry. It defines the bootstrap agent:

- `library`
- `docker`
- `shub`
- and [many more](https://sylabs.io/guides/3.5/user-guide/appendix.html#buildmodules)

### `From` (mandatory)
Define the base container.

```
From: [<collection>/]<container>[:<tag>]
```

## Section
Each section starts with `%`. All sections are optional.

### `%files`

Copy files into the container.

```
%files
    <source1> [<destination1>]
    <source2> [<destination2>]
    ...
```

Files in the `%files` section are always copied before the `%post` section is executed so that they are available during the build and configuration process.

### `%post`

Installation commands. Example:

```
%post
    apt-get update && apt-get -y install lolcat
```

### `%environment`

Define environment variables (set at runtime). Not available at build time. Example:

```
%environment
    export LC_ALL=C
```

### `%runscript`

List of commands to be executed upon `singularity run`.

### `%labels`

Add metadata in the form of key-value pairs. Example:

```
%labels
    Author Ruoshi Sun
```

### `%help`

Text to be displayed upon `singularity run-help`.

<br>

# Convert a Dockerfile

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

# Other Features

## Sandbox

- Create and use a writable directory
- Useful for debugging definition file

```
singularity build --sandbox <directory> <URI/DEF>
singularity shell --writable <directory>
```

## Add environment variables in `%post` section

```
%post
    <some step that determines x>  # cannot determine x before build
    echo 'export VARIABLE_NAME=x' >>$SINGULARITY_ENVIRONMENT
```

## Remote

- Host your own repository
- Push image to repository
- Remote build

See [here](https://sylabs.io/guides/3.5/user-guide/endpoint.html)


---

# References
- [Apptainer User Guide](https://apptainer.org/docs/user/1.0/index.html)
- [Definition File](https://apptainer.org/docs/user/1.0/definition_files.html)

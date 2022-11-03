---
title: Building Containers [Docker]
toc: true
type: book
weight: 3

---

_Docker, Dockerfile, Docker Hub_

Starting from a simple Dockerfile, we will adopt best practices sequentially and see their effect.

# Prerequisites

1. Command line (Unix)

2. Install Docker

    To follow along on your own computer, please install Docker Desktop and register for a free account on Docker Hub. Both can be found [here](https://www.docker.com/get-started).

    After the installation, open a terminal ("cmd" on Windows) and make sure you can execute the command `docker run hello-world` successfully.

3. Set `BUILDKIT_PROGRESS=plain` for plain output (or remember to run `docker build --progress=plain ...`).

---

# Building _for_ (not _on_) Rivanna

- **Docker**
    - No Docker on Rivanna
    - Docker Hub
    - Can be converted into Singularity
- Singularity
    - Users cannot build on Rivanna (needs `sudo` privilege)
    - Singularity Library/Hub (more limitations)
    - Refer to [workshop](https://learning.rc.virginia.edu/workshops/singularity/) in Spring 2020

# Intro to Dockerfile: lolcow

`fortune | cowsay | lolcat`

- fortune cookie
- talking cow
- rainbow color

Steps:
1. Choose a base image
2. Install software dependencies (if any)
3. Install software

## Step 1: Choose a base image

Use `FROM` to specify the base image. In this example, we'll use Ubuntu 16.04. You do not need to install this on your computer - Docker will pull from Docker Hub when you build it.

```dockerfile
FROM ubuntu:16.04
```

- OS: `ubuntu`, `debian`, `centos`, ...
- Doesn't have to be a bare OS
    - `python`, `continuumio/miniconda3`, `node`, `nvidia/cuda`, etc.

[Dockerfile reference](https://docs.docker.com/engine/reference/builder/#format)

## Steps 2 & 3: Install software

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

### Summary so far

- Build:
    - Update package manager
    - Automatic yes to prompt
- Run:
    - Use `--rm` to remove container after it exits
    - Use `-it` for interactive processes (e.g. shell)
- Problems:
    - User needs to know path to executable
    - User just wants to run "lolcow"

## Use `ENV` to set environment variable

This is equivalent to `export PATH=/usr/games:${PATH}` but it is preserved at runtime. In doing so we can execute `fortune`, `cowsay`, and `lolcat` directly without specifying the full path.

```dockerfile
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y fortune cowsay lolcat

ENV PATH=/usr/games:${PATH}
```

## Use `ENTRYPOINT` to set default command

```dockerfile
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y fortune cowsay lolcat

ENV PATH=/usr/games:${PATH}

ENTRYPOINT fortune | cowsay | lolcat
```

Finally, we can simply run `docker run --rm -it <img>` to get the desired behavior. You now know how to build a working Docker container.

# 4 Best Practices

While our container is functional, there is a lot of room for improvement. We shall look at some important best practices for writing Dockerfiles.

## 0. Package manager cache busting

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

## 1. Clean up

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

## 2. Only install what's needed

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

## 3. Use a smaller base image

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

## Image size comparison

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

## Summary

1. Choose a base image (`FROM`)
2. Install software dependencies (`RUN`)
3. Install software (`RUN`)
4. Clean up (in same `RUN` statement as installation)
5. Define environment variables (`ENV`)
6. Define default command (`ENTRYPOINT`)

# Push to Docker Hub

You can push your image to Docker Hub easily. First, let's set our lolcow version 3 as the latest.

```bash
docker tag <user>/lolcow:3 <user>/lolcow:latest
```

Then sign in to Docker Hub and push as follows:

```bash
docker login
docker push <user>/lolcow:latest
```

## Docker Hub interface

In your browser, go to `https://hub.docker.com/r/<user>/lolcow`.

- Overview: 
    - Sync with GitHub to update `README.md`; or
    - Use [docker-pushrm](https://github.com/christian-korneck/docker-pushrm)
- Tags:
    - List all versions
    - View image history if Dockerfile not provided
    - Compressed size is much smaller than size on disk

---

# Case Studies (hands-on)

By now, we know how to write a simple Dockerfile to install software using the distro's package manager. In practice, we may encounter software that does not exist in the package list. How do we deal with such cases?

## Compiled language (C++) 

https://github.com/lilab-bcb/cumulus_feature_barcoding

Hints:
- You do not have to start from a bare OS. Search for `gcc` on Docker Hub.
- Install `build-essential` if you are starting from a bare OS.
- Version pinning - to choose a specific version, download from https://github.com/lilab-bcb/cumulus_feature_barcoding/releases (you will need `wget`).

## Interpreted language (Python)

https://docs.qiime2.org/2022.8/install/native/#install-qiime-2-within-a-conda-environment

Hints:
- Click on "Linux" to get the URL for the yaml file. Download the yaml file in the same directory as your Dockerfile.
- You do not have to start from a bare OS in your Dockerfile. Search for `miniconda3` on Docker Hub.
- (Recommended) There is a much faster dependency solver than conda - micromamba. If you use it as the base image, see [here](https://github.com/mamba-org/micromamba-docker#quick-start) and [here](https://github.com/mamba-org/micromamba-docker#activating-a-conda-environment-for-entrypoint-commands) for instructions.
- Use the suggested `COPY` and `ENTRYPOINT` statements.
- After you're done, compare with the [official Dockerfile](https://github.com/qiime2/vm-playbooks/blob/0fda9dce42802596756986e2f80c38437872c66e/docker/Dockerfile) and image size. What is the biggest reason for the difference?

## General Remarks

- Play with different base images and package managers.
- If you encounter a Docker statement that you have not used before, first check the official documentation for best practices.
- A comprehensive list of dependencies may be lacking. Some developers may not specify any at all. You will have to rely on a combination of experience, error message, and web search. (Most likely all of the above.)
- Especially for Python packages, versions may be too permissive or too restrictive such that, in either case, future installation of the application will fail. (I have encountered both.) Tweak the versions until it works.
- The next step is "multi-stage build" which is covered in the [Minimal Containers](/workshops/minimal-containers) workshop. There you will learn how to distinguish between buildtime versus runtime dependencies and separate them out.

# Clean Up

If you build containers often, you can run out of disk space quickly. To clean up:

1. Run `docker rmi <IMAGE_ID>` to remove a specific image.
1. Run `docker system prune` to clean up cache. (This will not affect images that are tagged.)

    ```bash
    $ docker system prune
    WARNING! This will remove:
      - all stopped containers
      - all networks not used by at least one container
      - all dangling images
      - all dangling build cache

    Are you sure you want to continue? [y/N] y
    ```

---

# References

- [UVA Rivanna-Docker GitHub](https://github.com/uvarc/rivanna-docker)
    - Dockerfiles by UVA Research Computing
    - [Tips](https://github.com/uvarc/rivanna-docker/wiki/Tips)
- [_Best practices for writing Dockerfiles_](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [Natanael Copa, _Small, Simple, and Secure: Alpine Linux under the Microscope_, DockerCon EU (2017)](https://youtu.be/sIG2P9k6EjA)

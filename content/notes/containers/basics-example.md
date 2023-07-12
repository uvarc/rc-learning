---
title: A Quick Example
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 110

menu:
  containers:
      parent: Container Basics
---

Cowsay is a Linux game that prints ASCII art of a cow and a speech bubble containing a string input by the user. The Whalesay image modifies that game by replacing the cow with a whale.

Let's try pulling the whalesay image from DockerHub and running a container on our machine.

## 1. Pull the Image

```
> docker pull docker/whalesay

Using default tag: latest
latest: Pulling from docker/whalesay
Image docker.io/docker/whalesay:latest uses outdated schema1 manifest format. Please upgrade to a schema2 image for better future compatibility. More information at https://docs.docker.com/registry/spec/deprecated-schema-v1/
e190868d63f8: Pull complete 
909cd34c6fd7: Pull complete 
0b9bfabab7c1: Pull complete 
a3ed95caeb02: Pull complete 
00bf65475aba: Pull complete 
c57b6bcc83e3: Pull complete 
8978f6879e2f: Pull complete 
8eed3712d2cf: Pull complete 
Digest: sha256:178598e51a26abbc958b8a2e48825c90bc22e641de3d31e18aaf55f3258ba93b
Status: Downloaded newer image for docker/whalesay:latest
docker.io/docker/whalesay:latest
```

You'll notice that there are several lines with a hash/SHA ID followed by "Pull complete". These correspond to the different layers of the image, or the different components that will make up the compute environment when we run the container. We will talk more about layers when we cover building our own images.

## 2. List all pulled images

By listing all images, we are confirming that we successfully pulled the whalesay image.
```
> docker images

REPOSITORY        TAG       IMAGE ID       CREATED       SIZE
docker/whalesay   latest    6b362a9f73eb   8 years ago   247MB
```

## 3. Run the Container
```
> docker run docker/whalesay
```

Just running whalesay looks like it didn't do anything since we didn't provide a command. What happened behind-the-scenes is that Docker spun up an instance of the whalesay image, ran an empty command, and then exited once that was complete.

Let's try running a container again, this time adding the cowsay command at the end.

```
> docker run docker/whalesay cowsay "hi there"

 __________ 
< hi there >
 ---------- 
    \
     \
      \     
                    ##        .            
              ## ## ##       ==            
           ## ## ## ##      ===            
       /""""""""""""""""___/ ===        
  ~~~ {~~ ~~~~ ~~~ ~~~~ ~~ ~ /  ===- ~~~   
       \______ o          __/            
        \    \        __/             
          \____\______/ 
          
```


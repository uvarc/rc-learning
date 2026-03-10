---
title: Why Use Containers?
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 20
menu:
  containers:
      parent: Containers
---

Have you ever tried using new code or software from an exciting paper you just read, only to end up spending hours figuring out which versions of the dependencies work on your own machine? Containers eliminate that issue altogether!

A container is a single unit of software that contains all the packages and code you need to run an application. Sometimes that application is as small as a single function (like printing 'Hello World!'), and sometimes that application is an entire web app. A container will always run the same, regardless of the host system it runs on--making it the perfect solution for sharing reproducible code.

There are several container technologies out there, but the big ones are Docker and Singularity. Docker is what you will encounter most often in the wild. Singularity (now called Apptainer) is used on HPC systems where most users don't have root access.

<img src="/notes/containers/img/docker.png" /> 
<img src="/notes/containers/img/apptainer.png" /> 
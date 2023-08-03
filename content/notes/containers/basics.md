---
title: Container Basics
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 100

menu:
  containers:
      name: Container Basics
---

To run and build containers, you will need Docker Desktop installed on your local machine. Instructions and installation files can be found here: https://docs.docker.com/engine/install/.

## Terminology

**Image**: The layers of libraries, code, and configuration that make up the environment that you need to run your application. 

**Container**: A running instance of an image. You can have many containers of a single image run simultaneously.

**DockerHub**: An online registry for Docker images (similar to GitHub)

## Commonly Used Docker Commands

- **docker pull**: Fetches an image from a container registry to your local machine

- **docker images**: List all locally available images (kind of like ls)

- **docker run**: Run a container based on a particular image

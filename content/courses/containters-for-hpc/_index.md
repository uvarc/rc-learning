---
date : "2022-11-03T00:00:00-05:00"
title : "Software Containers for HPC"
summary: "An Introduction to using and building software containers."
authors: [rs]
categories: ["containers","rivanna","hpc"]
tags: [containers, rivanna, hpc]
toc: true
type: book
weight: 1

---

## Overview
This short course is an introduction to using and building software containers, structured as follows:

- Introduction to Software Containers
- Using Containers on Rivanna [Singularity]
    - pull and convert Docker containers
    - inspect a container
    - run containers interactively and non-interactively
    - navigate container modules
    - submit container jobs via Slurm
    - create custom Jupyter kernels
- Building Containers [Docker]
    - Dockerfile
    - best practices
    - DockerHub
    - case studies
- Minimal Containers [Docker]
    - multistage build
    - OS-less base images
    - dynamic vs static linking
- Appendix: Building Containers [Singularity]
    - pull
    - convert local Docker image
    - Singularity definition file
    - convert Dockerfile

The chapters are mutually independent, except for "Minimal Containers" that requires "Building Containers [Docker]" or equivalent knowledge. The appendix on building Singularity containers is derived from a workshop that we no longer offer.

**You are strongly encouraged to repeat the examples and do the exercises.**

## Prerequisites
- Linux command line.
- Rivanna account for the "Using Containers" chapter.
- Docker installation and DockerHub account for the "Building Containers [Docker]" chapter.

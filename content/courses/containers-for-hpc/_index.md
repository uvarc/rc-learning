---
date: "2022-11-03"
title: "Software Containers for HPC"
summary: "An Introduction to using and building software containers."
authors: [rs]
categories: ["Containers","HPC"]
tags: [containers, hpc]
weight: 1
---

## Overview
This short course is an introduction to using and building software containers, structured as follows:

- Introduction to Software Containers
- Using Containers on HPC [Apptainer]
    - pull and convert Docker containers
    - inspect a container
    - run containers interactively and non-interactively
    - navigate container modules
    - submit container jobs via Slurm
    - create custom Jupyter kernels
- Building Containers on HPC [Apptainer]
    - definition file
    - best practices
    - registry
    - case studies
- Appendix 1: Building Containers [Docker]
    - Dockerfile
    - best practices
    - DockerHub
    - case studies
- Appendix 2: Minimal Containers [Docker]
    - multistage build
    - OS-less base images
    - dynamic vs static linking

The chapters are mutually independent, except for "Minimal Containers" that requires "Building Containers [Docker]" or equivalent knowledge.

**You are strongly encouraged to repeat the examples and do the exercises.**

## Prerequisites
- Linux command line.
- Rivanna account for the main chapters.
- Docker installation and DockerHub account for the appendices.

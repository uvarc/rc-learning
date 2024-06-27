---
date : "2024-6-03T00:00:00-05:00"
title: Introduction to Software Containers
toc: true
type: book
weight: 2

---

<https://www.docker.com/resources/what-container>

## Why use software containers?
- **Simple** Containers simplify software installation and management.
- **Portable** You can build an image on one machine and run it on another.
- **Reproducible** Versioning and freezing of containers enable data reproducibility.

## Characteristics
Containers
- share the OS kernel of the host
- virtualize the OS instead of hardware
- have much less overhead and faster deployment than a VM

## Apptainer/Singularity is designed for HPC

- Does not require sudo privilege to run (unlike Docker)
- Interoperates well with HPC resource managers in multi-node environments
- Easily makes use of GPUs, high speed networks, parallel filesystems
- Able to convert Docker images

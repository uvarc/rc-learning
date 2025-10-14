---
title: Working with Files
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 400

menu:
  hpc-intro:
    name: Working with Files
---
<style>
.embedded-video {
    aspect-ratio: 16 / 9;
    width: 100%;
    max-width: 600px;
}
</style>  
<iframe class="embedded-video" src="https://www.youtube.com/embed/asN63Ujhzks?rel=0" title="Working with Files video" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

Files are the foundation of working with an HPC cluster.  We need to be able to

* Transfer files to and from the cluster
* Edit text files
* Create files through the software we run

Each user has a `home` location and a `scratch` location.  When you log in you will be in the `home` location.  For now we will assume you will work with files in your home folder.  We will discuss the scratch folder later.


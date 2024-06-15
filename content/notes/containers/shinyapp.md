---
title: Serving a ShinyApp
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 200
date: "2023-05-01T00:00:00Z"
menu:
  containers:
      name: Serving a ShinyApp
---

In this section of the workshop, we will take a completed ShinyApp and create a Docker image for it by writing a Dockerfile. We will then run the container to make sure that the ShinyApp is working as expected. Once we know the container is working, we will create a GitHub repository for our Shiny code and Dockerfile.

To complete this section on your own, you will need:

1. Docker Desktop installed on your machine
    The installers and installation instructions can be found here: https://docs.docker.com/engine/install/

2. A GitHub account
    If you don't have one already you can make one here: https://github.com/

3. A copy of the Shiny code
    You can clone a copy of the code to your local machine using `git clone https://github.com/uvarc/chickweight`
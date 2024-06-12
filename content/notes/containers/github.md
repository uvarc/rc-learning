---
title: Version Control
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 300
date: "2023-05-01T00:00:00Z"
menu:
  containers:
      name: Version Control
---

Tools like Git and GitHub are great keeping track of changes in our app. They also allow for easy collaboration and sharing of code.

Another great functionality of GitHub is GitHub Actions (GHA). GHA detect updates pushed to your repository and can kick off workflows automatically. For example, if we make updates to our Shiny code and push them to our repository, GHA can automatically rebuild our Docker image and push the new image to GHCR. This means we don't have to manually go through the whole rebuild+push process ourselves!

To take advantage of this, we will need to:

1. Create a new GitHub repository for our Shiny App

2. Add the Dockerfile to the GitHub repo.

3. Set up a workflow for rebuilding the container whenever there is a new commit.
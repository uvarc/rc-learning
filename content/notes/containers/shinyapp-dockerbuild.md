---
title: Building the App Image
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 220

menu:
  containers:
      parent: Serving a ShinyApp
---

Now that we've written the Dockerfile, it's time to build our image! To do that, we will use the `docker build` command.

In the same directory that your Dockerfile is in, you can run the following:

```
docker build -t <dockerhub-username>/<image-name>:<tag> .
```

- `-t` lets us specify the tag name for this image.

- `dockerhub-username` is pretty self-explanatory.

- `image-name` is the name for our image. It will also be the name of the image repository in DockerHub.

- `tag` can be a version number or other identifying label to help you differentiate this image from previous iterations. If you leave this blank, the tag name will default to `latest`.


I will be calling this image `cagancayco/chickweight:latest`. Simply replace `cagancayco` with your own username.
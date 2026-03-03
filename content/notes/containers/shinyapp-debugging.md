---
title: Running and Debugging
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 230
menu:
  containers:
      parent: Serving a ShinyApp
---

## Running the Container

Before we do anything else with our new Docker image, we want to make sure it runs correctly locally.

To run the container, we will use the following command.

```
docker run -p 8080:80 <dockerhub-username>/<image-name>:<tag>
```

With `-p 8080:80`, we are mapping port 80 of the container to port 8080 of our local machine. When we want to preview our app, we will go to [localhost:8080](localhost:8080) in our web browser. You don't have to use 8080, you can use another number if you want.

{{<figure src="/notes/containers/img/chickweight.png">}}

<br>

## Debugging

What if there's something wrong with the image for our Shiny app? How will we know? If there's an error, we'll see this message: 

{{<figure src="/notes/containers/img/error.png">}}

How do we go about debugging this? We'll need to take a peek into our app. We will use `docker run` like before, but this time we will run the container interactively.

```
docker run -it -p 8888:80 <dockerhub-username>/<image-name>:<tag> /bin/bash
```

- `-it` allows us to run the container interactively

- With `/bin/bash`, we are entering the container in a bash shell.

- We use port 8888 instead of 8080, because we need to restart Docker Desktop in order to use 8080 again.

<br>

When we try going to [localhost:8888](localhost:8888), the app isn't running yet. We had a command at the end of our Dockerfile to start up Shiny server and run our app, but we just replaced that with `/bin/bash`. So within our new container, let's run `/usr/bin/shiny-server.sh`.

Now we see that error message again when we reload the page in the browser. Hit **Ctrl + C** to stop running the app.

Navigate to `/var/log/shiny-server`. Here you will see log files--use the `cat` command to take a look at them. In this example, we are missing the `fresh` package from our Docker image. We need to add that to our Dockerfile and rebuild the image.

---
title: Running WebApps in a Container
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 120
date: "2023-05-01T00:00:00"
menu:
  containers:
      parent: Container Basics
---

Just like with the whalesay/cowsay program, we can run web apps in a container. Let's try with this static website example.

## 1. Pull the nginx image

```
> docker pull nginx
```

## 2. Run the nginx container

We can use -p to specify a custom port to connect to our container. In this case we are using port 8080.

```
> docker run -p 8080:80 nginx
```

We can see that the container is running when we go to [http://localhost:8080](http://localhost:8080) in the browser.

When you're done, you can use Ctrl + C to stop the container.
---
title: Pushing our Image to a Container Registry
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 240
date: "2023-05-01T00:00:00Z"
menu:
  containers:
      parent: Serving a ShinyApp
---

Once our container runs successfully, we can push it to a container registry so that others can pull the container. "Others" includes our Kubernetes cluster at UVA.

We have a few options for container registries. The two main options are DockerHub and GitHub Container Registry (ghcr.io).

For this workshop, we're going to use GHCR. The main benefit of using GHCR over DockerHub is that it is less restrictive with how many times your image can be pulled in an hour.

## Re-tagging our image

To use GHCR, we need to prepend our image name with `ghcr.io/<github-username>`. That means we will need to re-tag our image. We can do that with the `docker tag` command.

```
docker tag cagancayco/chickweight:latest ghcr.io/uvarc/chickweight
```

## Allowing Docker to push to GHCR

If this is your first time using GHCR, it is likely that you need to authorize your local Docker installation to push to GitHub.

1. Go to GitHub -> Settings -> Developer Settings -> Personal Access Tokens, or click [here](https://github.com/settings/tokens).

2. Generate new token with read/write permissions for packages.

3. Copy the token to the clipboard.

4. In your terminal, run

    ```
    docker login ghcr.io -u <github-username> -p <copied-token>
    ```

Now we can use `docker push` to push our container to GitHub!

```
docker push ghcr.io/uvarc/chickweight
```

## Set the package to public

By default, GHCR packages (or images) are set to private. We need them to be public in order for the Kubernetes cluster to see it. To change the visibility, do the following:

1. Go to Packages -> chickweight -> Package Settings

2. Click "change visibility" and follow the on-screen instructions.
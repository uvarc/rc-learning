---
title: GitHub Actions
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 340
menu:
  containers:
      parent: Version Control
---

GitHub Actions allow us to incorporate **Continuous Integration/Continuous Deployment** in our repository. We can automatically rebuild and redeploy our app whenever any changes are committed to the repo.

To add a workflow to your repo:

1. Create a `.github/workflows` directory.

2. Add the following .YAML file to the new directory, replacing the **IMAGE_NAME** and **SVC_NAME** with your image's name and the name that you want for your app on the Kubernetes cluster, respectively.

```
name: Container Build CICD

on:
  push:
    branches:
    - 'main'

env:
  # The preferred container registry
  REGISTRY: ghcr.io
  # The base org/image-name for the container
  IMAGE_NAME: uvarc/chickweight
  # A service name if mapping this to a k8s deployment
  SVC_NAME: chickWeight

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Set ENV
        run: echo "IMAGE_TAG=${GITHUB_REF#refs/*/}" >> $GITHUB_ENV
      -
        name: Set up QEMU
        uses: docker/setup-qemu-action@v1
      -
        name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v1
      -
        # GHCR require a GitHub username and a Personal Access Token with the right permissions.
        # These can be stored as repository secrets in the repo settings.
        name: Login to GHCR
        uses: docker/login-action@v1
        with:
          registry: ghcr.io
          username: ${{ secrets.GHCR_USERNAME }}
          password: ${{ secrets.GHCR_PAT }}
      -
        name: Build and push
        id: docker_build
        uses: docker/build-push-action@v2
        with:
          push: ${{ github.event_name != 'pull_request' }}
          tags: ghcr.io/${{ env.IMAGE_NAME }}:${{ env.IMAGE_TAG }}
          labels: ${{ steps.meta.outputs.labels }}
      -
        name: Image digest
        run: echo ${{ steps.docker_build.outputs.digest }}

      # Now update another repo so that ArgoCD can deploy the new version.
      # Note that the dispatch call simply curls a POST payload to another repository with JSON that you define.
      - name: Remote Dispatch
        run: |
          curl -X POST https://api.github.com/repos/uvarc/uvarc-services/dispatches \
            -H 'Accept: application/vnd.github.everest-preview+json' \
            -H "Authorization: token ${{ secrets.GHCR_PAT }}" \
            --data '{"event_type": "${{ env.IMAGE_NAME }} update to ${{ env.IMAGE_TAG }}", "client_payload": { "service": "${{ env.SVC_NAME }}", "version": "${{ env.IMAGE_TAG }}" }}'

```
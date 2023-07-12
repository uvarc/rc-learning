---
title: Common Git Commands
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 320

menu:
  containers:
      parent: Version Control
---

As we make changes to our Shiny app or Dockerfile, we will need to push the updates to our GitHub repository. Typically this is done with a `git add` + `git commit` + `git push`.

1. `git add <file(s)>`: **git add** is similar to packing a box. We are adding the files that we want to send up to our repository.

2. `git commit -m <message>`: With **git commit** we are creating a "packing slip" for our box of updated files. We can add a message that tells us and others why we're pushing these updates and what they do. Your message can be vague or informative--the choice is up to you.

3. `git push`: This command is doing the actual "shipping" of our files.

<br>

If we are working with collaborators, another useful command is `git pull`. This allows us to update our local repository with any updates our friends have pushed.
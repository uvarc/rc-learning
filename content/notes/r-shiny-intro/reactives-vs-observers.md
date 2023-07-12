---
title: "Reactives vs. Observers"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 380

menu:
  r-shiny-intro:
      parent: "Project 2: Reactives"
---

**Reactives**: calculate or cache a value, meant to be used as a variable, has a result

**Observers**: for side effects--runs some code when a dependency changes, doesn't yield a result, isn't used as input for other expressions, not assigned to a variable (e.g., updating a menu based on a selection)

<br>

{{< figure src="/notes/r-shiny-intro/img/dog.jpeg" >}}

> Imagine a delivery man rings your doorbell to deliver a package. You head to the front door to receive it. At the same time, your pet dog starts wildly barking at the door. You open the door and take the package from the delivery man.

In this example:

- The delivery man ringing the doorbell is a reactive input.

- You going to the front door is a conductor.

- Your dog barking is an observer. The dog is reacting to the doorbell ringing, but the barking has nothing to do with package delivery.

- You taking the package is a reactive endpoint (not a side effect).
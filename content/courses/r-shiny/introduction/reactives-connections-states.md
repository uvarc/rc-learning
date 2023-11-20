---
title: "Project 2: Reactives"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 340

menu:
  r-shiny:
      parent: Introduction to Shiny

---

## Reactives have connections

* Inputs have outward connections
* Observers have inward connections
* Conductors have both inward and outward connections

## Reactive States

***Reactives have 2 kinds of states***

1. **Their value**, e.g. numerical/text value, plot, image
2. **Their validity**--validated, invalidated

<br>

- Reactives are invalidated when their inputs change value. They become valid again when they recalculate or re-execute based on their input's new value.

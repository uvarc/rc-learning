---
title: What Is a Shiny App?
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 110

menu:
  r-shiny:
      parent: Introduction to Shiny

---

**Shiny is an R package for developing interactive web applications, or apps.** Shiny apps are just webpages!


Webpages are made up of two main components.

1. **HTML/CSS:** what your app looks like (the **form**)

2. **JavaScript:** what your app does (the **function**)

The difference between Shiny apps and regular webpages: **Shiny apps are powered by an R session.**

<br>

## Creating a Shiny app

To create a Shiny app, we need:

1. Regular R stuff: writing code to manipulate and analyze data, visualize with graphs, etc...

<br>

2. To create a user interface

3. To connect **1** to **2** with **reactive logic**

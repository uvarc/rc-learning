---
title: "Step 4: Display the data"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 440

menu:
  r-shiny-intro:
      parent: "Project 3: Plotting a Data Frame"
---

Write reactive logic to:

1. Display the head of the selected data frame in a table (only the response and explanatory variables)

2. Plot response versus explanatory in a scatterplot

You'll be using the `render` functions to create the displays. Is there a way you can create a common object for that can be used by both of the `render` functions? Hint: use `reactive()`

---
title: "Step 3: `response` and `explanatory`"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 430

menu:
  r-shiny-intro:
      parent: "Project 3: Plotting a Data Frame"
---

Write some reactive logic so that the appropriate choices appear for the `response` and `explanatory` input widgets

Hint: use `updateSelectInput()`. The variable names will be returned by `names(Raw_data())`. Should you use a `eventReactive()` or `observeEvent()` to accomplish this?


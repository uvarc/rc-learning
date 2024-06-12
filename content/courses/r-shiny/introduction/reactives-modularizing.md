---
title: "Modularizing Reactions"
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 360

menu:
  r-shiny:
      parent: Introduction to Shiny
---

## Adding an expression

This script still calculates the hypotenuse but splits the calculation into two parts

1. `C2 <- reactive(input$A^2 + input$B^2, label = "C2")`

2. `output$C <- renderText(sqrt(C2()))`

- Run `pythagorean-2.R` and change the input values.

- Stop the app

- Run `reactlogShow()`

- Reactives are lazy and only update when they need to

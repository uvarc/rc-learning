---
date: "2023-05-01"
title: "Modularizing Reactions"
weight: 360
---

## Adding an expression

This script still calculates the hypotenuse but splits the calculation into two parts

1. `C2 <- reactive(input$A^2 + input$B^2, label = "C2")`

2. `output$C <- renderText(sqrt(C2()))`

- Run `pythagorean-2.R` and change the input values.

- Stop the app

- Run `reactlogShow()`

- Reactives are lazy and only update when they need to

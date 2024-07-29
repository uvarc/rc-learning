---
date: "2023-05-01"
title: "reactlogShow()"
weight: 350
---

## Project 2: Pythagorean Theorem

This script calculates the hypotenuse of a right triangle using 
```
output$C <- renderText({
    sqrt(input$A^2 + input$B^2)
  })
```

- Run `pythagorean-1.R` and change the input values a few times.

- Stop the app

- Run `reactlogShow()` in the console (may need to install first)

A log will open in your browser


## reactlogShow() chart

- Click the forward arrow in the log window

- When `input$A` or `input$B`'s value changed, it became invalidated.

- This then invalidates `output$C`

- `output$C` becomes validated once it recalculates

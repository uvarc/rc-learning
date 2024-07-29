---
date: "2023-05-01"
title: "Reactives: An Example"
weight: 320
---

```
server <- function(input, output) {
    output$distPlot <- renderPlot({
        hist(rnorm(input$obs))
    })
}
```

In this example, `input$obs` is a reactive source, and `output$distPlot` is a reactive endpoint.

- Clicking or typing into input widgets will set some sort of value, or **reactive source**.

- A **reactive endpoint** is an object that appears in the app, like a plot, text, or values. Reactive endpoints re-execute when their reactive inputs change.

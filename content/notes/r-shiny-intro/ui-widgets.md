---
title: "Widgets"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: true # Show table of contents? true/false
type: docs  # Do not modify.
weight: 220

menu:
  r-shiny-intro:
      parent: "Project 1: User Interface"
---

- Go to `projects/project1-ui` and take a look at `UI_starting.R` and `Knight_bus.R`.

- Run `app.R`.

- **Widgets** are the different buttons and fields we see on a webpage. 

- We only see the inputs from `UI_starting.R` because nothing is connected to the outputs.

<br>

## Input Widgets

### The Shiny Widget Gallery

- You can try out and play with Shiny input widgets on RStudio's website.

- The website shows you how the values change when you modify the input widgets. The site will also show you the code you need to include the widget in your own app.

https://shiny.rstudio.com/gallery/widget-gallery.html

<br>

## Output Widgets

- Output widgets are similar to input widgets. However, output widgets require a `render` function to be visible in the app.

- Each output widget has its own corresponding `render` function.

{{< figure src="/notes/r-shiny-intro/img/output-widgets.png" >}}

<br>

## Playing with Widgets

Add some input widgets to `UI_starting.R`

- actionLink

- checkboxInput

- radioButtons

- textInput

Make sure the app still works with your changes!

<br>

## Connecting Inputs to Outputs

Add some reactive logic to `Knight_bus.R` so that the text output `felix` (`output$felix`) displays the selected choice from `annie` (`input$annie`)

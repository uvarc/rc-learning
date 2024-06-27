---
title: "Frameworks and Layouts"
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: true # Show table of contents? true/false
type: docs  # Do not modify.
weight: 205

menu:
  r-shiny:
      parent: Introduction to Shiny

---

- Replace `UI_starting.R` with `UI_fluid_page.R` in the `app.R`.

- Run the app

- Pretty ugly, right? Now try replacing `tagList` with `fluidPage`

<br>

## fluidPage()

A fluid page layout consists of rows which in turn contain columns

- Rows ensure that items appear on the same line as long as the browser is wide enough (hence the fluid)

- Columns define how much horizontal space elements should occupy. Horizontal space is defined by a 12-unit wide grid

- Adds some Bootstrap styling (framework for designing websites--developed by Twitter)

<br>

## fluidRow() and column()


Changing `tagList` to `fluidPage` didn't do much

Let's add some `fluidRows()` and `column()` functions to create this:

{{< figure src="/courses/r-shiny/introduction/img/fluid-wide.png" >}}

The solution is in `UI_language_soln.R`

## `fluidPage()` is responsive

- Try adjusting the size of your browser window.

- The text adjusts so that it all fits within a single window--no need to scroll left and right!

## Other Layouts

Check out the Shiny cheatsheet to see other types of layouts

https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

- `tabsetPanel()` + `tabPanel()`

- `sidebarLayout()` + `sidebarPanel()`/`mainPanel()`

- `splitLayout()`

- `wellPanel()`

- `navbarPage()`

- `navlistPanel()`

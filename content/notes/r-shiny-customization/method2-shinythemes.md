---
title: "shinythemes"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 210

menu:
  r-shiny-customization:
      parent: "Method 2: Shiny Themes"
---

1. `install.packages("shinythemes")`

2. Add `library(shinythemes)` to `app.R` or `ui.R`

3. Add the `theme` argument to your `fluidPage()`, `navbarPage()`, etc. Set `theme` with `shinytheme("<theme>")`

{{< figure src="/notes/r-shiny-customization/img/shinytheme-code.png">}}
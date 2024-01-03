---
title: "Start with Regular Shiny Code"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 940

menu:
  r-shiny:
      parent: Customization

---

1. Load bslib
2. Instantiate a `bs_theme` object
3. Set `theme` to your `bs_theme` object in the UI

```
library(shiny)
library(bslib) # step 1

my_theme <- bs_theme() # step 2

ui <- fluidPage(
  theme = my_theme, # step 3
  ...
)  

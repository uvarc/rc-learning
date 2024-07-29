---
date: "2023-05-01"
title: "Preview Your App"
weight: 450
---

1. Add `bs_themer()` to your server function

```
library(shiny)
library(bslib) # step 1

...

server <- function(input, output){
   bs_themer()
   
   ##cool reactive logic##
}

```

{{< figure src="/courses/r-shiny/customization/img/bslib.gif" caption="Adjust colors and fonts">}}

<br>

When you make changes in the Preview tool, code for updating your theme is printed to the console. Copy the updates to your Shiny app.

```
my_theme <- bs_theme()

# This line is copied from the console (may need to update theme name)             
my_theme <- bs_theme_update(my_theme, bg = "#4B3E23", fg = "#000000")

```

<br>

## How do I find out Bootstrap variable names?

- List of Bootstrap sass variables:

    [https://rstudio.github.io/bslib/articles/bs4-variables.html](https://rstudio.github.io/bslib/articles/bs4-variables.html)

- bslib commands:
    [https://rstudio.github.io/bslib/reference/index.html](https://rstudio.github.io/bslib/reference/index.html)
    

---
title: "Theming Plots"
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 960

menu:
  r-shiny:
      parent: Customization

---

The `thematic` package enables automatic theming of plots

```
library(thematic)

thematic_on()
onStop(thematic_off)
```

{{< figure src="/courses/r-shiny/customization/img/plot.png" caption="Great for dark mode!">}}

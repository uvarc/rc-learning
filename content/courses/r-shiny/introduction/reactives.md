---
title: "Project: Reactivity"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 310

menu:
  r-shiny:
      parent: Introduction to Shiny

---

We've already seen some examples with our `render` functions, but what exactly is **reactivity**?

## More Harry Potter Comparisons

Remember, the Knight Bus is our connection between regular R code (Muggle World) and the world of reactives (Wizarding World)

```
knight_bus <- function(input, output, session) {
    reactive code here!
}
```


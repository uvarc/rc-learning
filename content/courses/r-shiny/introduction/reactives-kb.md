---
title: "3 Types of KB Passengers"
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 330

menu:
  r-shiny:
      parent: Introduction to Shiny

---

{{< table >}}
| inputs | conductors | observers |
|  ---   |    ---     |    ---    |
| {{< figure src="/courses/r-shiny/introduction/img/input.png" >}} | {{< figure src="/courses/r-shiny/introduction/img/conductor.png" >}} | {{< figure src="/courses/r-shiny/introduction/img/observer.png" >}} |
| actionButton() | reactive() | observe() |
| selectInput()  | eventReactive() | observeEvent() |
| reactiveValues() | renderText() | textOutput() |
{{< /table >}}

- Inputs are reactive sources. Reactive endpoints are called observers because they observe changes to inputs.

- Conductors act like both sources and endpoints. They depend on inputs, but they also provide inputs to other reactives.

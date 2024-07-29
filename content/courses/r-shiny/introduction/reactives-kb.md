---
date: "2023-05-01"
title: "3 Types of KB Passengers"
weight: 330
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

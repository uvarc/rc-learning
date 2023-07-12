---
title: Creating a New Shiny App
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: true  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 40

menu:
  r-shiny-intro:
      parent: Intro to Shiny Apps
---

How do you create a new blank Shiny app?

## Old Faithful

Using **New File** -> **Shiny Web App...** creates a living, breathing Shiny App

{{< figure src="/notes/r-shiny-intro/img/old-faithful.png" >}}

## A Better Way to Do It

1. Open a new R script file.

2. Start typing `shinyapp` and press **Tab** to autocomplete. **This will expand into a "snippet" of code--the skeleton of a Shiny App.**
  
3. Save the file in the `sandbox` folder and run the app.

It's still a working Shiny app--it just doesn't do anything. Starting from the snippet is less error-prone than creating a new project and deleting the guts.

Stop an app by clicking the STOP button in the console.
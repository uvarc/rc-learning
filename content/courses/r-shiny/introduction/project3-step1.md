---
title: "Step 1: Reorganize the Code"
date: "2023-05-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false # Show table of contents? true/false
type: docs  # Do not modify.
weight: 410

menu:
  r-shiny:
      parent: Introduction to Shiny
---

Start with `app-0.R`. It's a working app-see for yourself!

Even though the app has very little functionality, the script is already crowded.

1. Pull out the UI and put it into a file `ui.R`. Do the same for `server`.

2. Source these into `app-0.R`.

3. Make sure the app still works!

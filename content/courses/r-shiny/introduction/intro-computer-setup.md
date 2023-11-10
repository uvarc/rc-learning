---
title: Computer Setup
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: true  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 55

menu:
  r-shiny:
      parent: Introduction to Shiny

---

## What You'll Need

1. **Development** server (just your computer!)

2. **Deployment** server (shinyapps.io, UVA Microservices)

## Development Server

This is where you will write and test the code for your Shiny app. You can interact with your app, but no one else can.

{{< figure src="/courses/r-shiny/introduction/img/development-server.jpg" >}}

You will spend most of your time on your development server.

### Set Up Your Development Server

**Option 1**: Clone or Download the GitHub Repository

1. Run `git clone https://github.com/uvarc/shiny-workshop` in the terminal
2. -OR- Go to https://github.com/uvarc/shiny-workshop and click green "Code" button, then click "Download ZIP"

<br>

**Option 2**: Create a New R Project (need Git installed for this option)

1. **File** > **New Project** > **Version Control** > **Git**
2. Repository URL: https://github.com/uvarc/shiny-workshop
3. **Create Project**

After downloading the code, run `packages-for-workshop.R`

<br>

## Deployment Server

Once you're ready to share your app, it's time to move it to a deployment server (i.e. deploy your app).

{{< figure src="/courses/r-shiny/introduction/img/deployment-server.jpg" >}}

Once deployed on the deployment server, your development server is no longer serving your app. This means any changes you make locally will need to be pushed to the deployment server before they're visible to the world.


### Set Up Your Deployment Server

Create a free account on shinyapps.io.

Can host 5 apps on shinyapps.io for free.

You can also host your apps on UVA servers: 
https://www.rc.virginia.edu/userinfo/microservices/

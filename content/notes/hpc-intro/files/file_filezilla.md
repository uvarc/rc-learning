---
title: "Transferring with Graphical Clients: macOS and Linux"
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 440

menu:
  hpc-intro:
    parent: Working with Files
---

## Filezilla

This illustration is from a Linux computer.  macOS is similar.

First click the Site Manager icon in the upper left.

{{< figure src="/notes/hpc-intro/img/Filezilla_ribbon.png" caption="Site Manager" alt="The FileZilla toolbar with the Site Manager icon in the top left circled." >}}

Select New Site.  Rename it.  Fill in the text boxes and dropdown.  Be sure to select SFTP in the Protocol box.  As for MobaXTerm, we recommend using a specific host name such `login.hpc.virginia.edu`.  Click `OK` to save and `Connect` to initiate the connection.  A multiple-pane window similar to that of MobaXTerm will open.

{{< figure src="/notes/hpc-intro/img/Filezilla_settings.png" caption="Site Manager" alt="The Site Manager, with options to create a new site and fill in its parameters." >}}

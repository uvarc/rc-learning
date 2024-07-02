---
title: Creating and Deleting Files
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 470

menu:
  hpc-intro:
    parent: Working with Files
---

There are three quick ways to work with files.

* The Open OnDemand File Explorer.  

* If logged in through FastX, you can use the "Caja" file manager.  It can be accessed through the filing-cabinet icon in the ribbon at the top, or via the Applications->System Tools menu.  Caja works very similarly to Windows Explorer and the Mac Finder, but is somewhat more limited. It should be simple to use.  The Open OnDemand file manager shows only one location at a time, whereas Caja, like Explorer or Finder, can open multiple windows. Note: you will not be allowed to do anything as "Administrator."

{{< info >}}
In Open OnDemand and Caja, rather than trying to navigate to your /scratch directory, use Go To (OOD) or Go->Location (Caja) and type the path `/scratch/mst3k`, using your own ID rather than `mst3k`.
{{< /info >}}


## Creating Files and Folders

* Open OnDemand: click the New File (file) or New Dir (folder) button and provide the name. You may also provide a path.
* In FastX with Caja: For a new file go to the File->Create Document menu. For a folder use File->Create Folder.
* In FastX you can use an editor such as `pluma`, which is accessible through the Applications->Accessories menu, using its File->New menu item.  You can then use the editor to add content to the file.

## Deleting Files and Folders
* In the Open OnDemand File Explorer, select the file or folder, then click the red Delete button.  It will request confirmation. 
* In the "Caja" file manager on FastX, right-click and Delete.  Since the space in your home directory is limited, we recommend not moving to Trash.


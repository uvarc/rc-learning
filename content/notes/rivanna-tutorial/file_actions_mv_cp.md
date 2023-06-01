---
title: Copy, Rename, and Move Files
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 562

menu:
  rivanna-tutorial:
    parent: Working with Files
---

## Renaming Files and Folders
* In the Open OnDemand File Explorer, click on Files on the Dashboard and click the file or folder you want to rename.  Click the Rename/Move button.
* In the "Caja" file manager on FastX, select the file or folder.  The combination of clicking on the icon and hitting the F2 key, should work on most keyboards as it does for Windows.  You can also right-click and choose Rename. 
* From the command line type `mv oldname newname`.

## Moving Files and Folders
* In the Open OnDemand File Explorer, use the Rename/Move button as for renaming, but provide the new path. 
* In the "Caja" file manager on FastX, if moving within the same parent folder, just drag the file or folder to the new location.  If moving between folders that do not share a parent, open another Caja window.  `Cut` the file or folder and `paste` to its new location.
* From the command line type `mv oldpath newpath` (that is, include the path and not just the file/folder name).

## Copying Files and Folders
* In the Open OnDemand File Explorer, use the `Copy` and `Paste` buttons.
* In the "Caja" file manager on FastX, open another Caja window and drag the icon of the file or folder between them.  Alternatively right-click and use the `copy` and `paste` menu items.
* From the command line type `mv oldpath newpath` (that is, include the path and not just the file/folder name).

---
title: "Adding Folders to the Globus Path"
type: docs
toc: true
date: 2023-02-02T00:00:00-05:00
weight: 30
menu:
    globus
---

When you first set up Globus, it only has access to certain folders of your local drive. You can add additional locations such as mapped network drives or external hard drives in the Globus Options/Preferences menu.

{{< video src="/notes/globus/videos/globus-external-drives.mp4" controls="yes" >}}

**Instructions**

1. Click Globus icon in toolbar
2. Click “Preferences” (Mac) or “Options” (Windows)
3. Click the Access tab
4. Click the “+”
5. Select the drive location and click “Open”
6. Navigate to the drive in the File Manager

**Tips for Navigating to Mapped Drives**
 
- Click the Up button in the File Manager to navigate to higher level directories
- On a Mac, mapped network drives will typically be located at `/Volumes/drive_name`
- In Windows, network drives will be mapped to a drive letter (e.g., C: or Z:)
- In Globus, `Z:\Drive_Name\my_files` becomes `/Z/Drive_Name/my_files`


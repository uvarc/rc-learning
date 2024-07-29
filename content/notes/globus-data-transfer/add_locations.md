---
date: "2023-02-02"
title: "Adding Folders to the Globus Path"
weight: 30
---

When you first set up Globus, it only has access to certain folders of your local drive. You can add additional locations such as mapped network drives or external hard drives in the Globus Options/Preferences menu.

**Instructions**

1. Right-click Globus icon in toolbar
2. Click “Preferences” (Mac) or “Options” (Windows)
3. Click the Access tab
4. Click the “+”
5. Select the drive location and click “Open”
6. Navigate to the drive in the File Manager

{{< figure src=/notes/globus-data-transfer/imgs/globus_connect_options.png caption="Select Options or Preferences from the Globus Connect menu." >}}

{{< figure src=/notes/globus-data-transfer/imgs/globus_connect_add_location.png caption="After clicking the +, navigate to the folder you wish to add." >}}

**Tips for Navigating to Mapped Drives**
 
- Click the Up button in the File Manager to navigate to higher level directories
- On a Mac, mapped network drives will typically be located at `/Volumes/drive_name`
- In Windows, network drives will be mapped to a drive letter (e.g., C: or Z:)
- In Globus, `Z:\Drive_Name\my_files` becomes `/Z/Drive_Name/my_files`


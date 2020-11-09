---
title: "Imaging Data Management with OMERO"
author: "Christina Gancayco"
highlight_style: "github"
date: 2020-10-22T00:00:00-05:00
toc: true  
type: article
draft: false
---
<img src="/images/ome-main-nav.svg" style="width:60%;height:60%"></img>

<p class="lead">
OMERO is image management software that allows you to organize, view, annotate, analyze, and 
share your data from a single centralized repository. With OMERO, you and your collaborators 
can access your images from any computer without having to download the images directly to 
your machine. The following tutorial will cover importing and managing data with OMERO, organizing 
and annotating images, and incorporating OMERO with your current image analysis pipelines.
</p>

# 1. How OMERO Works
OMERO is client-server software. The OMERO server, which is hosted on the UVa HPC cluster Rivanna, 
manages your images and data. Using client software like the OMERO desktop app, OMERO 
web application, or third-party software such as Fiji/ImageJ, you can communicate with the OMERO server and have images delivered to your 
computer for viewing or processing and analysis. This allows you to keep all your data in one 
centralized storage location, with OMERO as a thin layer on top to manage and organize all your different 
images and metadata.

Integrating OMERO into your image analysis pipeline only requires minor additions to your 
existing workflow, as seen in the chart below. The main difference is that **rather than managing the 
location of images and directly locating them yourself, you are allowing OMERO to manage the 
images and deliver them to your image processing software tools.** 

## Common Imaging Analysis Workflow

| Without OMERO | With OMERO Integration |
| ----          |        ----            |
| 1. Acquire images into acquisition directory on microscope computer | 1. Acquire images into acquisition directory on microscope computer|
| 2. Copy acquisition directory to permanent storage (hard drive, Rivanna, lab workstation) | 2. Copy acquisition directory to permanent storage (hard drive, Rivanna, lab workstation) **and import images to OMERO database**|
| 3. Preprocess images | 3. Preprocess images **pulled from OMERO** |
| 4. Save preprocessed images to new directory | **4. Import preprocessed images to OMERO** |
| 5. Analyze preprocessed images | 5. Analyze preprocessed images **pulled from OMERO** |
| 6. Save results to new file | 6. Save results **as annotations or attachments in OMERO** |

<br></br>
<img src="/images/omero-workflow.png" style="width:100%;height:100%"></img>

# 2. Getting Started


## Requesting an OMERO Account
OMERO accounts can be requested by emailing `hpc-support@virginia.edu`. You will need to 
provide the following information when submitting the request:

>**OMERO Account Request Info**
>
>1. Group name for your lab (e.g. Smith Lab, Brain Imaging Lab, etc)
>
>2. Names and UVa computing IDs for each lab member that needs access

Optionally, you can also indicate the type of permissions you would like members 
of your group to have. **By default, all group members will be able to view their own and others' 
data. Group members can make annotations on each other's data, but cannot modify or delete another 
member's data.**

For details on obtaining more restrictive or flexible permissions for your group members, 
please read the [Group/User Permissions section](#3-group-user-permissions).

## Logging into Your Account

Once you have an OMERO account you can log in and begin importing images*. You can log into your OMERO account on your computer using the **OMERO desktop app, OMERO.insight**, or you can 
log into OMERO online and access your images with **your web browser**. 

<!-- You can also run OMERO directly on Rivanna using FastX using either OMERO.insight *or* the 
web client. Since images are imported into centralized storage that is mounted directly on 
Rivanna, large images and movie files are rendered more quickly when using OMERO on Rivanna. -->

\***Note:** The OMERO web client is not capable of data import. To import images, use the 
desktop app OMERO.insight. <!--or the command-line interface.-->

### Logging in with OMERO.insight

1. Download the OMERO.insight app and follow installation instructions: [https://help.openmicroscopy.org/getting-started-5.html#installing](https://help.openmicroscopy.org/getting-started-5.html#installing)

2. Open OMERO.insight. You should see the following login screen.

    <img src=/images/omero-insight-login.png style="width:30%;height:30%"></img>
    
3. Click the wrench icon to open the **Servers** menu. You should see **localhost 4064** listed under 
**Server Address and Port**.

    <img src=/images/omero-wrench.png style="width:30%;height:30%"></img><img src=/images/omero-add-server1.png style="width:28.5%;height:28.5%"></img>

4. Click the **+** icon and enter **omero.hpc.virginia.edu** to add the UVa OMERO server. Click **Apply**.

    <img src=/images/omero-add-server2.png style="width:28.5%;height:28.5%"></img><img src=/images/omero-add-server3.png style="width:28.5%;height:28.5%"></img>
    
5. Enter your computing ID (e.g. `mst3k`) in the *Username* field. For password, enter the 
password emailed to you after your initial account request (you will be able to change this 
after logging in for the first time). Click **Login**.



### Logging in with OMERO.web

1. Go to [omero.hpc.virginia.edu](omero.hpc.virginia.edu).

2. Log in with your computing ID (e.g. `mst3k`) and the password that was emailed to you 
upon your initial account request.

> **Important things to note when using OMERO.web:**

>* OMERO.web cannot be used to import images

>* Tags cannot be created in OMERO.web

>* To use OMERO.web off-grounds, you must be connected to the UVa Anywhere VPN. Instructions for 
installing and using the VPN can be found on the [UVa ITS Service Now page](https://virginia.service-now.com/its?id=itsweb_kb_article&sys_id=f24e5cdfdb3acb804f32fb671d9619d0).


## Managing Your Account

After logging into OMERO for the first time, it is highly recommended that you change your 
password. You can manage your account settings using either OMERO.insight or OMERO.web.

### Using OMERO.insight

1. After logging into OMERO using the desktop app, click the **Administration** tab in the side 
menu.

    <img src=/images/omero-insight-admin.png style="width:70%;height:70%"></img>
    
2. Click the arrow next to your lab/group account name, then click your own name to open 
the user account menu.

    <img src=/images/omero-insight-pw.png style="width:70%;70%"></img>

3. Enter your new password in the **New Password** field and click the **Change Password** 
button.

    You can also change other settings in your account such as your default group account and 
    email address.

### Using OMERO.web

1. After logging in to OMERO in your web browser, click your name in the top right corner 
of the screen and then click **User settings**.

    <img src=/images/omero-web-usrsettings.png style="width:50%;height:50%"></img>

2. Click the **Change Password** button. Enter your current password and your desired new 
password. Click **OK** when complete.

    <img src=/images/omero-web-pw.png style="width:50%; height:50%"></img>

<!-- ### Using OMERO on Rivanna -->
<!-- ### Installing the OMERO Command-Line Interface -->

# 3. Group/User Permissions

OMERO users can have one of two user roles, **Group owner** or **Group member**.

As a Group owner of a lab/group account, you can edit the permissions of other 
users in your group. This can be done from the desktop app OMERO.insight.

1. In the **Administration** tab of the sidebar menu, click the name of your group. This 
will open the Group settings menu.

    <img src=/images/omero-group.png style="width:70%;height:70%"></img>

2. Select your desired permissions and click **Save**.

<br></br>

There are several different permissions settings, which are described below. By default, 
all group permissions are set to **Read-Annotate**.

| Permission Type     | Description | Example   |
| ----                | ----        | ----      |
| **Private**         | The Group owner can see all Group members and view their data, but cannot add annotations. Group members can only view and annotate their own data. This permissions setting does not allow for much collaboration. | A PI is a Group owner with student Group members. The PI is able to see each student's data, but the students are only able to see and annotate their own data. |
| **Read-Only**       | The Group owner can see all Group members' data and add annotations. Group members can see other members and their data. | A PI as Group owner can view and annotate student Group members' data such as marking regions of interest to the images. Students are able to view each other's data but not add annotations. |
| **Read-Annotate**   | The Group owner and Group members can view and annotate each other's data. No one can overwrite or remove data. | Several people are working together on data for a publication. As Group members, all are able to read and annotate each other's data. |
| **Read-Write**      | The Group owner and Group members can view, annotate, modify, and delete each other's data. | A group is working on data for a publication in a way that requires them to be able to modify each other's work. |

# 4. Importing Images
## Import Using the OMERO.insight

Images can be imported using the desktop app OMERO.insight. Images can be added to OMERO 
individually or in folders.

1. Click the Importer tool icon in the top toolbar. This will open the OMERO Importer tool.

    <img src="/images/omero-import-icon.png" style="width:40%;height:40%"></img>
    
2. In the left window pane of the Importer tool, navigate to the directory containing the images or folders of images 
that you want to import to the OMERO database. 

    <img src=/images/omero-import-leftpane.png style="width:60%;height:60%"></img>

3. Click on the files or folders containing the images you want to import. To select 
multiple items, hold down the Ctrl (Windows) or Command (Mac) key while clicking. Once you 
are finished making your selections, click the ">" button. This will open the Import Location 
menu.

    <img src=/images/omero-import-select.png style="width:60%;height:60%"></img>

4. In the Import Location menu, you can choose a Project, Dataset, or Screen\* to import your 
images to, allowing you to organize your images before they are uploaded to the OMERO database. 
If your desired Project/Dataset/Screen doesn't exist yet, you can click the **New** button 
to create a new one.

    <img src=/images/omero-import-location.png style="width:60%;height:60%"></img>


    \*Please visit the section on [Organization Techniques](#organization-techniques) to learn more 
about projects and screens.

5. Once your data and locations are selected, click the **Add to the Queue** button. This 
will close the Import Location menu and return you to the Importer tool. Click the 
**Import** button in the bottom right corner of the Importer tool to begin uploading your images.

    <img src=/images/omero-import-addqueue.png style="width:60%;height:60%"></img>
    
    <img src=/images/omero-import-startimport.png style="width:60%;height:60%"></img>

    You can monitor the progress of your import and begin viewing images in OMERO before the 
entire upload is complete.

    <img src=/images/omero-import-progress.png style="width:60%;height:60%"></img>

    Once imported, you will see any new Projects or Datasets you created in the left window pane. 
If you expand the Datasets, thumbnail versions of your images will appear in the center window 
pane of the OMERO.insight app.

    <img src=/images/omero-importedimages.png style="width:60%;height:60%"></img>


<!--### Import Using the Command Line-->

# 5. Viewing Images
OMERO is great for viewing images without having to download them or copy them locally to 
your machine. Viewing images using the desktop app and web app is largely similar; however, 
you can [define regions of interest (ROIs)](#defining-rois) with OMERO.insight.

## Viewing with OMERO.insight
As shown in the last step of the [Importing Images section](#importing-images), thumbnail 
versions of your images will appear in the center window pane of the OMERO.insight app. 
Clicking on an image will select it, and will present the image's metadata in the right 
window pane.

<img src=/images/omero-viewer-thumbnail.png style="width:60%;height:60%"></img>

<br></br>

To view the full-size image, double-click the corresponding thumbnail or click the **Full 
Viewer** button under the **General** tab of the right window pane. This will open the 
Image Viewer.

<img src=/images/omero-viewer-window.png style="width:60%;height:60%"></img>

## Viewing with OMERO.web
Like with OMERO.insight, thumbnails of your images will display in the center window pane 
of OMERO.web when you select a Dataset. Click on an image to select it and display the image's 
metadata.

<img src=/images/omero-viewer-web.png style="width:60%;height:60%"></img>

You can view the full-size version of your image by double-clicking the image or by 
clicking the **Full Viewer** button under the **General** tab of the right window pane. 
This will open the Image Viewer in a new tab in your browser. 

<img src=/images/omero-viewer-webwindow.png style="width:60%;height:60%"></img>

For more documentation on the OMERO image viewer, please visit: [https://www.openmicroscopy.org/omero/iviewer/](https://www.openmicroscopy.org/omero/iviewer/)

<!--## Zooming In

## Splitting Channels

## Viewing Movies-->

<!--# 6. Defining ROIs-->
 
# 6. Organization Techniques
Most researchers organize their data with some hierarchical structure. Often researchers 
use nested folders with descriptive names to indicate which experiment data belong to or 
to indicate when the data were collected. OMERO offers two schema for organizing your images 
in a similar fashion.

## Organizing Data By Project
Data can be organized using the following hierarchy, from top to bottom:

1. Project: The highest level organizational divider or "folder type" in OMERO. Projects 
can contain Datasets, but not individual images.

2. Dataset: The next organizational level after Project. Datasets contain images. You can store 
a Dataset within a Project, or the Dataset can exist on its own outside of any Projects. 
Multiple Datasets can be stored within a project.




## Organizing Data By Screens
Data can also be organized by Screens and Well Plates. The hierarchy for this organizational 
schema is (from highest to lowest level): 

1. Screens: The highest organizational level. This typically refers to some sort of phenotypic 
screening. Screens cannot contain images.

2. Plate: The next level of organization. This typically refers to a microarray well plate. 
Plates can be stored under a Screen, or can exist outside of a Screen.

3. Well: This usually refers to a single well from a well plate. Wells can only exist within 
a Plate (It is possible for wells to contain multiple images)

In order to import and organize data in this way, it is necessary to use Python and [OMERO's 
Python package](https://docs.openmicroscopy.org/omero/5.5.0/developers/Python.html). Advanced 
tutorials for writing your own Python scripts will be available on this workshop site in the near 
future!

<!--## Creating New Projects and Datasets-->




# 7. Annotating Data

Documentation on annotating data can be found here: [https://help.openmicroscopy.org/managing-data.html#annotating](https://help.openmicroscopy.org/managing-data.html#annotating)


# 8. Exporting Data
You can export data managed by OMERO to your local computer. You can export an image in its 
original file format or download it as a JPEG, PNG, TIFF, or OME-TIFF.

Documentation on exporting data can be found here: [https://help.openmicroscopy.org/export.html](https://help.openmicroscopy.org/export.html)


# 9. Image Analysis with OMERO
OMERO is compatible with a variety of third-party image processing software packages. Using these 
OMERO-software bindings, you can import images from OMERO to your software such as Fiji or Python, 
and then process and analyze them as usual. You can then export any results or preprocessed 
images back to OMERO.

Using OMERO to serve images to your analysis software has many benefits over more traditional methods 
of reading imaging data. With OMERO, there is no need to download the images directly to your 
local machine. 

## ImageJ/Fiji
Images managed by OMERO can be imported using the ImageJ/Fiji plugin for OMERO. Detailed 
instructions for installing and using the plugin can be found in OMERO's online documentation: 
[https://help.openmicroscopy.org/imagej.html](https://help.openmicroscopy.org/imagej.html).

## MATLAB and Python
You can install packages to connect to OMERO with MATLAB or Python. These packages include 
functions for connecting to the OMERO server and reading and exporting data.

 
OMERO's online documentation for the OMERO MATLAB language bindings can be found here: 
[https://docs.openmicroscopy.org/omero/5.5.0/developers/Matlab.html](https://docs.openmicroscopy.org/omero/5.5.0/developers/Matlab.html)

More information on the OMERO Python language bindings can be found here: 
[https://docs.openmicroscopy.org/omero/5.5.0/developers/Python.html](https://docs.openmicroscopy.org/omero/5.5.0/developers/Python.html).

More in-depth tutorials and sample scripts will be available on our workshop site soon!


<!-- ## OMERO on Rivanna -->


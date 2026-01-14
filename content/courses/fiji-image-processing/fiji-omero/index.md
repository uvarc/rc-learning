---
title: Image Processing with Fiji and Omero
authors: [khs]
highlight_style: "github"
date: 2020-11-09T00:00:00-05:00
toc: true  
type: book
draft: false
weight: 200

---

## Introduction to OMERO

OMERO is an image management software package that allows you to organize, view, annotate, analyze, and share your data from a single centralized database. With OMERO, you and your collaborators can access your images from any computer without having to download the images directly to your computer.

In this chapter you will learn to view and manipulate images through the [Fiji
/ImageJ](https://fiji.sc/) software package.

For more details, review the [OMERO tutorial](/tutorials/omero-hands-on) or visit the Research Computing website describing the [UVA Omero Database Service](https://www.rc.virginia.edu/userinfo/omero/overview/).

---

## Installation of Fiji and the OMERO Plugin

1. Install the Fiji application on your computer as described in this [chapter](/courses/fiji-image-processing/introduction/).

2. Start Fiji and go to `Help` > `Update`. This starts the updater which looks for plugin updates online.

3. In the `ImageJ Updater` dialog window, click on `Manage Update Sites`. Ensure that the boxes for the following plugins are checked:

    * Java-8
    
    * Bio-Formats
    
    * OMERO 5.4
    

4. Click `Close`. This will take you back to the `ImageJ Updater` window.

5. Click `Apply Changes`.

6. Restart Fiji.

### Download the Example Scripts

To follow along, you can download the Jython scripts presented in this tutorial through [this link](/scripts/fiji/fiji-omero-scripts.zip).

### Check your OMERO Database Account

1. If you are accessing the OMERO Database from an off-Grounds location, you have to connect through a UVA Virtual Private Network (VPN).  Please follow these [instructions to set up your VPN](https://virginia.service-now.com/its?id=itsweb_kb_article&sys_id=f24e5cdfdb3acb804f32fb671d9619d0).

2. Open a web browser and go to http://omero.hpc.virginia.edu. Login to the OMERO web interface is described [here](/courses/omero/#logging-in-with-omeroweb).

    * **Username:** Your computing ID
    
    * **Password:** Your personal password
    
	If you are new to UVA's OMERO Database, a temporary password had been emailed to you.  

	**Please change your temporary password when you log in for the first time to the Omero database server as described in these [instructions](https://www.rc.virginia.edu/userinfo/omero/overview/#changing-your-omero-database-password).** 

3. You may be a member of multiple groups. For this workshop we want to specify  `omero-demo ` as the default group.
 
If you cannot log in, please submit a [support request](https://www.rc.virginia.edu/form/support-request/).  Select **Omero Image Analysis** as Support Category.  

---

# Working with the OMERO Web Client and Desktop Applications

OMERO provides a group of client software to work with your imaging data.

* [OMERO.web Client](https://docs.openmicroscopy.org/omero/5.6.1/users/clients-overview.html#omero-web): 

	OMERO.web is a web-based client for OMERO.  At UVA you can use it by connecting to http://omero.hpc.virginia.edu. The Web client can be used to view images and metadata, and manage image tags, results, annotations, and attachments. It cannot be used to import data or measure regions-of-interest (see OMERO.insight).
	
	* View Data
	
	* Import Data
	
	* View Image Metadata
	
	* Manage Image Tags
	
	* Manage Results and Annotations

* [OMERO.insight](https://docs.openmicroscopy.org/omero/5.6.1/users/clients-overview.html#omero-insight): 

	The two main additional features of OMERO.insight which are not available yet for OMERO.web are:

	* The Measurement Tool, a sub-application of ImageViewer that enables size and intensity measurements of defined regions-of-interest (ROIs), and 
	
	* image import.

* [OMERO.importer](https://docs.openmicroscopy.org/omero/5.6.1/users/clients-overview.html#omero-importer): 

	The OMERO Importer is part of the OMERO Desktop client but can also be run as a standalone application.

* [OMERO.cli](https://docs.openmicroscopy.org/omero/5.6.1/users/clients-overview.html#omero-cli): 

	The OMERO.cli tools provide a set of administration and advanced command line tools written in Python.


For this workshop we will be using the OMERO Web client.


# Projects, Datasets, Screens, Plates and Images

For this workshop we will be using prepared image sets and also upload new image data to your accounts.  Omero organizes images in nested hierarchies of `Projects` and `Datasets` or `Screens` and `Plates`. The hierarchical nature is visualized as tree structures in the OMERO.web and OMERO.insight clients.  

Images can be linked to individual projects, datasets, screens, or plates. A given image can be linked to multiple folders, allowing you to logically assign the same image to different folders without copying it. You can think of it like smart folders or symbolic links.

In addition, each group has a folder `Orphaned Images` that contains images that have not been linked to any projects, datasets, screens, or plates.

**Note:**
Each project, dataset, screen, plate, and image has a __unique numerical ID__ that unambiguously identifies the data artifact.  We use these numerical IDs rather than image, folder, or file names to get access to these elements.

### Sample Data

Sample data has been added to the `Fiji Omero Workshop` project.  Inside the blue project folder you will find several dataset subfolders. 

* Group: **omero-demo** (Group ID: `53`)

* Project: **Fiji Omero Workshop** (Project ID: `130`)

**Make note of the Group ID and Project ID.**

### Your Personal Projects and Datasets

Let's create a new project and dataset through the OMERO web client. 

1. In the left `Explore` panel, click on `omero-demo` label and switch to `All Members`.  Now you can see all data shared within the `omero-demo` group.

2. Right-click on the avatar with your name (or the "All Members" label). You can also just right click folder icon labeled `Fiji Omero Workshop`. In the popup menu, select `Create new` > `Project`. 

2. Name the new Project `<YOUR_UID>_workshop`, for example `mst3k_workshop`. 
    
3. Click on the blue folder icon of your new project and take note of the `Project ID`. We will need this to direct Fiji where to load data from or save data to.  No right click on your blue project folder icon and create a new dataset.  Right-click on the dataset icon and take note of the `Dataset ID`.

After the project is generated, your user interface should look like this:
    ![](/courses/fiji-omero/fiji-omero-datasetid.png)

---

# Interactive Use of Fiji and OMERO

### Uploading Images with the OMERO.insight client

Here we will demonstrate, the upload of a set of image files via the OMERO.insight client. The import process is described in our [OMERO.insight Import Tutorial](/lesson/omero-hands-on). 

After the upload, the files are located in the following Project and Dataset: 

* **Project:** Fiji Omero Workshop (Project ID: `130`)

* **Dataset:** HeLa Cells (Dataset ID: `265`)

**Note that images cannot be uploaded with the web-based OMERO.web client.**

---

### Uploading Images with Fiji

Images that have been opened and are displayed in the Fiji graphical user interface can be uploaded to OMERO using the `File` > `Export` > `OMERO...` command. The naming of the tool can be confusing -- it's all a matter of perspective: **Export from Fiji** equates to an **Import in OMERO**.  To minimize possible confusion we avoid these terms for the purpose of this workshop and refer to **upload** for any process that sends data to OMERO and **download** for any process that retrieves data from OMERO.

Before you begin, you need to know the dataset ID that the image should be linked to in OMERO. You can use the OMERO.web, OMERO.insight, or OMERO.cli tools to look up an appropriate dataset ID for this. **Keep in mind that you can upload images only to those Projects and Datasets that you own.**

* If you choose a dataset ID -1, the image will be added to the `Orphaned Images` in your default OMERO group.  For this workshop, we have chosen the `omero-demo` group as default. 

* Alternatively, you can export the image to a specific dataset in your default group, you just need to provide the dataset ID when executing the upload command.

**Note: Image uploads via the built-in OMERO Export function is limited to your default group.** This is not an issue if you belong to a single group.  If you are a member of multiple groups, you can change your default group via the OMERO.web client. 

### Exercises

**Export RGB image**

1. File > Open Sample > Leaf (36k)

2. File > Export > OMERO...

	Since we chose `Dataset: -1`, the uploaded image will end up in `Orphaned Images` for your group. 

3. Go to OMERO webclient (http://omero.hpc.virginia.edu) and look for the uploaded image in `Orphaned Images`.

	<img src="./leaf.png" style="float:left;width:40%;height:auto">
	<img src="./fiji-omero-new-export.png" style="width:40%;height:auto">


<br>

**Export image stack to a specific dataset**

1. In the OMERO webclient, create a new dataset under your personal workshop project. **Make note of the dataset ID.**

2. File > Open Samples > T1 Head (2.4M, 16-bit)

3. File > Export > OMERO...

**Question:** What happens when you repeatedly upload the same image to Omero?

---

### Uploading Results with Fiji

1. In the OMERO.web client, make note of a dataset ID where you would like to upload a new image and associated results file to.  You make pick the dataset ID from the previous exercise.

2. In Fiji, go to `File` > `Open Samples` > `Blobs (25K)`

3. Let's apply some basic filtering and simple image segmentation to identify all cellular outlines in the image:

	a. Go to `Process` > `Filter` > `Median`.  In the popup dialog enter a `Radius` of  `3.0` and click `OK`.  This will smooth out some of the image's intrinsic noise without degrading the object outlines.

	b. Go to `Image` > `Adjust Threshold`.  In the popup dialog choose the `Default` thresholding algorithm, uncheck the `Dark Background` box and click `Apply`. The image should have been converted to a binary mask with white objects ona black background.
			<img src="./fiji-omero-setthreshold.png" style="float:left;width:47%;height:auto">
			<img src="./fiji-omero-blobs-thresholded.png" style="width:40%;height:auto">
	
	
	c. Go to `Analyze` > `Set Measurements...`. In the popup dialog specify the parameters as shown in this screenshot. Click `OK`.

	d. Go to `Analyze` > `Analyze Particles` and set up the parameters as shown. Click `OK`.
			<img src="./fiji-omero-setmeasurements.png" style="float:left;width:40%;height:auto">
			<img src="./fiji-omero-analyzeparticles.png" style="width:50%;height:auto">

    e. These steps should create a `Results` and a `Summary` table.

3. Go to `File` > `Export` > `OMERO...`.  Enter the dataset ID you chose under step 1 and click `OK`.

{{< figure src="/courses/fiji-omero/fiji-omero-blobs-export.png" >}}
	
4. Click on the `Results` table window and go to `File` > `Save As`.  Save the file as `Results.csv` on your computer.  Repeat the same for the `Summary` table.  

5. **Upload the results files:** Go to the OMERO web client, open the Dataset folder that you had chosen for the image upload. On the right side of the webpage, expand the `Attachments` pane and click on the `+` icon. In the popup dialog, click `Choose File` and select the saved csv files. Click `Accept`.  

Now you have analyzed the image and uploaded the image as well as all results to the OMERO dataset.


---

# Scripting

Fiji provides convenient programming wrappers for the Fiji/ImageJ and OMERO functions that allow you to develop your scripts in a variety of programming languages:

* ImageJ macro language: simple, slow, not very versatile
* Jython: Python syntax with a few limitations, easy to learn, very versatile
* BeanShell: Syntax similar to Java, versatile
* Several others…

Fiji provides a richer programming environment than ImageJ and it is recommended to use Fiji instead of ImageJ for any script development.  Our [Fiji/ImageJ: Script development for Image Processing](/tutorials/fiji-scripting/) tutorial provides a more general introduction to this topic.

### Example Scripts
To follow along, you can download the Jython scripts presented in this tutorial through **[this link](/scripts/fiji/fiji-omero-scripts.zip)**.


### The Script Editor {#script-editor-id}

We'll be using the built-in **Script Editor** in Fiji to run our scripts.  To start the script editor in Fiji go to menu `File` > `New` > `Script…`.

{{< figure src="/courses/fiji-omero/fiji-script-editor.png" >}}

* The top pane provides the editor. Multiple scripts can be open at the same time and will show up as separate tabs.
* The bottom pane shows output (e.g. produced by print statements) and any errors encountered during script execution.

**Script editor menus:**

+ **File:** 		Load, save scripts
+ **Edit:**		Similar to word processor functionality (Copy, Paste, etc.)
+ **Language:**	Choose language your script is written in with syntax highlighting
+ **Templates:**	Example scripts
+ **Tools:**		Access to source code of a class in the ImageJ/Fiji package
+ **Tabs:**		Navigation aid in script

<br>

### The Macro Recorder {#macro-recorder-id}
The Macro Recorder logs all commands entered through the Fiji graphical user interface (GUI). It is useful to convert these GUI actions into script commands.


* In the Fiji menu, go to `Plugins` > `Macros…` > `Record`.
+ In the `Record` drop-down menu, select `BeanShell`.
+ Clicking the `Create` button copies the code to a new script in the [Script Editor](#script-editor-id).

<br>
 
### The Console Window {#console-id}
In the Fiji menu, go to `Window` > `Console`.

+ The Console window shows the output and logging information produced by running plugins and scripts.

<br>

---

### Connecting to OMERO
In order to get full access to OMERO's programming interface, we will now use a more advanced approach to establish an authenticated connection with the OMERO database.  We need instances of three classes: `LoginCredientials`, `SimpleLogger`, and `Gateway`.  The central one for the configuration is `LoginCredentials` which has to be initialized with user specific credentials and database host information.

Our script would not be very useful or secure if we had to hardcode these values. Fortunately we can use the [SciJava@Parameter](https://imagej.net/Script_Parameters) annotation to prompt the script user for the relevant information:
```python
#@ String (label="Omero User") username
#@ String (label="Omero Password", style="password") password
#@ String (label="Omero Server", value="omero.hpc.virginia.edu") server
#@ Integer (label="Omero Port", value=4064) port
```

These four lines at the top of our scripts are sufficient to create a dialog window that prompts the user for information that will be populated in the `username`, `password`, `host`, and `port` variables. With these variables in place we can now establish a connection to the OMERO database server.

```python
cred = LoginCredentials()
if group_id != -1:
    cred.setGroupID(group_id)
cred.getServer().setHostname(server)
cred.getServer().setPort(port)
cred.getUser().setUsername(username)
cred.getUser().setPassword(password)
simpleLogger = SimpleLogger()
gateway = Gateway(simpleLogger)
e = gateway.connect(cred)
```

The return value of the `connect` method is stored as a boolean value in the variable `e`. If `e==True`, the connection was established; if `e==False`, the connection failed.  We can reuse this code block for most of our OMERO scripts.

It is very important to close the connection to the database at the end of your script, like this:
```python
gateway.disconnect()
```

---

### Getting the Basic OMERO Dataset Info

OMERO organizes users in groups. Each user can be a member of multiple groups. Images are organized in Projects and Datasets, or in Screens and Plates. The following script, `Omero_Info.py`, connects a user to a remote OMERO instance and shows a list of:

* the groups that the user belongs to and the associated group ID. This ID is important when you want to access images stored for a particular group;
* the projects and datasets for a particular group (specified via unique group ID);
* and a list of images, organized by project and dataset, that the user has access to in a particular group.

The following script, `Omero_info.py` establishes a connection to the OMERO database and outputs your OMERO group memberships, as well as a list of all of your projects, datasets, and images. The code contains separate functions to connect to the database, retrieve information from the database, and parse the data into a set of tables.  If you're just starting with programming, you may find it helpful to work through our [Fiji Scripting](/tutorials/fiji-scripting/) and other tutorials on our [learning portal](/categories/).

(Click on the black triangle next to **View** to take a look at the script.)

<details>
<summary>View <code>Omero_Info.py</code> script</summary>

{{< highlight python "linenos=table,linenostart=1" >}}
#@ String (label="Omero User") username
#@ String (label="Omero Password", style="password") password
#@ String (label="Omero Server", value="omero.hpc.virginia.edu") server
#@ Integer (label="Omero Port", value=4064) port
#@ Integer (label="Omero Group ID", min=-1, value=-1) group_id


# Basic Java and ImageJ dependencies
from ij.measure import ResultsTable
from java.lang import Long
from java.lang import String
from java.util import ArrayList

# Omero dependencies
import omero
from omero.gateway import Gateway
from omero.gateway import LoginCredentials
from omero.gateway import SecurityContext
from omero.gateway.exception import DSAccessException
from omero.gateway.exception import DSOutOfServiceException
from omero.gateway.facility import BrowseFacility
from omero.log import SimpleLogger


def connect(group_id, username, password, server, port):    
    """Omero Connect with credentials and simpleLogger"""
    cred = LoginCredentials()
    if group_id != -1:
    	cred.setGroupID(group_id)
    cred.getServer().setHostname(server)
    cred.getServer().setPort(port)
    cred.getUser().setUsername(username)
    cred.getUser().setPassword(password)
    simpleLogger = SimpleLogger()
    gateway = Gateway(simpleLogger)
    e = gateway.connect(cred)
    return gateway


def get_groups(gateway):
    """Retrieves the groups for the user"""
    currentGroupId = gateway.getLoggedInUser().getGroupId()
    ctx = SecurityContext(currentGroupId)
    adminService = gateway.getAdminService(ctx, True)
    uid = adminService.lookupExperimenter(username)
    groups = []
    for g in sorted(adminService.getMemberOfGroupIds(uid)):
        groupname = str(adminService.getGroup(g).getName().getValue())
        groups.append({
            'Id': g,
            'Name': groupname,
        })
        if g == currentGroupId:
            currentGroup = groupname     
    return groups, currentGroup


def get_projects_datasets(gateway):
    """Retrieves the projects and datasets for the user"""
    results = []
    proj_dict = {}
    ds_dict = {}
    groupid = gateway.getLoggedInUser().getGroupId()
    ctx = SecurityContext(groupid)
    containerService = gateway.getPojosService(ctx)

    # Read datasets in all projects
    projects = containerService.loadContainerHierarchy("Project", None, None) # allowed: 'Project", "Dataset", "Screen", "Plate"
    for p in projects:                # omero.model.ProjectI
        p_id = p.getId().getValue()
        p_name = p.getName().getValue()
        proj_dict[p_id] = p_name
        for d in p.linkedDatasetList():
            ds_id = d.getId().getValue()
            ds_name = d.getName().getValue()
            results.append({
                'Project Id': p_id,
                'Project Name': p_name,
                'Dataset Id': ds_id,
                'Dataset Name': ds_name,
                'Group Id': groupid,
            })
            ds_dict[ds_id] = ds_name     

    # read datasets not linked to any project 
    ds_in_proj = [p['Dataset Id'] for p in results]
    ds = containerService.loadContainerHierarchy("Dataset", None, None)
    for d in ds:                # omero.model.ProjectI
        ds_id = d.getId().getValue()
        ds_name = d.getName().getValue()
        if ds_id not in ds_in_proj:
            ds_dict[ds_id] = ds_name
            results.append({
                'Project Id': '--',
                'Project Name': '--',
                'Dataset Id': ds_id,
                'Dataset Name': ds_name,
                'Group Id': groupid,
            })
    return results, proj_dict, ds_dict         


def get_images(gateway, datasets, orphaned=True):
    """Return all image ids and image names for provided dataset ids"""
    browse = gateway.getFacility(BrowseFacility)
    experimenter = gateway.getLoggedInUser()
    ctx = SecurityContext(experimenter.getGroupId())
    images = []
    for dataset_id in datasets:
        ids = ArrayList(1)
        ids.add(Long(dataset_id))
        j = browse.getImagesForDatasets(ctx, ids).iterator()
        while j.hasNext():
            image = j.next()
            images.append({
                'Image Id': String.valueOf(image.getId()),
                'Image Name': image.getName(),
                'Dataset Id': dataset_id,
                'Dataset Name': datasets[dataset_id],
            })
    if orphaned:
        orphans = browse.getOrphanedImages(ctx, ctx.getExperimenter()) # need to pass user id (long)
        for image in orphans:
            images.append({
                'Image Id': String.valueOf(image.getId()),
                'Image Name': image.getName(),
                'Dataset Id': -1,
                'Dataset Name': '<Orphaned>',
            })	
    return images


def show_as_table(title, data, order=[]):
    """Helper function to display group and data information as a ResultsTable"""
    table = ResultsTable()
    for d in data:
        table.incrementCounter()
        order = [k for k in order]
        order.extend([k for k in d.keys() if not d in order])
        for k in order:
        	table.addValue(k, d[k])
    table.show(title)


# Main code
gateway = connect(group_id, username, password, server, port)

groups, current_group = get_groups(gateway)
show_as_table("My Groups", groups, order=['Id', 'Name'])

all_data,_,datasets = get_projects_datasets(gateway)
show_as_table("Projects and Datasets - Group: %s" % current_group, all_data, order=['Group Id', 'Dataset Id', 'Dataset Name', 'Project Name', 'Project Id'])

gateway.disconnect()	
{{< /highlight >}}
</details>


---

### Downloading Images from the OMERO database

Let's try to download images from the database through a script.  The OMERO plugin provides simple download (aka import to Fiji) functionality to achieve this.

1. In the OMERO web interface, click on any image in the `Fiji Omero Workshop` project or your `xxx_workshop` project/dataset and note the Image ID displayed in the sidebar on the right side of the webpage. **Image retrieval relies on these unique image identifiers**.

2. Go back to the Fiji Script Editor and open the `Omero_Image_Download.py` script.
	
3. Run the script. A dialog window will open; enter these values:

    * **Omero User:** Your computing ID
    * **Omero Password:** Your OMERO Password
    * **Omero Server:** omero.hpc.virginia.edu
    * **Omero Port:** 4064
    * **Omero Group ID:** Enter `53` as ID for the `omero-demo` group, or use `-1` to use your default group
    * **Image ID:** Enter the ID for an image that is part of your `xxx_workshop` dataset, or use `11980` from the example files. 

The script consists of the these core blocks:

* Lines 1-6 define user input to connect to OMERO.
* Lines 12-20 define a `command` variable that specifies OMERO connection and image parameters.
* Line 21 executes the OMERO importer plugin that retrieves the image.

{{< highlight python "linenos=table,linenostart=1" >}}
# @ String (label="Omero User") user
# @ String (label="Omero Password", style="password") pwd
# @ String (label="Omero Server", value="omero.hpc.virginia.edu") server
# @ Integer (label="Omero Port", value=4064) port
# @ Integer (label="Omero Group ID", min=-1, value=53) omero_group_id
# @ Integer (label="Image ID", value=2014) image_id

from ij import IJ
from loci.plugins.in import ImporterOptions
	 
# Main code
command="location=[OMERO] open=[omero:"
command+="server=%s\n" % server
command+="user=%s\n" % user
command+="port=%s\n" % port
command+="pass=%s\n" % pwd
if omero_group_id > -1:
	command+="groupID=%s\n" % omero_group_id
command+="iid=%s] " % image_id
command+="windowless=true view=\'%s\' " % ImporterOptions.VIEW_HYPERSTACK
IJ.runPlugIn("loci.plugins.LociImporter", command)
{{< / highlight >}}

---

### Uploading Images to the OMERO database

Let's try to upload an image from Fiji to OMERO.

1. Go back to Fiji and then to `File` > `Open Samples` > `Blobs`.

2. Go back to the Fiji Script Editor and open the `Omero_Image_Upload.py file`.

	{{< highlight python "linenos=table,linenostart=1" >}}
	from ij import IJ

	imp = IJ.getImage()
	IJ.run(imp, "OMERO... ", "")
	{{< /highlight >}}

3. Run the script. The **Export to OMERO** dialog window will open. Enter the following values:

    * **Server**: omero.hpc.virginia.edu
    
    * **Port:** 4064
    
    * **User:** Your computing ID
    
    * **Password:** Your OMERO password
    
    * **OMERO Dataset ID:** Enter the ___ID___ for the `xxx_workshop` dataset that you created in the OMERO web interface. 
    
    * Check the **Upload new image** box.  Leave the other boxes unchecked.

    Click `OK`.

    If you see an error, make sure you entered the correct password and Dataset ID.  **Note: you have to use your own project/dataset.** 

4. Go to the OMERO website and refresh the page. Double-click on your `xxx_workshop` dataset icon to expand it. You should see the blobs.gif image.

---

### Creating Key:Value Annotations

{{< figure src="/courses/fiji-omero/fiji-omero-keyvalue.png" >}}
OMERO allows you to link other pieces of information to your Project, Dataset, Screen, Plate or Image objects. This additional information is displayed on the right side in the OMERO web client, labeled under the `General` tab as `Image Details`, `Tags`, `Key-Value Pairs`, `Tables`, `Attachments`, `Comments`, and `Ratings`. In addition, there is the `Acquisition` tab that provides metadata information that was automatically extracted from the image file headers during import.  

For the remainder of this workshop, we will focus on `Key-Value` pairs and `Attachments`.  The key-value pairs are implemented as a dictionary (or HashMaps) that can be used to annotate individual images or whole datasets/project or plates/screens with additional information. Such information may include experimental conditions etc.. 

Let's look at an example:

1. In the OMERO webclient, expand the `Fiji Omero Workshop` project folder and the `Sample Data` dataset folder inside it. 

2. Click on the `blobs.gif` image.  In the general tab, you will see three entries under the `Key-Value` group. (You may have to click on the triangle next to the label to expand the tab and see it).

The values displayed are not particular meaningful, but they illustrate the concept. You can create and modify annotations interactively through the OMERO client.  In addition, you can manipulate key-value pairs (as well as other annotation categories) through Fiji scripts.
   
<details>
<summary>View <code>Omero_Map_Annotation.py</code> script</summary>

{{< highlight python "linenos=table" >}}
#@ String (label="Omero User") username
#@ String (label="Omero Password", style="password") password
#@ String (label="Omero Server", value="omero.hpc.virginia.edu") server
#@ Integer (label="Omero Port", value=4064) port
#@ Integer (label="Omero Group ID", min=-1, value=-1) group_id
#@ String (label="Target", value="Image", choices = ["Image", "Dataset", "Project"]) target_type
#@ Integer (label="Target ID", min=-1, value=-1) target_id

# Basic Java and ImageJ dependencies
from ij.measure import ResultsTable
from java.lang import Double
from java.util import ArrayList
from ij import IJ
from ij.plugin.frame import RoiManager
from ij.measure import ResultsTable

# Omero dependencies
import omero
from omero.log import SimpleLogger
from omero.gateway import Gateway
from omero.gateway import LoginCredentials
from omero.gateway import SecurityContext
from omero.gateway.model import ExperimenterData;

from omero.gateway.facility import DataManagerFacility
from omero.gateway.model import MapAnnotationData
from omero.gateway.model import ProjectData
from omero.gateway.model import DatasetData
from omero.gateway.model import ImageData
from omero.model import NamedValue
from omero.model import ProjectDatasetLinkI
from omero.model import ProjectI
from omero.model import DatasetI
from omero.model import ImageI


def connect(group_id, username, password, server, port):    
    """Omero Connect with credentials and simpleLogger"""
    cred = LoginCredentials()
    if group_id != -1:
    	cred.setGroupID(group_id)
    cred.getServer().setHostname(server)
    cred.getServer().setPort(port)
    cred.getUser().setUsername(username)
    cred.getUser().setPassword(password)
    simpleLogger = SimpleLogger()
    gateway = Gateway(simpleLogger)
    e = gateway.connect(cred)
    return gateway


def create_map_annotation(ctx, annotation, target_id, target_type="Project"):
    """Creates a map annotation, uploads it to Omero, and links it to target object"""
    # populate new MapAnnotationData object with dictionary
    result = ArrayList()
    for item in annotation:
        # add key:value pairs; both need to be strings
        result.add(NamedValue(str(item), str(annotation[item])))
    data = MapAnnotationData()
    data.setContent(result);
    data.setDescription("Demo Example");

    # use the following namespace if you want the annotation to be editable in the webclient and insight
    data.setNameSpace(MapAnnotationData.NS_CLIENT_CREATED);
    dm = gateway.getFacility(DataManagerFacility);
    target_obj = None

    # use the appropriate target DataObject and attach the MapAnnotationData object to it
    if target_type == "Project":
        target_obj = ProjectData(ProjectI(target_id, False))
    elif target_type == "Dataset":	
        target_obj = DatasetData(DatasetI(target_id, False))
    elif target_type == "Image":	
        target_obj = ImageData(ImageI(target_id, False))
    result = dm.attachAnnotation(ctx, data, target_obj)
    return result

# Main code
gateway = connect(group_id, username, password, server, port)
currentGroupId = gateway.getLoggedInUser().getGroupId()
ctx = SecurityContext(currentGroupId)	

# create a dictionary with key:value pairs
annotation = {'Temperature': 25.3, 'Sample': 'control', 'Object count': 34}

result = create_map_annotation(ctx, annotation, target_id, target_type=target_type)
print "Annotation %s exported to Omero." % annotation

gateway.disconnect()
{{< /highlight >}}

</details>

---

### Batch Processing and Results Tables for OMERO Datasets

The previous examples demonstrated how to export local images to OMERO, or how to import OMERO images to a local workstation. As the final exercise, let's explore how an entire dataset comprised of many images can be downloaded from the remote OMERO instance, processed and analyzed locally, followed by an upload of the processed images and created results files back to the OMERO database.

The example script, `Omero_Batch_Processing.py`, consists of five key functions:

* **connect:** Establishes a connection to the OMERO server with specific user credentials. It returns an instance of the OMERO  `Gateway` class that is used later to upload processed images to the same OMERO server instance.
* **get_image_ids:** Gets a list of unique image IDs for a given dataset managed by the remote OMERO instance.
* **open_image:** Downloads the image associated with an image ID and shows it in Fiji.
* **process:** Applies a custom image processing routine to a given image. In this case a basic segmentation and counting of cells.
* **create_map_annotation:** Uploads the cell count value to OMERO and links it to the original image.
* **upload_csv_to_omero:** Converts an ImageJ ResultsTable into a csv file, uploads that csv file and links it ot the original image objects.
* **upload_image:** Uploads an Image to a specific dataset managed by the remote OMERO instance.

**Remember that the gateway connection needs to be closed at the end of the script**.

{{< figure src="/courses/fiji-omero/fiji-omero-batchprocessing.png" >}}

To test this and see the process in action we will process a set of four images that has been deposited in the OMERO database. The setup is as follows:

1. Go to the OMERO webclient and make note of your `Project ID`, or you cna create a new project if you prefer. Again you need the `ID`.

2. In the Fiji Script Editor, open the `Omero_Batch_Processing.py` script and execute it.

3. In the popup window, specify the parameters as follows:

	a. Replace the `mst3k` with your own credentials.
	
	b. **Omero Input Dataset ID:**  `265`
	
	c. **Omero Output Dataset Name:** Enter name to your liking
	
	d. **Omero Output Project ID:**  Enter the `ID` that you looked up as step 1. The script will create a new dataset (with the name you chose) and place all the processed images in there.
	
4. Click `OK`. Watch the console output for logging messages.

5. After the script ru has completed, go to the OMERO webclient and open the Project that you had chosen to collect the output.  Look for the `binary segmentation masks`, the attached `Results.csv` files and the new `Key-Value Pairs` annotations for each image.

<details>
<summary>View <code>Omero_Processing_Nuclei.py</code> script</summary>

{{< highlight python "linenos=table" >}}
#@ String (label="Omero User") username
#@ String (label="Omero Password", style="password") password
#@ String (label="Omero Server", value="omero.hpc.virginia.edu") server
#@ Integer (label="Omero Port", value=4064) server_port
#@ Integer (label="Omero Group ID", min=-1, value=-1) omero_group_id
#@ Integer (label="Omero Input Dataset ID", min=-1, value=-1) dataset_id
#@ String (label="Omero Output Dataset Name", value="Processed Images") target_ds_name
#@ Integer (label="Omero Output Project ID", min=-1, value=-1) project_id


import os
import tempfile


from java.lang import Long
from java.lang import String
from java.lang.Long import longValue
from java.util import ArrayList
from jarray import array
from java.lang.reflect import Array
import java
from ij import IJ,ImagePlus
from ij.measure import ResultsTable
import loci.common
from loci.formats.in import DefaultMetadataOptions
from loci.formats.in import MetadataLevel
from loci.plugins.in import ImporterOptions

from loci.plugins.in import ImporterOptions

# Omero Dependencies
import omero
from omero.rtypes import rstring
from omero.gateway import Gateway
from omero.gateway import LoginCredentials
from omero.gateway import SecurityContext
from omero.gateway.facility import BrowseFacility
from omero.gateway.facility import DataManagerFacility
from omero.log import Logger
from omero.log import SimpleLogger
from omero.gateway.model import MapAnnotationData
from omero.gateway.model import ProjectData
from omero.gateway.model import DatasetData
from omero.gateway.model import ImageData
from omero.gateway.model import FileAnnotationData
from omero.model import FileAnnotationI
from omero.model import OriginalFileI
from omero.model import Pixels
from omero.model import NamedValue
from omero.model import ProjectDatasetLinkI
from omero.model import ProjectI
from omero.model import DatasetI
from omero.model import ImageI
from omero.model import ChecksumAlgorithmI
from omero.model.enums import ChecksumAlgorithmSHA1160

from ome.formats.importer import ImportConfig
from ome.formats.importer import OMEROWrapper
from ome.formats.importer import ImportLibrary
from ome.formats.importer import ImportCandidates
from ome.formats.importer.cli import ErrorHandler
from ome.formats.importer.cli import LoggingImportMonitor
from omero.rtypes import rlong


def connect(group_id, username, password, host, port):    
    '''Omero Connect with credentials and simpleLogger'''
    cred = LoginCredentials()
    if group_id != -1:
        cred.setGroupID(group_id)
    cred.getServer().setHostname(host)
    cred.getServer().setPort(port)
    cred.getUser().setUsername(username)
    cred.getUser().setPassword(password)
    simpleLogger = SimpleLogger()
    gateway = Gateway(simpleLogger)
    gateway.connect(cred)
    group_id = cred.getGroupID()
    return gateway


def open_image(username, password, host, server_port, group_id, image_id):
    command="location=[OMERO] open=[omero:"
    command+="server=%s\n" % server
    command+="user=%s\n" % username
    command+="port=%s\n" % server_port
    command+="pass=%s\n" % password
    if group_id > -1:
        command+="groupID=%s\n" % group_id
    command+="iid=%s] " % image_id
    command+="windowless=true "
    command+="splitWindows=false "
    command+="color_mode=Default view=[%s] stack_order=Default" % ImporterOptions.VIEW_HYPERSTACK
    print "Opening image: id", image_id 
    IJ.runPlugIn("loci.plugins.LociImporter", command)
    imp = IJ.getImage()
    return imp


def upload_image(gateway, server, dataset_id, filepath):    
    user = gateway.getLoggedInUser()
    ctx = SecurityContext(user.getGroupId())
    sessionKey = gateway.getSessionId(user)
    
    config = ImportConfig()
    config.email.set("")
    config.sendFiles.set('true')
    config.sendReport.set('false')
    config.contOnError.set('false')
    config.debug.set('false')
    config.hostname.set(server)
    config.sessionKey.set(sessionKey)
    config.targetClass.set("omero.model.Dataset")
    config.targetId.set(dataset_id)
    loci.common.DebugTools.enableLogging("DEBUG")
    
    store = config.createStore()
    reader = OMEROWrapper(config)
    library = ImportLibrary(store,reader)
    errorHandler = ErrorHandler(config)
    
    library.addObserver(LoggingImportMonitor())
    candidates = ImportCandidates (reader, filepath, errorHandler)
    reader.setMetadataOptions(DefaultMetadataOptions(MetadataLevel.ALL))
    success = library.importCandidates(config, candidates)
    return success


def get_image_ids(gateway, dataset_id):
    """Return all image ids for given dataset"""
    browse = gateway.getFacility(BrowseFacility)
    experimenter = gateway.getLoggedInUser()
    ctx = SecurityContext(experimenter.getGroupId())
    images = []
    ids = ArrayList(1)
    ids.add(Long(dataset_id))
    j = browse.getImagesForDatasets(ctx, ids).iterator()
    while j.hasNext():
        image = j.next()
        images.append({
            'Image Id': String.valueOf(image.getId()),
            'Image Name': image.getName(),
            'Dataset Id': dataset_id,
        })
    return images


def create_map_annotation(ctx, annotation, target_id, target_type="Project"):
    # populate new MapAnnotationData object with dictionary
    result = ArrayList()
    for item in annotation:
        # add key:value pairs; both need to be strings
        result.add(NamedValue(str(item), str(annotation[item])))
    data = MapAnnotationData()
    data.setContent(result);
    data.setDescription("Demo Example");

    #Use the following namespace if you want the annotation to be editable in the webclient and insight
    data.setNameSpace(MapAnnotationData.NS_CLIENT_CREATED);
    dm = gateway.getFacility(DataManagerFacility);
    target_obj = None

    # use the appropriate target DataObject and attach the MapAnnotationData object to it
    if target_type == "Project":
        target_obj = ProjectData(ProjectI(target_id, False))
    elif target_type == "Dataset":	
        target_obj = DatasetData(DatasetI(target_id, False))
    elif target_type == "Image":	
        target_obj = ImageData(ImageI(Long(target_id), False))
    result = dm.attachAnnotation(ctx, data, target_obj);
    return result


def upload_csv_to_omero(ctx, file, tablename, target_id, target_type="Project"):
    """Upload the CSV file and attach it to the specified object"""
    print file
    print file.name
    svc = gateway.getFacility(DataManagerFacility)
    file_size = os.path.getsize(file.name)
    original_file = OriginalFileI()
    original_file.setName(rstring(tablename))
    original_file.setPath(rstring(file.name))
    original_file.setSize(rlong(file_size))

    checksum_algorithm = ChecksumAlgorithmI()
    checksum_algorithm.setValue(rstring(ChecksumAlgorithmSHA1160.value))
    original_file.setHasher(checksum_algorithm)
    original_file.setMimetype(rstring("text/csv"))
    original_file = svc.saveAndReturnObject(ctx, original_file)
    store = gateway.getRawFileService(ctx)

    # Open file and read stream
    store.setFileId(original_file.getId().getValue())
    print original_file.getId().getValue()
    try:
        store.setFileId(original_file.getId().getValue())
        with open(file.name, 'rb') as stream:
            buf = 10000
            for pos in range(0, long(file_size), buf):
                block = None
                if file_size-pos < buf:
                    block_size = file_size-pos
                else:
                    block_size = buf
                stream.seek(pos)
                block = stream.read(block_size)
                store.write(block, pos, block_size)

        original_file = store.save()
    finally:
        store.close()

    # create the file annotation
    namespace = "training.demo"
    fa = FileAnnotationI()
    fa.setFile(original_file)
    fa.setNs(rstring(namespace))

    if target_type == "Project":
        target_obj = ProjectData(ProjectI(target_id, False))
    elif target_type == "Dataset":	
        target_obj = DatasetData(DatasetI(target_id, False))
    elif target_type == "Image":	
        target_obj = ImageData(ImageI(target_id, False))

    svc.attachAnnotation(ctx, FileAnnotationData(fa), target_obj)
 

def process_file(imp):
    """Run segmentation"""
    print "Processing", imp.getTitle()
    title = imp.getTitle().split('.')[:-1]
    title = '.'.join(title) + "_mask.ome.tiff"
    nimp = ImagePlus(title, imp.getStack().getProcessor(1))
    IJ.run(nimp, "Median...", "radius=3")
    IJ.run(nimp, "Auto Local Threshold", "method=Bernsen radius=15 parameter_1=0 parameter_2=0 white")
    IJ.run(nimp, "Watershed", "")

    IJ.run("Set Measurements...", "area mean standard centroid decimal=3")
    IJ.run(nimp, "Analyze Particles...", "size=50-Infinity summary exclude clear add")
    rt = ResultsTable.getResultsTable()
    rt.show("Results")

    imp.close()
    return nimp, rt 


def create_new_dataset(ctx, project_id, ds_name, ):
    dataset_obj = omero.model.DatasetI()
    dataset_obj.setName(rstring(ds_name))
    dataset_obj = gateway.getUpdateService(ctx).saveAndReturnObject(dataset_obj)
    dataset_id = dataset_obj.getId().getValue()

    dm = gateway.getFacility(DataManagerFacility)
    link = ProjectDatasetLinkI();
    link.setChild(dataset_obj);
    link.setParent(ProjectI(project_id, False));
    r = dm.saveAndReturnObject(ctx, link);
    return dataset_id 

	
# Main code
gateway = connect(omero_group_id, username, password, server, server_port)
currentGroupId = gateway.getLoggedInUser().getGroupId()
ctx = SecurityContext(currentGroupId)

image_info = get_image_ids(gateway, dataset_id)
tmp_dir = tempfile.gettempdir()
print tmp_dir

target_ds_id = create_new_dataset(ctx, project_id, target_ds_name)
for info in image_info:
    imp = open_image(username, password, server, server_port, omero_group_id, info['Image Id'])
    processed_imp, rt = process_file(imp)

    # Save processed image locally in omero_tmp dir
    imgfile = tempfile.TemporaryFile(mode='wb', prefix='img_', suffix='.tiff', dir=tmp_dir)

    #filepath = os.path.join(tmp_dir, processed_imp.getTitle())
    options = "save=" + imgfile.name + " export compression=Uncompressed"
    IJ.run(processed_imp, "Bio-Formats Exporter", options)
    # ignore changes & close
    processed_imp.changes=False 
    processed_imp.close()

    # uploaad image to a target dataset
    upload_image(gateway, server, target_ds_id, [imgfile.name])

    # create annotation
    annotation = {
        "Cell count": rt.size()
    }	
    create_map_annotation(ctx, annotation, info['Image Id'], target_type="Image")

    # export ResultsTable to csv file and link to image object
    file = tempfile.TemporaryFile(mode='wb', prefix='results_', suffix='.csv', dir=tmp_dir)
    rt.saveAs(file.name)
    #upload_csv_to_omero(ctx, file, "Results.csv", long(info['Image Id']), "Image")

# done, clean up	
shutil.rmtree(tmp_dir)
gateway.disconnect()	
print "Done.\n"

{{< /highlight >}}
</details>

---

# Resources {#resources-id}

**OMERO**

* OMERO: https://www.openmicroscopy.org/omero/
* OMERO User Support: https://help.openmicroscopy.org
* UVA Research Computing: https://www.rc.virginia.edu
* OMERO at the University of Virginia: https://www.rc.virginia.edu/userinfo/omero/overview/
 
**Fiji Scripting**

* RC tutorial [Fiji/ImageJ: Script development for Image Processing](/tutorials/fiji-scripting/)
* Tutorial: https://syn.mrc-lmb.cam.ac.uk/acardona/fiji-tutorial/
* Tips for Developers: https://imagej.net/Tips_for_developers
* API: https://imagej.nih.gov/ij/developer/api/
* SciJava: https://javadoc.scijava.org/Fiji/

**General Python Programming**

* [Introduction to Programming in Python](/courses/python_introduction/)
* [Programming in Python for Scientists and Engineers](/courses/programming_python_scientists_engineers/)

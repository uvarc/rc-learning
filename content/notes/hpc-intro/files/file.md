---
title: Working with Files
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 400

menu:
  hpc-intro:
    name: Working with Files
---
{{< youtube-reduced-width id="asN63Ujhzks" maxwidth="600px" title="Working with Files video" transcriptid="wwf-video-transcript">}}

Files are the foundation of working with an HPC cluster.  We need to be able to:

* Transfer files to and from the cluster
* Edit text files
* Create files through the software we run

Each user has a `home` location and a `scratch` location.  When you log in you will be in the `home` location.  For now we will assume you will work with files in your home folder.  We will discuss the scratch folder later.

<!--more-->

{{< spoiler text="Working with Files Video Transcript" id="wwf-video-transcript">}}
**Narrator:** Hello and welcome back to the University of Virginia's High Performance Computing tutorial series. In this module, we will be covering the various ways of working with files on UVA's HPC system.

First, it's important to understand the file structure used by the HPC system. Since the system runs on Linux, we'll be using Linux naming conventions for files and folders. All file paths start from the root, which is denoted by a forward slash (/). All other files and folders within the cluster stem from this root, forming an upside-down tree structure.

The main folder at the top is the root folder, and there's nothing else above it. This is where everything is located. Within the root are folders such as home and scratch, which contain folders named after the computing IDs of active users on the cluster. Your computing ID within home leads to your home folder.

To navigate to your scratch folder from there, you follow a similar path but go backwards in the file tree: from your computing ID to home to root, and then over to scratch, and then your computing ID. Everything within there is your personal scratch folder. It's important to note that the home and scratch folders are connected only through the root.

All files have a full name called their path, which provides the operating system with the location of the file. This can be the exact location starting from the root or relative to a starting point, usually the folder you're currently in. This is different from other operating systems. For example, Windows users might be used to having a drive name at the beginning, like the C drive, and using backslashes (\) instead of forward slashes (/) to navigate through users and folders to a specific file. macOS uses a similar folder structure to Linux, starting from root, then Users, the user's name, Documents, and other folders leading to a file.

In Linux, specifically on our system, paths go from root, forward slash, to either home, scratch, project, or standard. Home and scratch have folders for each individual computing ID, while project and standard have storage share names. All paths traverse through folders, which Linux calls directories (these terms are synonymous) and end at a file.

To transfer data to and from the HPC system, there are many different options available. Here are some of the more popular options. MobaXterm is primarily an SSH client for Windows, but it also allows you to transfer files with a simple drag and drop interface. Another desktop app you can use is FileZilla, which is specifically for file transfer and works on Mac and Linux. For small files, you can use the upload and download buttons in the Open OnDemand file manager.

There are also command line transfer options. One option is scp, Secure Shell Copy, which you can use to transfer files through the terminal. Another popular option is rsync. Through the web browser on FastX or the Desktop interactive app, you can download data from the internet, including UVA Box or other cloud storage locations. Git is also available from the HPC terminal, allowing you to clone repositories directly onto the system.

The last option, which is the most involved, and the one we most recommend for large amounts of data, is Globus. This is a separate application you can use to transfer data between two different locations, like your laptop and the cluster.

Here are some examples of using those different graphical file transfer options. To connect with MobaXterm on a Windows machine, click Session in the top left, then the SFTP option at the top. Use one of the specific hosts like login1, login2, or login3.hpc.virginia.edu. Type in your computing ID into the username box and click OK. Click Accept on any server identity prompt that pops up. A separate prompt will appear asking for your password. Then you will have the option to save your password to the password manager. I'll click No for now.

A window will open where you can select a folder from your local machine in the top left pane, then drag and drop files between that folder in the bottom left pane and the cluster in the right pane.

To connect using FileZilla, click the Site Manager icon in the upper left. Select New Site and rename it. On the right side of the dialog box, select SFTP in the protocol dropdown. We recommend using a specific host name, which would be login1, login2, or login3.hpc.virginia.edu. If the port field is blank, type 22 into it. Then enter your username, which is your computing ID, and then your password. Click OK to save. You may be asked if you want FileZilla to remember passwords. If you select Do not save, then you will have to re-enter your password whenever you connect. I'll select Save passwords for now.

To initiate the connection, reopen the Site Manager, then click Connect. If you get an unknown host key warning, click OK. A double-paned window will open where you can transfer files by dragging and dropping between your local computer in the left-hand pane and the HPC system in the right-hand pane.

Open OnDemand has a file manager, which is an easy way to work with files on the cluster right through your browser. Clicking on the Files tab opens a dropdown with the different folders that you have access to. Every account has Home and Scratch, and then there's also Project and Standard Storage if you're part of a group that leases one of these two different storage options. Clicking on one of these folders opens the file manager.

Here, you can perform various operations such as creating a new file or directory. Let's create a simple text file named testfile. Scroll down to find your file, then click on it to open the file. At first, the file will be blank since you haven't added anything to it yet. To edit this text file, click the ellipsis on the right. This dropdown allows you to view, rename, download, or delete the file. To edit it, click the Edit button, which will open a new browser tab where you can add content to the file. You can add text or whatever you want. Whenever you're done editing, click the Save button at the top left. Make sure you save before exiting the browser, as exiting will close the file without saving.

Two other useful features are the Upload and Download buttons. Clicking Upload presents a drag and drop screen where you can drag files from your local file system and upload them to the cluster. Alternatively, you can click browse files to open your file manager and navigate using that. For downloading, select a file using the checkboxes on the left, then hit Download. This will download the file or folder to your computer's Downloads folder. Note that the Upload and Download buttons can only handle relatively smaller file sizes. Anything larger than 100 megabytes will fail, so you'll need to use another file transfer method.

You can also copy or move files. Select the file, then click Copy/Move, then navigate to the desired folder where you can then click the Copy or Move buttons on the left to finalize. To delete a file, use the ellipses, or select the file then hit Delete in the top right. Confirm the deletion and then the file will be permanently deleted. Finally, there's a Refresh button if needed, an Open in Terminal button to open the command line in another tab with the current folder set as the working directory, and a button that takes you to the Globus browser application.

Another option for working with files is using the Caja file manager in FastX. This is accessible either through the folder icon on the desktop or the little filing cabinet at the top. This will give you a GUI similar to the Windows File Explorer or macOS Finder app. To create new files, go to File → Create Document to create an empty file, then type in the file's name. You can also right-click and select Open With. There are a couple of different text editors you can use, such as Emacs, Gvim, LibreOffice Writer, and Pluma. We recommend Pluma for editing plain text. Clicking Pluma will open the file in a GUI text editor where you can add content and save it using the Save button in the top right. You can also open the Pluma editor by going to the Applications → Accessories menu. This will open a blank document where you can type text and then click Save to save it anywhere in the file system.

In FastX, you can also use graphical development interfaces such as VSCode, Spyder, or RStudio for editing programs or scripts. For running programs or scripts through environments such as these, use the Open OnDemand interactive apps.

To create a new folder in Caja, go to the File menu and click Create Folder. To rename, delete, cut, copy, or paste files or folders, right-click to access the appropriate option. Note that the Delete option performs a permanent deletion while there is a separate Move to Trash option. We recommend not using Move to Trash since the space in your home directory is limited. Caja also supports drag and drop, so you can move files either by cutting and pasting or by dragging them into a new location.

It's better to edit files directly on the system rather than on your local machine and then transferring them, to avoid any formatting issues that may arise from using a word processor on your local machine to edit plain text files.

To use Globus, you'll need a separate application to transfer files. You'll be using a browser app on the Globus page. With Globus, you can move and share your data between or within applications. Whether your files are on your local workstation, a lab workstation used by multiple users, our cluster, or another cluster at a different university, you can manage and transfer all of that data within the web app that Globus provides.

One big advantage of Globus is the ability to queue up a set of files or folders to be copied over. You'll select the transfer button to initiate the transfer, which is then performed on specific data transfer nodes or DTNs. These computers or servers will carry out the transfer, allowing you to go off and do other tasks without worrying about it. The transfers are high performance, thanks to the dedicated nodes, making this a reliable method for handling large amounts of data without the risk of data loss. If you have terabytes of data, this is the transfer method we recommend.

To get started with Globus, go to UVA Research Computing's Globus page, as it provides detailed instructions. Here you'll find information on getting started, setting up your own collection on your computer, transferring files, sharing folders with other users, and other advanced features.

Finally, if you need help with UVA's HPC system, there are multiple ways to get assistance. You can visit our Zoom-based office hour sessions on Tuesdays from 3 to 5 p.m. and Thursdays from 10 a.m. to noon. If you can't make it to these office hours or have a more specific request, you can submit a support ticket and we'll get back to you. Links to both are on the RC Learning website. The main Research Computing website is also a valuable resource. We add a lot of documentation here and keep it updated. If you have a basic question or think it might be covered already, we have an FAQ section. We also have a list of how-to's on various topics. If you can't find what you're looking for, we have a search feature where you can search our site for different information.

This concludes the Working with Files tutorial at the University of Virginia. Thank you.
{{< /spoiler >}}
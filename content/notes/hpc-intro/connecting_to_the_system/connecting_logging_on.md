---
title: Connecting and Logging On To HPC
date: "2022-10-01T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 100

menu:
  hpc-intro:
    name: Connecting and Logging On To HPC
---
{{< youtube-reduced-width id="BpaFQG4JOEU" maxwidth="600px" title="Connecting to HPC video" >}}

There are three ways to connect to the HPC System:
* Open OnDemand, a graphical user interface through a web browser
  * you can examine and manipulate files and submit jobs.
  * you can access applications such as Matlab, Jupyterlab, and R Studio Server.
* FastX Web, direct access to a desktop 
* _ssh_ (Secure Shell) client, which provides direct access to the command line.
   * For Windows we recommend [MobaXterm](https://mobaxterm.mobatek.net/)

<!--more-->

{{< spoiler text="Connecting to HPC Video Transcript" >}}
**Narrator:** Hello and welcome back to the University of Virginia's high-performance computing tutorial series. In this module, we will cover three different ways to connect to the HPC system at UVA.

The first method is Open OnDemand, which is a web application accessed through a web browser. From there, you can manipulate files, work with jobs, and use a host of different GUI applications. The second method is FastX, which is another web application that gives you direct access to a Linux desktop, where you can access your files, run GUI applications, and use a browser within the HPC system. Lastly, there is SSH, the Secure Shell Client, which gives you a command line-only view of the cluster.

Note that these methods are for accessing the login nodes, which are for tasks like editing files or submitting jobs to the compute nodes. Anything computationally intensive or process-heavy needs to be run on the compute nodes, either in a batch job, a Slurm interactive job, or the appropriate Open OnDemand interactive application.

Let's start by going over Open OnDemand. To connect, open your web browser and type ood.hpc.virginia.edu. Once you do that, you'll need to authenticate with your NetBadge computing ID and password. You could do this using any network, so you don't need to be using UVA Wi-Fi or the UVA VPN, making it very accessible.

Open OnDemand is a web application. If the browser ever freezes or you are waiting for certain changes to apply, you might have to refresh the pages. You could do this by either clicking the UVA Open OnDemand button, which will navigate you back to the dashboard, or by refreshing your browser, which will likely unfreeze any browser issues or apply changes. Open OnDemand will also open many other tabs such as text files or other applications. Generally, if you're not using those tabs, it's safe to close them. However, if you were writing a text file in one of those tabs, there is no automatic saving, so you should periodically click Save, and then it will be safe to close that tab.

This is what the dashboard looks like. The top bar contains all the actual functions of Open OnDemand. First, we have the Files tab. This is where you can manage and manipulate your files. Next is the Jobs tab. Here you can create, run, and view your active jobs. The Clusters dropdown allows you to open a command line. Under Interactive Apps, you can find all the interactive applications. Finally, we have the Utilities section. These are smaller applications designed to provide a graphical user interface for various tasks. Logging out of Open OnDemand is simple. Just close your browser.

Connecting with FastX is another web-based login method, which connects you directly to a desktop view. You can access it either through the Open OnDemand Interactive Apps menu, or by using a direct link to FastX: fastx.hpc.virginia.edu. FastX requires you to be on the UVA network, either by using on-Grounds Wi-Fi or Ethernet, or by turning on the UVA VPN. If you are not on the UVA network, the page won't load. Once connected, it will prompt you to enter a username and password. Across all of Research Computing systems, if there is a username and password field, it's usually asking for your UVA computing ID and NetBadge password. Go ahead and log in.

You will then be presented with a new screen where all of the applications that you can use are on the left. You can open up a terminal here to do command line work. The actual desktop login method is the MATE desktop. Click that and then click on the play button. It'll open up a new tab asking you how you want to connect. The browser client is recommended as the desktop client needs to be installed to your computer before use. Once you click on that, you'll be presented with a desktop on the cluster. From here, you can manipulate files, either by clicking the folder icon to see all your files in your home folder, or by clicking the filing cabinet icon on the top bar, which opens up the same screen.

There are a couple of other dropdowns: Applications where you can open up different applications and System where you can log out. You can open up a terminal by clicking the terminal icon at the top. One of the most useful features of FastX is the Firefox browser. Clicking on that will open up Firefox and you can browse the internet while on the cluster. Some use cases for this include accessing data on cloud storage platforms like Google Drive, UVA Box, or Microsoft OneDrive. If there is software you need to download, you can do it directly on the cluster through this browser, saving steps. If you are more familiar with GUI applications or a GUI text editor, you can open different GUI applications from the terminal, like GVim.

To log out, either go up to System and click Log Out or exit out of the browser. If you exit out of the browser, your session still exists. You should always terminate them when they're done. To terminate the session, click the little X at the top right, then Terminate. You can always click the play button again to reopen your session.

The last method of accessing HPC is connecting through the terminal using SSH. You would use this command: ssh -Y YourComputingID@login.hpc.virginia.edu. The -Y option enables X11 forwarding, which allows you to use GUI applications through the command line on supported systems. However, it's slower than using FastX, which we recommend for GUI applications. If you're on Mac or Linux, you should already have an SSH interpreter built into your command line. Simply open a terminal and use that command. On newer versions of Windows, you can run the SSH command from PowerShell. If you have an older version of Windows, you might need to use a separate application. We recommend MobaXterm.

Similar to FastX, you must be on the UVA network to use SSH, either by being on Grounds or using the UVA VPN. For this example, I'll be using MobaXterm. The command I'll run is ssh MyComputingID@login.hpc.virginia.edu. When you first use this command in MobaXterm, you'll be prompted to enter a password, which is just your NetBadge password. You'll know you've logged in because you'll see "Welcome to UVA HPC." Then you'll see the bash interpreter, signified by the prompt on the left of the command line. From here, you can go ahead and list your folders and do various tasks on the HPC system. To log out, you can use the command exit, or if you simply close out of MobaXterm, it will cancel any SSH processes that are running, which will terminate your connection.

One final note, some of the methods discussed in this video require using the UVA VPN if you are connecting from off-Grounds. For details on installing and using the UVA VPN, see this link. Finally, if you need help with UVA's HPC system, there are multiple ways to get assistance. You can visit our Zoom-based office hours sessions on Tuesdays from 3-5pm and Thursdays from 10am to noon. If you can't make it to these office hours or have a more specific request, you can submit a support ticket and we'll get back to you. Links to both are on the RC Learning website. The main Research Computing website is also a valuable resource. We add a lot of documentation here and keep it updated. If you have a basic question or think it might be covered already, we have an FAQ section. We also have a list of how tos on various topics. If you can't find what you're looking for, we have a search feature where you can search our site for different information.
This concludes the Connecting to HPC tutorial at the University of Virginia. Thank you.
{{< /spoiler >}}
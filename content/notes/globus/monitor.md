---
title: "Using Globus to Transfer Data"
type: article
toc: true
date: 2023-02-02T00:00:00-05:00
---

{{< figure library="true" src="globus.png" width=30% >}}

This tutorial will cover data transfer to and from UVA Research Computing storage systems using Globus software. Topics include: installing Globus, transferring files, monitoring large transfers, and sharing data with collaborators.

<br>

# An Overview of Globus

Globus is a non-profit service for secure, reliable **research data management** developed and operated by the **University of Chicago** and **Argonne National Laboratory**, supported by funding from the Department of Energy, NSF, and the NIH. With Globus, subscribers can **move, share, & discover data** via a single interface – whether your files live on a supercomputer, lab cluster, tape archive, public cloud or your laptop, you can manage this data from anywhere, using your existing identities, via just a web browser.

Globus stems from GridFTP and the high energy physics community, but grew much beyond that. It started as a pure transfer tool with two strengths:

1. Fast transfers over good networks
2. Robust transfers over flaky networks

Globus now has the add functionality of:

- Data sharing and flexible access control
- Identity management
- A web GUI, scriptable command line tool, and powerful API


## Advantages of Using Globus

Globus provides a secure, unified interface to your research data. Use Globus to "fire and forget" high-performance data transfers between systems within and across organizations.

{{< figure src="/notes/globus/imgs/globus-advantages.png" width=30% >}}

There are many advantages to using Globus: 

- The Globus web app has an easy-to-use point-and-click interface.
- Transfers faster than SCP/SFTP (usually by a factor of two).
- Globus continues interrupted transfers – no need to restart.
- Globus allows you to schedule regular transfers.
- Get email notifications for successful or failed transfers.
- Globus accounts are free! Collaborators don’t need a sponsored UVA account to use Globus.
- VPN is not needed to transfer to and from UVA systems.
- Approved for transferring sensitive data (HIPAA, CUI).


### VPN or no VPN?

UVA Anywhere/More Secure VPN is not necessary for Globus. With just the web app, you can control transfers between systems that have Globus Personal Connect or Server installed.

* The VPN will slow down transfers between your computer and Rivanna.
* The High Security VPN completely blocks transfers between your computer and secure Ivy storage.


## Globus Terminology

**Collection:** A computer, server, or folder containing data.

**Personal Collection:** A PC or workstation where you installed Globus.

**UVA Standard Security Storage:** The collection for Rivanna/Research Project and Research Value storage.

**UVA IVY-DTN:** The collection for Ivy Central Storage.

**Shared Endpoint:** A folder that you can share with other Globus users.


## Installing Globus

To transfer data to and from your computer, you will first need to install Globus Personal Connect. The following links provide instructions for installing Globus Personal Connect based on your machine's operating system.

{{< table >}}
| Platform | Installation instructions |
| --- | --- |
| Mac | https://docs.globus.org/how-to/globus-connect-personal-mac |
| Linux | https://docs.globus.org/how-to/globus-connect-personal-linux |
| Windows | https://docs.globus.org/how-to/globus-connect-personal-windows |
{{< /table >}}

The video and set of instructions below show how to navigate to the installation links from the Globus homepage.

{{< video src="globus-install-link.mp4" controls="yes" >}}

**Instructions**

1. Go to https://www.globus.org/
2. Click **“I Want To…”** > **“Enable Globus on my system”**
3. Scroll down to “Globus Connect Personal” (light blue box) and click the **“Get Globus Connect Personal”** link
4. Scroll down to “Install Globus Connect Personal” (light blue box)
5. Click the link for your operating system and follow the installation instructions


## Setting up Globus

The following video and instructions show how to set up Globus once it is installed on your computer.

{{< video src="globus-setup.mp4" controls="yes" >}}

**Instructions**

1. Open the Globus Application and click “Log In”
2. Choose “University of Virginia” and log in using Netbadge
3. Choose a label for consent and click “Allow”. (The label you choose doesn't really matter.)
4. Choose a name for your Personal Collection. This is the name that you will see in the Globus collections list, so choose something descriptive enough that you know what it is and can quickly find it by searching (e.g. Martinez-Lab-Workstation, Zhang-Personal-Laptop).
5. **Do NOT click the High Assurance checkbox!** The UVA Secure Data Transfer Node is already configured for sensitive data transfer. Checking the box is redundant conflicts with the default configuration.
6. Click "Save" then "Exit Setup".

## Transferring Files

Files are transferred with the Globus File Manager Web App. There are three ways to get to the app:

1. Go straight to https://app.globus.org/file-manager
2. Go to https://www.globus.org/ -> Log In (top right corner)
3. Click Globus icon in Toolbar -> Web: Transfer Files

Once the app is open you can choose the collections between which you wish transfer data, as detailed in the video and instructions below.

{{< video src="globus-transfer-data.mp4" controls="yes" >}}

**Instructions**

1. Click the “Collection” field.
2. Click the “Your Collections” tab.
3. Select your Personal Collection.
4. Click “Transfer or Sync to…” in the gray menu.
5. Click the second “Collection” field.
6. Search for and select “UVA Standard Security Storage” or “UVA IVY-DTN”.
7. Select the files or folders you want to transfer.
8. Select the destination for your files.
9. Click the highlighted “Start” button.

### Monitoring Your Data Transfer

By clicking on the "Activity" tab, you can check on the progress of transfers, monitor the effective transfer speed, and look for any failures.

{{< video src="globus-monitor-transfer.mp4" controls="yes" >}}

### Transfer Options

- By default, transfers on UVA DTNs are synced (option 1) and encrypted (option 5) – no need to select them.
- Files with errors will cause entire transfer to fail – skip files with errors instead (option 6).
- Schedule one-time and regular transfers with Timer.

{{< figure src="/notes/globus/imgs/globus-transfer-options.png" width=30% >}}


### Connecting to External Drives

When you first set up Globus, it only has access to certain directories of your local drive. You can add additional drives such as mapped network drives or external hard drives in the Globus Options/Preferences menu.

{{< video src="globus-external-drives.mp4" controls="yes" >}}

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

## Sharing Data with Collaborators

Globus users are able to share data with anyone with a Globus account. All UVA Rivanna and Ivy users have Globus accounts (authenticate with Netbadge).

External collaborators don’t need to be affiliated with an institution using Globus in order for you to share data with them. Anyone can create a personal Globus account using @globusid.org

{{< figure src="/notes/globus/imgs/globus-collab.png" width=30% >}}

The following video and instructions show how to create a **shared endpoint**, a folder in which collaborators can upload and download data. Shared endpoints may be public (visible to the world!) or accessible only to users with permission.

{{< video src="globus-share-data.mp4" controls="yes" >}}

**Instructions** 

1. Select the file or folder you want to share.
2. Click the `Share` button.
3. Click "Add a Shared Endpoint".
4. Enter a name and description for the Shared Endpoint.
5. Click "Create Share".
6. Click "Add Permissions – Share With".
7. Enter the UVA or Globus ID of the user you want to share with.
8. Click "Add" and "Add Permission".

Optional: Enter an email message and add write permissions so the user can upload data.

## Troubleshooting

{{< table >}}
| Common Issues | Solution |
| --- | --- |
| I have admin privileges on my Health System computer. Why isn’t the Globus installation working? | Sometimes the Health System firewall prevents Globus software from connecting to the mothership. Ask HIT to remote in and complete the installation. |
| Why won’t my transfer to Ivy storage start? | Globus doesn’t work while connected to the High Security VPN. Disconnect while transferring data. |
| Globus is transferring folders but they’re all empty. | There is probably a file with bad permissions or characters in the filename. Choose “Skip files with errors” in the Transfer options |
| I can’t connect to UVA Standard Security Storage. | When leaving UVA, your Eservices account can expire before your email – meaning no Globus access. |
{{< /table >}}

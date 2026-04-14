---
title: Local SSH vs OOD
date: 2026-04-14T05:31:18Z
type: docs 
toc: true
weight: 40
menu: 
    vscode-intro:
---

### **SSH vs OOD**

| | **SSH** | **OOD** |
| :-- | :-- | :-- |
| Network | UVA Network/VPN required | No network requirement |
| Execution | On front-end, no running code | On production nodes, can run code |
| Resources | ijob resources specific to terminal | No need for ijobs, have resources already |
| Notes | Must take extra care to use properly | Some extensions may be unavailable |

---

### **Why Even SSH?**

- Extension Availability
  - But if one isn't available just reach out to us!
- Local preferences
  - But you can export the settings.json file to HPC
- No queue wait time
  - But if you are requesting appropriate, interactive resources the wait time will be minutes at our busiest

---

### **In Most Cases, Use OOD**

**We strongly urge our users to use OOD.**

Those resources are available and best for our users.

All functionality is the same and you will always have access to compute resources.

You will not need to worry about overwhelming with processes or fight with ijob terminals.

---

### **Setting Up SSH**

#### **Download SSH Extension**

Search for **"Remote – SSH"** in the Marketplace.

It is not **necessary** but it is a tool to make it **much** easier

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_44.png" alt="Screenshot of the Remote - SSH extension in the VS Code marketplace" width="600px" >}}

#### **Create Connection**

Connect to Host > **Add New SSH Host...**

```bash
ssh -YA mst3k@login.hpc.virginia.edu
```

Modify your config file

You can later open and edit the config file as you need

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_45.png" alt="Screenshot of the VS Code Remote Explorer panel showing Connect to Host option" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_46.png" alt="Screenshot of the Add New SSH Host input dialog in VS Code" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_47.png" alt="Screenshot of the SSH config file opened in VS Code for editing" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_48.png" alt="Screenshot of the SSH config file showing the newly added HPC host entry" width="600px" >}}

#### **You Are Now Connected!**

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_49.png" alt="Screenshot of VS Code showing a successful connection to UVA HPC displayed in the status bar" width="600px" >}}

Now that you are connected you can open up your workspaces you've created, open your folders, etc.

You can see your connection in the bottom right

You still stay connected so long as you stay on the network

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_50.png" alt="Screenshot confirming the active VS Code SSH connection to the HPC cluster" width="600px" >}}

---

### **Running Code with SSH**

#### **Run an IJob**

Open a new terminal

```bash
ijob -c 6 -A hpc_training -p interactive --time=00:30:00
```

Starts an ijob on the interactive partition with 6 cores for 30 minutes

Will need to wait for resources to be available

An ijob is **specific to the terminal you start it in**, you only have access to those resources in that terminal

**The job ends if you close the terminal**

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_51.png" alt="Screenshot of a VS Code terminal with an active ijob session on the interactive partition" width="600px" >}}

**Important Caveats:**

This is for running code via terminal commands

If you want to run python code you need to "Run Python File" first, then type in ijob command in that **new** terminal

**Different method for JupyterLab**

**DO NOT RUN JOBS FROM IJOBS**

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_52.png" alt="Screenshot illustrating the warning that jobs must not be run from within an ijob session" width="600px" >}}

---

### **Logging Out**

VSCode can create many processes on the front-ends.

If you are going to use SSH you *must* use it properly.

Disconnect your session every time you are finished.

Do not leave it running in the background.

In most cases, **OOD interactive app** is superior and preferred to SSH.

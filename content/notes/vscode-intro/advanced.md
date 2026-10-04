---
title: Advanced Features
date: 2026-04-14T05:31:18Z
type: docs 
toc: true
weight: 50
menu: 
    vscode-intro:
---

## **Transferring settings.json**

Open the Command Palette and select **Preferences: Open Profiles (UI)**

Export your profile: **… -> Export...**

On the target machine, select **Import Profile > Create**

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_53.png" alt="Screenshot of the VS Code profile list with a 3-dot menu showing the Export option for a specific profile" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_54.png" alt="Screenshot of the Select Profile Template File dialog in VS Code" width="600px" >}}

---

## **Creating an SSH Key**

Open a new bash terminal (local) and run:

```bash
ssh-keygen
```

Navigate to your `.ssh` directory

`cat` your .pub file and copy it

Log in to remote server and paste your key in your `/home/id/.ssh/authorized_keys` file. If it does not exist, create it.

Passwordless login!

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_55.png" alt="Screenshot of the ssh-keygen command running in a local bash terminal" width="600px" >}}

---

## **Source Control with GitHub**

- Open a Folder or Clone a Repository
- Push/Pull/Commit
- Create new branches
- Even works on HPC without needing to load git module

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_56.png" alt="Screenshot of the VS Code Source Control panel showing Git integration" width="600px" >}}

{{< figure src="/notes/vscode-intro/img/Introduction%20to%20visual%20studio%20code%20for%20hpc%20%281%29_57.png" alt="Screenshot of the VS Code Git clone dialog with a URL typed in" width="600px" >}}

**Get started with Git and GitHub:**

[Hello World guide on GitHub Docs](https://docs.github.com/en/get-started/start-your-journey/hello-world)

---

## **Need Help?**

[xve5kj@virginia.edu](mailto:Xve5kj@virginia.edu) | [hpc-support@virginia.edu](mailto:hpc-support@virginia.edu)

**Zoom Office Hours:**

Tuesdays 3pm-5pm | Thursdays 10am-12pm

[UVA Research Computing Support](https://www.rc.virginia.edu/support/)

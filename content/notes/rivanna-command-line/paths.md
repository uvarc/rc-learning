---
title: Paths
date: 2023-12-11-14:11:14Z
type: docs 
weight: 750
menu: 
    rivanna-command-line:
---

In most operating systems, all files and directories are located with a **path**.  The “path” is the "full name" of every file & directory.

In a Unix-based operating system, the files are organized into a _tree_ structure.  The tree is "upside down" because the **root directory** is at the top, and directories branch off from there.  The root directory is indicated with a forward slash `/`.

{{< diagram >}}
graph TD
    C["/"] --> etc["etc"]
    C["/"] --> opt["opt"]
    C["/"] --> lib["lib"]
    C --> usr["usr"]
    C --> home["home"]
    C --> var["var"]
    
    home --> mst3k["mst3k"]
    home --> im4u["im4u"]

    mst3k-->myfile["myfile"]
    mst3k-->mydir["mydir"]

    mydir-->file1["file1"]
    mydir-->file2["file2"]

    usr--> bin["bin"]
    usr--> share["share"]
    usr--> local["local"]

    bin--> ls["ls"]
    bin--> pwd["pwd"]
    bin--> cd["cd"]
{{< /diagram >}}

The **home** directory is the usual name on Linux and similar Unix-based operating systems for the folder that holds user directories and files.  On MacOS it is called User.  On both, the _separator_ between branches of the tree is the forward slash.

{{< hl >}}
/home/mst3k/myfile
{{< /hl >}}
<br>
{{< hl >}}
/Users/Misty Tea/Documents/Homework.pages
{{< /hl >}}

Windows files and folders also have paths.  In Windows, drive letters or _volumes_ are the top-level folders, and usually there is more than one.  User files are in Users, similar to MacOS.

{{< hl >}}
C:\Users\Misty Tea\Documents\Homework.docx
{{< /hl >}}

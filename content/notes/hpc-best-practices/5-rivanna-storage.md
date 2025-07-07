---
title: V - Rivanna Storage Systems
date: 2025-06-14-14:47:30Z
type: docs 
weight: 250
toc: true
menu: 
    hpc-best-practices:
---

## Rivanna File Systems

The default home directory on Rivanna has 50GB of storage capacity, e.g., `/home/gka6a`. Each user will have access to 10 TB of __temporary__ storage, e.g., `/scratch/gka6a`. The `/home` and `/scratch` directories are for personal use and not shareable with other users.

__Important:__ `/scratch` is __NOT permanent__ storage, and files that have not been accessed for more than __90 days__ will be marked for deletion.

## Running Jobs from Scratch

We recommend that you run your jobs out of your `/scratch` directory for two reasons:
* `/scratch` is on Weka filesystem (a storage system designed specifically for parallel access)
* `/scratch` is connected to the compute nodes with Infiniband (a very fast network connection)

We also recommend:
* You keep copies of your programs and data in more permanent locations (e.g., your home directory or leased storage such as `/project` or `/value`)
* After your jobs finish, you copy the results to more permanent storage


## Leased Storage

{{< table >}}
| Feature | Research Project Storage | Research Standard Storage |
|---|-----|-----|
| Quota | 1TB increments | 1TB increments |
| Price | $70 /TB/yr | $45 /TB/yr |
| Snapshots | Daily snapshots for 1 week | No |
| Replication | No | No |
| Backup | No | No |
| Access | Rivanna, Afton, mountable on local workstation | Rivanna, Afton, mountable on local workstation |
| Use cases | Ideal for long term storage of data to be accessed from Rivanna, sharing data within a research group, and running jobs with smaller files. | Budget solution for storing data that can be accessed by a personal computer or Rivanna. It is not recommended to run Slurm jobs against research standard storage unless absolutely necessary. File operations on Research Standard storage are slower than on Rivanna `/home`, `/scratch`, or `Research Project` storage. |
{{< /table >}}


## Checking Your Storage

To see how much disk space you have used in your home and scratch directories, open a terminal window and type `hdquota` at the command-line prompt.

```bash
hdquota
```

Example output:  
{{< figure src="/notes/hpc-best-practices/img/hdquota.png" width=70% height=70% >}}

## FAQs

__If I'm over my disk quota in either my `/home` directory or my `/scratch` directory, how can I determine my disk usage?__

You can run the following command from your `/home` or `/scratch` directory to see how your disk usage is distributed across subdirectories, and where you need to remove files. You can increase `max-depth` to go further down the directories.

```bash
du . -h --max-depth=1|sort -h -r
```

__If I'm over my file limit in `/scratch`, how can I determine where all the files are located?__

From your `/scratch` directory, run the following command to determine where you need to remove files.

```bash
find . -type f | cut -d/ -f2 | sort | uniq -c
```


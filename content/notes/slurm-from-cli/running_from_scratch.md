---
title: Running Jobs from Scratch
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1100
menu: 
    slurm-from-cli
---

We recommend that you run your jobs out of your /scratch directory.
    * Your personal /scratch/mst3k folder has much more storage space than your home directory. 
    * /scratch is on a Weka filesystem, a storage system designed specifically for fast access.
    * /scratch is connected to the compute nodes with Infiniband, a very fast network connection.

{{< alert >}}
The scratch system is not permanent storage, and files older than 90 days will be marked for deleting (purging). You should keep copies of your programs and data in more permanent locations such as your home directory, leased storage such as /project or /standard, or on your lab workstation. After your jobs finish, copy the results to permanent storage.
{{< /alert >}}

**Exercise 3**

Move or copy the hello.slurm script and the hello.py script to the new folder you created in your scratch directory in Exercise 2.  Submit hello.slurm.

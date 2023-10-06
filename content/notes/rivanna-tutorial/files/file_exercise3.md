---
title: Exercise 3
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 495

menu:
  rivanna-tutorial:
    parent: Working with Files
---

Now that we have covered the basics of OOD interactive apps, OOD functionality, and how to work with files, we will now put everything together to create a unique job submission script and run it through the job composer on OOD. In this example we will write a simple 'Hello World!' python script and a submission script to be run with the OOD job composer.

You'll need to create two files in your Desktop (```/home/computingID/Desktop```): ```hello.py``` and ```hello.slurm```. You can use any text editor of your choice: FastX editors (pluma, gedit, etc.) or the  OOD file editor. In hello.py add the following lines:

```
# Write hello 10 times
for i in range(10):
    
   print ("\n {}  Hello World!".format(i+1))

print("\n\n")
```

Next, we will need a submission script to submit this code to run on a compute node. Open ```hello.slurm``` and add the following:

```
#!/bin/bash
#SBATCH --cpus-per-task=1            
#SBATCH --mem=6000            
#SBATCH --time=00:05:00       
#SBATCH --partition=standard    
#SBATCH --account=your_allocation

module purge
module load anaconda
python hello.py
```

Be sure to replace ```your_allocation``` with the name of the allocation you have access to.

Once these two files are created, you can use the job composer on OOD to submit ```hello.slurm``` to a compute node to run the python code.

Once the job has completed, you should see a slurm-jobID.out file in your Desktop. View the file and make sure its contents are what you expect.


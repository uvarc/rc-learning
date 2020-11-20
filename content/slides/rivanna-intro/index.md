title: "Introduction to Rivanna"
slides:
  theme: "white"
---

<section data-background-image="/img/slide-bg.png">

- Rivanna is the university's resource for high-performance computing on non-sensitive data. 

- Rivanna is a _cluster_ of many _compute nodes_ behind several _login nodes_ (also called frontends).

---

Terminology

- Node

-- A node is a compute server.  Each node has its own memory.

-- Nodes are connected by networking.  Rivanna nodes are connected by conventional Ethernet and by a 
high-speed network called Infiniband.

-- Compute nodes run _jobs_ for users.

-- Login nodes, or head nodes, or frontends, can be used for logging on, editing files and other 
short tasks, and submitting jobs.

- Core

-- A core is an individual processor on a computer.  

-- Each node has multiple cores.  The number ranges from 20 to 40 depending on the age and model of the node.

---

 Illustrations
 
---
 
 Getting Started
 
 - Allocations and Accounts
 
 - Connecting to the Cluster
 
 - Cluster Environment
 
 - Modules and Partitions
 
 - SLURM and Job Submissions
 
---
 
 ALLOCATIONS
 
---
 
- Time on Rivanna is allocated.

- An allocation refers to a chunk of CPU time that you can use to run your computations.

-- Allocations are measured in **service units** (SUs).  Generally, **one SU=1 core-hour** though
specialty hardware may charge more SUs per core-hour.

- Only faculty, postdocs, and research staff may request an allocation.   

-- Students must be sponsored by a faculty or research staff.

-- All individuals on a given allocation share the service units. 

-- Allocations may be requested at 
https://www.rc.virginia.edu/userinfo/rivanna/allocations/

---

CONNECTING TO RIVANNA

---

If you are Off Grounds

- Most access requires a VPN

-- Graduate students, faculty, and staff can use the UVA More Secure Network VPN profile

-- Undergraduate students use the UVA Anywhere VPN profile

- To install the VPN on your computer, go to https://in.virginia.edu/vpn and follow the instructions.

---

Logging In To Rivanna

UVA RC provides options to log in to a frontends

1. Open OnDemand
	- Connect to a Web interface through a browser

2. FastX 
	- A "desktop" environment on a frontend
	- Accessible either as an interactive application through Open OnDemand or directly
	- https://rivanna-desktop.hpc.virginia.edu
	
3. ssh (Secure Shell)
	- Connecting through ssh requires opening a terminal window

---

Open OnDemand

- Authenticate through Netbadge

- Once logged in, you will see the Dashboard.  
- Examine and manipulate files and submit jobs
- Access applications such as JupyterLab, RStudio Server, and FastX
- https://rivanna-portal.hpc.virginia.edu

- The default is for each application to open in a new tab.  Return to the Dashboard as the "home page."

---

Screenshot

---

Interactive Apps Through OOD

Screenshot
---

Screenshot of Interactive Apps Menu

---

Example:  JupyterLab

Start JupyterLab.  Fill in the textboxes with your allocation group name, time, and memory requirements.  Click Submit. 
Your job will be queued.  When it starts, click the _launch session_ button.

Screenshot

---

FastX

FastX requires your _Eservices_ password.  This is _not_ necessarily the same as your Netbadge password.

Screenshot

---

Starting a Session

- Click on the plus sign

Screenshot

---

Starting the Desktop

Select MATE, then click Launch

Screenshot

---

FastX Desktop

Screenshot

---

SSH

- You need a _client_ program to use SSH.  This must be installed to your computer.

- Windows

	We recommend MobaXterm (link)

- Mac
	Macs ship with a Terminal app.  You should also install XQuartz (link).  To connect type
	```
	ssh -Y mst3k@rivanna.hpc.virginia.edu
	```
- When off Grounds, you must be connected to the VPN

- Open OnDemand ssh client

	The Clusters tab on OOD allows you to access a text shell.  This does not require a VPN.  It does 
	not support graphics, however.

---

Connecting to the Cluster

The generic hostname is
```
rivanna.hpc.virginia.edu
```

This is an alias for three specific frontends:
```
rivanna1.hpc.virginia.edu
rivanna2.hpc.virginia.edu
rivanna3.hpc.virginia.edu
```

You may connect using specific names.  

---

Using the Command Line

When using a ssh client you will be logged in to the _shell_.  We communicate with the shell through
a _command line_.

- Some familiarity with Unix/Linux commands will be necessary in order to navigate.

- See https://workshops.rc.virginia.edu/lesson/unix-tutorial for an introduction to Unix commands.

- Some useful commands
```
 $ls
 $pwd
 $cd _folder_  
 $cp _file1_ _file2_
 $rm _file1_
 ```
 Folder are usually called _directories_ in Unix.
 
---
 
 The Cluster Environment
 
---
 
 After Logging In
 
- You will be in your home directory.

- The home directory on Rivanna has 50GB of storage capacity.

- The home directory is for personal use and is not shareable with other users.

---

Checking Your Allocations

To see how many SUs you have available for running jobs, type allocations at the command-line prompt:
```
allocations

Allocations available to Misty S. Theatre  (mst3k):

 * robot_build: less than 6,917 service-units remaining.
 * gizmonic-testing: less than 5,000 service-units remaining.
* crow-lab: less than 2,978 service-units remaining.
 * gypsy: no service-units remaining

for more information about a specific allocation, please run:
  'allocations -a <allocation name>'
```

---

The /scratch Directory

- Each user will have access to 10 TB of temporary storage.
	It is located in a subdirectory under /scratch, and named with your userID
	e.g.,  /scratch/mst3k
	
- You are limited to 350,000 files in your scratch directory.

- The /scratch directory is for personal use and is not shareable with other users.

Important:   
 /scratch is NOT permanent storage and files that have not been accessed for more than 90 days will be marked for deletion.

---

Running Jobs from Scratch

We recommend that you run your jobs out of your /scratch directory for two reasons:
- /scratch is on a Lustre filesystem (a storage system designed specifically for parallel access).
- /scratch is connected to the compute nodes with Infiniband (a very fast network connection). 

We also recommend that
- You keep copies of your programs and data in more permanent locations (e.g., your home directory or leased storage).

- After your jobs finish, you copy the results to more permanent storage).

---

Checking Your Storage

To see how much disk space you have used in your home and scratch directories, open a Terminal window and 
type `hdquota` at the command-line prompt:

Screenshot

---

Moving Data To and From Rivanna

You have several options for transferring data onto your home or /scratch directories.
1. Use the scp command in a terminal window.
2. Use a drag-and-drop option with MobaXterm (Windows) or Fugu (Mac OS).
3. Use the web browser in the FastX desktop to download data from UVA Box.
4. Use the git clone command to copy git repositories
5. Set up a Globus endpoint on your laptop and use the Globus web interface to transfer files.  
	(See https://www.rc.virginia.edu/userinfo/globus/ for details)

---

MODULES

---

Modules set up your environment to make it easier for you to use software packages.

- Any application software that you want to use will need to be loaded with the `module load` command.  
For example:
```
module load matlab
module load anaconda/5.2. 0-py3.6
module load gcc R/3.5.1
```

- You will need to load the module any time that you create a new shell
-- Every time that you log out and back in
-- Every time that you run a batch job on a compute node

---

More Module Commands

- `module avail` – Lists all available modules and versions.

- `module spider` – Shows all available modules

- `module key _keyword_` – Shows modules with the keyword in the description

- `module list` – Lists modules loaded in your environment.

- `module load mymod` – Loads the default module to set up the environment for some software.
- `module load mymod/N.M` – Loads a specific version N.M of software mymod.
- `module load _compiler_ _mpi_ _mymod_` – For compiler- and MPI- specific modules, loads the modules in the appropriate order and, optionally, the version.

- `module purge` – Clears all modules.

---

Finding Modules

To locate a python module, try the following:
```
$ module avail python

$ module spider python

$ module key python
```
To find bioinformatics software packages, try this:
```
$ module key bio
```
The available software is also listed on our website:
	https://www.rc.virginia.edu/userinfo/rivanna/software/complete-list/
	
---

SLURM

---

SLURM is a *resource manager*, also called a *queueing system*.  To use a resource manager, you prepare
a script which describes the resources you are requesting and specifies how to run your program.  The 
script, often called a _job script_ or a _submit script_, is **submitted** to a queue, which SLURM calls
a _partition_.  The scheduler determines the priority of your job and submits it to a compute node 
when resources are available.

- The frontends are for use only for editing, compiling, and very short test runs.  Limits on
memory and core usage are enforced.
- Production jobs are submitted to the compute node through SLURM.

For details please see our SLURM pages at http://www.rc.virginia.edu/userinfo/rivanna/slurm
Much more information is available at http://slurm.schedmd.com/documentation.html

---

Partitions

Rivanna has several partitions (or queues) for job submissions.
You will need to specify a partition when you submit a job.
To see the partitions that are available to you, type `qlist` at the command-line prompt.

```
$ qlist

Queue	   Total   	Free    	Jobs    	Jobs    	Time           SU
(partition)    Cores   	Cores   	Running 	Pending 	Limit          Charge
=====================================================================
bii                   200          180                0                0                7-00:00:00     1
Standard       200          180                0                0                7-00:00:00     1
Dev                200          180                0                0                   1:00:00        0
Parallel          200          180                0                0                3-00:00:00     1
largemem     200          180                0                0                4-00:00:00     1
gpu                  28            28                 0                0                3-00:00:00     3
knl                 200          180                 0                0                3-00:00:00     1
```

---

Compute Node Partitions

Name  | Purpose | Job Time Limit  |  Memory/Node  | Cores/Node  |
-----------------------------------------------------------------
standard | Jobs on one compute node | 7 days | 256 GB/ 384GB | 28/40 |
gpu  |  Jobs that can use general-purpose graphical processing units (GPUS) | 3 days | 256GB | 28 |
parallel | For multi-node parallel jobs up to 120 nodes | 3 days | 128 GB | 20 |
largemem | For memory-intensive jobs | 4 days | 1 TB  | 16 |
dev | To run short test jobs | 1 hour | 128 GB | 4 |
-----------------------------------------------------------------

SLURM Scripts
A SLURM script is a bash script with SLURM directives (#SBATCH) and command-line instructions 
for running your program.
```
#!/bin/bash
#SBATCH --nodes=1             #total number of nodes for the job
#SBATCH --ntasks=1            #how many copies of code to run 
#SBATCH --time=1-12:00:00     #amount of time for the whole job
#SBATCH --partition=standard  #the queue/partition to run on
#SBATCH --account=myGroupName #the account/allocation to use

module purge
module load gcc R             #load modules that my job needs
Rscript myProg.R              #command-line execution of my job
```

---

Submitting a SLURM Job

To submit the SLURM command file to the queue, use the `sbatch` command at the command line prompt.

For example, if the script on the previous slide is in a file named job_script.slurm, we can submit it as
follows:

```
-bash-4.1$ sbatch job_script.slurm
Submitted batch job 18316
```
The system responds with the **job ID** number.  

---

Checking Your Job Status

To display the status of only your active jobs,  type:    
``` 
squeue –u <your_user_id>
```
The squeue command will show pending jobs and running jobs, but not failed, canceled or completed jobs.

Typing `squeue` alone shows all jobs in all partitions.

Typing `squeue -p <partition>` shows jobs in the specified partition.

---

Job Status Summaries

To display the status of all jobs,  type:
``` 
sacct –S <start_date>
```

Example
```
-bash-4.2$ sacct –S 2019-01-29
 
3104009      RAxML_NoC+   standard  hpc_build         20  COMPLETED      0:0 
3104009.bat+      batch             hpc_build         20  COMPLETED      0:0 
3104009.0    raxmlHPC-+             hpc_build         20  COMPLETED      0:0 
3108537      sys/dashb+        gpu  hpc_build          1 CANCELLED+      0:0 
3108537.bat+      batch             hpc_build          1  CANCELLED     0:15 
3108562      sys/dashb+        gpu  hpc_build          1    TIMEOUT      0:0 
3108562.bat+      batch             hpc_build          1  CANCELLED     0:15 
3109392      sys/dashb+        gpu  hpc_build          1    TIMEOUT      0:0 
3109392.bat+      batch             hpc_build          1  CANCELLED     0:15 
3112064            srun        gpu  hpc_build          1     FAILED      1:0 
3112064.0          bash             hpc_build          1     FAILED      1:0
```

The `sacct` command lists all jobs (pending, running, completed, canceled, failed, etc.) since the specified date.

---

Canceling a Job

To delete a job from the queue, use the scancel command with the job ID number at the command line prompt:
```
-bash-4.2$ scancel 18316
```

To cancel all your jobs, type
```
-bash-4.2$ scancel –u $USER
```

---

Need Help?

Research Computing Zoom Office Hours
Tuesdays:	3 pm – 5 pm
Wednesdays:	3 pm – 5 pm
Thursdays:	10 am – noon

Or, contact us through the forms at:
	https://www.rc.virginia.edu/support/

---

</section>
-->
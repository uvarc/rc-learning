---
title: "WR Introduction to Rivanna"
summary: WR An introduction to using the Rivanna system
authors: [uvarc]
date: "2020-04-13T00:00:00Z"
slides:
  theme: white
  highlight_style: github
---

# Introduction to Rivanna

<div class=twocolumn>
 <div>
  <ul>
   <li>Rivanna is UVA's resource for high-performance computing on non-sensitive data</li>
   <li>Rivanna is a cluster of many compute nodes behind several frontends</li>
  </ul>
 </div>
 <div>
    <img src="RC_logo.png">
 </div>



---

{{< slide background-image="/slides/rivanna-intro/rivanna.jpg" background-opacity=0.3 >}}
 
## Getting Started

{{% fragment %}} Hardware {{% /fragment %}}
<br>
{{% fragment %}} Allocations {{% /fragment %}}
<br>
{{% fragment %}} Connecting to the Cluster {{% /fragment %}}
<br>
{{% fragment %}} Using the Frontends {{% /fragment %}}

---
{{< slide background-image="/slides/rivanna-intro/racks.jpg" background-opacity=0.3 >}}
 
## Hardware

---

### Nodes

<div class=twocolumn>
 <div>
  <ul>
   <li>A node is a compute server (think of it as a computer)</li>
   <li>Nodes are connected by networks</li>
   <li>Frontends are used for logging in, editing files, and submitting jobs</li>
   <li>Compute nodes run jobs</li>
  </ul>
 </div>
 <div>
    <img src="Rivanna_overview.png">
 </div>

---

### Cores

- A core is an individual processor on a computer  
- Each node has multiple cores  
    - Our nodes have between 20-40 cores depending on the age and model of the node

{{< figure src="cluster-sketch.jpg" >}}

---

{{< slide background-image="/slides/rivanna-intro/racks.jpg" background-opacity=0.3 >}}
 
## Allocations
 
---

### What is an Allocation
 
- Time on Rivanna is allocated
- An allocation refers to a chunk of CPU time that you use to run your computations
- Allocations are measured in **Service Units** (SUs)
    > Generally, **1 SU = 1 core-hour** 
    - specialty hardware may charge more SUs per core-hour
- Only faculty, postdocs, and research staff may request an allocation   
    - Students must be sponsored
    - All individuals on a given allocation share the service units 
- Allocations may be requested at 
https://www.rc.virginia.edu/userinfo/rivanna/allocations/

---

{{< slide background-image="/slides/rivanna-intro/racks.jpg" background-opacity=0.3 >}}
## Connecting To Rivanna

---

### Off Grounds

- When off grounds, use the VPN
    - Graduate students, faculty, and staff use: UVA More Secure Network 
    - Undergraduate students use: UVA Anywhere 

- To install the VPN, go to https://in.virginia.edu/vpn 

---

### Logging In To Rivanna

There are multiple ways to log in

1. Open OnDemand
	- Connect to a Web interface through a browser
2. FastX 
	- A "desktop" environment on a frontend
3. ssh (Secure Shell)
	- Connecting through ssh requires opening a terminal window

---

#### OpenOnDemand

<div class=twocolumn>
 <div>
  <ul>
   <li>OpenOnDemand (OOD) can be accessed at: https://rivanna-portal.hpc.virginia.edu</li>
   <li>Once you authenticate with your Netbadge, you will see the Dashboard</li>
   <li>OOD allows you to examine and manipulate files, submit jobs, and run interactive applications</li>
   <li>Interactive applications include: JupyterLab, RStudio Server, Matlab, and FastX</li>
  </ul>
 </div>
 <div>
    <img src="OOD_dashboard.png">
 </div>



---

#### Open OnDemand Dashboard
{{< figure src="OOD_dashboard.png" >}}

---

##### Interactive Apps Through OOD

{{< figure src="OOD_apps.png" >}}

---

##### Launch JupyterLab

    <img src="OOD_Jupyterlab.png", style="height:30vh; background-color:white; float:right;"/>

- Fill in the textboxes with your parition, time, memory/core requirements, and allocation
- Click submit and your job will be queued as it waits for resources
- When it starts, click the Launch Session button





---

#### JupyterLab Setup Page

{{< figure src="OOD_Jupyterlab.png" >}}

---

#### FastX

FastX is accessible either as an interactive application through Open OnDemand 
or directly at https://rivanna-desktop.hpc.virginia.edu

FastX requires your _Eservices_ password.  This is _not_ necessarily the same as your Netbadge password.

{{< figure src="FastX_login.png" >}}

---

##### Starting a FastX Session

- Click on the plus sign

{{< figure src="FastX_session_setup.png" >}}

---

##### Starting the Desktop

- Select MATE, then click Launch

{{< figure src="FastX_launch_MATE.png" >}}

---

##### FastX Desktop

{{< figure src="FastX_desktop.png" >}}

---

##### Terminating FastX

- You may go to System->Log Out
   When you confirm logging out, this will terminate your session

- You can also go to the FastX login page. Each session should appear as a terminal icon.  Select the session and click the `x` in the upper-right corner.
Or go to the blue menu icon and select Terminate.

{{< figure src="FastX_terminate.png" >}}

###  After Logging In
 
- You will be in your home directory (folder).

- The home folder on Rivanna has 50GB of storage capacity.

- The home folder is for personal use and is not shareable with other users.

- "Snapshots" of your files over the last seven days are available.
    - This is the only backup provided.
 
---

### Restrictions

- The frontend nodes are for short "housekeeping" tasks such as
   - Writing your programs or scripts
   - Compiling programs
   - Submitting jobs

- You may run _very short_ test runs with a limited number of cores and amount of memory. Your process will be terminated if it exceeds the time or memory limit.

- You may _not_ run multiple processes at once, nor may you run production jobs.

---

{{< slide background-image="/slides/rivanna-intro/rivanna.jpg" background-opacity=0.3 >}}
 
## The Cluster Environment
 
{{% fragment %}} Storage Options {{% /fragment %}}
<br>
{{% fragment %}} Command-Line and Modules {{% /fragment %}}
<br>
{{% fragment %}} SLURM and Job Submissions {{% /fragment %}}

---

{{< slide background-image="/slides/rivanna-intro/racks.jpg" background-opacity=0.3 >}}
### Storage Options

---

#### The /scratch Folder

- Each user will have access to 10 TB of temporary storage.
  It is located in a subfolder under /scratch, and named with your userID
	e.g.,  /scratch/mst3k
	
- You are limited to 350,000 files in your scratch area.

- Your /scratch folder is for personal use and is not shareable with other users.

**Important**
 /scratch is NOT permanent storage and files that have not been accessed for more than 90 days will be marked for deletion.

Your files on scratch are NOT backed up.  Deleted files are not recoverable.

---

#### Running Jobs from Scratch

- We recommend that you run your jobs out of your /scratch folder for two reasons:
   - /scratch is on a Lustre filesystem (a storage system designed specifically for parallel access).
   - /scratch is connected to the compute nodes with Infiniband (a very fast network connection). 

- We also recommend that
   - You keep copies of your programs and data in more permanent locations (e.g., your home folder or leased storage).

   - After your jobs finish, copy the results to more permanent storage.

---

#### Leased Storage

- Two options are available for a monthly fee.  Access is through groups that may but are not required to correspond to Rivanna allocation groups. All members of the group can access the storage, but not necessarily individual folders.

  - Project
    - Fast storage 
    - Seven-day snapshots
  - Value
    - Slower storage
    - No snapshots

---

#### Moving Data To and From Rivanna

1. Use the scp command in a terminal window (Mac and Linux).
2. Install a drag-and-drop option. We recommend [MobaXterm](https:https://mobaxterm.mobatek.net/) (Windows) or [Filezilla](https://filezilla-project.org) (MacOS).
   - In MobaXterm, start an SCP session
   - In Filezilla, start an SFTP session
3. For small files, use the Upload and Download buttons in the Open OnDemand file manager.
4. Use the web browser in the FastX desktop to download data from UVA Box.
5. Use the `git clone` command to copy git repositories.
6. Set up a Globus endpoint on your laptop and use the Globus web interface to transfer files. 
   - (See the [RC Globus page](https://www.rc.virginia.edu/userinfo/globus/) for details)

More details are available at [our data-transfer page]( https://www.rc.virginia.edu/userinfo/data-transfer/)

---

#### Checking Your Storage

To see how much disk space you have used in your home and scratch directories, open a Terminal window (see next few slides) and
type `hdquota` at the command-line prompt:
```bash
Type     Location      Name        Size Used Avail Use%
================================================================================
home      /home       mst3k         50G   45G  5.6G  89%
Project   /project    servo_lab    1.8P  1.7P  117T  94%
Value     /nv         vol67       1000G   84G  917G   9%

Location         Age_Limit(Days) Disk_Limit(GB) Use(GB)  File_Limit   Use
================================================================================
/scratch/mst3k       90              10240        147      350000      28387
```

---

{{< slide background-image="/slides/rivanna-intro/racks.jpg" background-opacity=0.3 >}}
### MODULES

---

#### Using a Command Line

Modules and SLURM require that you type a few commands from the command line.

Learning to use the command line and a text editor will make you a more productive user of Rivanna.  Please see our _Using Rivanna from the Command Line_ [tutorial](/slides/rivanna-cli) for an introduction.

In the examples below, the characters up to and included the `$` are the _prompt_.  Do not type them.  The prompt may differ for different users.  It indicates that the system is ready for input.

---

#### Starting a Command-Line Environment

1. From the OOD Dashboard menu, go to Clusters->Rivanna
   A new tab will open with a prompt.
2. Log in to FastX Web and start a terminal from the menu 
   Applications->System Tools->MATE terminal
   Or click the terminal icon in the taskbar at the top.
3. For more advanced users, use your local ssh client to log in.

---

#### Editing Files

- In OOD, select a file in the Files tab and click the `Edit` button.
- In FastX, select Pluma Text Editor from the Applications->Accessories
  -  You may also use Emacs or Vi Improved

---

#### Checking Your Storage

To see how much disk space you have used in your home and scratch directories, open a Terminal window and
type `hdquota` at the command-line prompt:
```bash
Type     Location      Name        Size Used Avail Use%
================================================================================
home      /home       mst3k         50G   45G  5.6G  89%
Project   /project    servo_lab    1.8P  1.7P  117T  94%
Value     /nv         vol67       1000G   84G  917G   9%

Location         Age_Limit(Days) Disk_Limit(GB) Use(GB)  File_Limit   Use
================================================================================
/scratch/mst3k       90              10240        147      350000      28387
```

#### Modules Modify Your Environment
Modules set up your environment to make it easier for you to use software packages.

- Any application software that you want to use will need to be loaded with the `module load` command.  
For example:
```bash
bash-4.2$module load matlab
bash-4.2$module load anaconda/5.2.0-py3.6
bash-4.2$module load goolf/7.1.0_3.1.4 R/3.6.3
```
- You will need to load the module any time that you start a new terminal.
  - Every time that you log out and back in.
  - Every time that you run a batch job on a compute node.

---

#### More Module Commands

- `module avail` – Lists all available modules and versions.
- `module spider` – Shows all available modules
- `module key _keyword_` – Shows modules with the keyword in the description
- `module list` – Lists modules loaded in your environment.
- `module load mymod` – Loads the default module to set up the environment for some software.
- `module load mymod/N.M` – Loads a specific version N.M of software mymod.
- `module load _compiler_ _mpi_ _mymod_` – For compiler- and MPI- specific modules, loads the modules in the appropriate order and, optionally, the version.
- `module purge` – Clears all modules.

---

#### Finding Modules

To locate a python module, try the following:
```bash
bash-4.2$ module avail python
bash-4.2$ module spider python
bash-4.2$ module key python
```
To find bioinformatics software packages, try this:
```bash
bash-4.2$ module key bio
```
The available software is also listed on our website:
<br>
	https://www.rc.virginia.edu/userinfo/rivanna/software/complete-list/
	
---

{{< slide background-image="/slides/rivanna-intro/racks.jpg" background-opacity=0.3 >}}
### SLURM

---

####  What is SLURM

SLURM is a *resource manager*, also called a *queueing system*.  

- To use a resource manager, you prepare a script which describes the resources you are requesting and specifies how to run your program.  
- The script, often called a _job script_ or a _submit script_, is **submitted** to a queue, which SLURM calls
a _partition_.  
- The scheduler determines the priority of your job and submits it to a compute node 
when resources are available.

For details and sample scripts, please see our SLURM pages at http://www.rc.virginia.edu/userinfo/rivanna/slurm
Much more information is available at http://slurm.schedmd.com/documentation.html

---

#### Partitions

- Rivanna has several partitions (or queues) for job submissions.
- You will need to specify a partition when you submit a job.
- To see the partitions, type `qlist` at the command-line prompt.  Not all users have access to all partitions.

---

#### Partition (Queue) Properties

```bash
bash-4.2$ qlist

Queue          Total   Free    Jobs    Jobs    Time           SU     
(partition)    Cores   Cores   Running Pending Limit          Charge 
======================================================================
bii            4600    3254    59      647     7-00:00:00     1      
standard       6004    2569    898     394     7-00:00:00     1      
dev            216     188     5       1       1:00:00        0      
parallel       5760    4328    23      0       3-00:00:00     1      
instructional  560     460     1       0       3-00:00:00     1      
largemem       80      16      17      96      4-00:00:00     1      
gpu            472     262     38      35      3-00:00:00     3      
bii-gpu        320     280     1       0       3-00:00:00     1      
knl            2048    1280    0       0       3-00:00:00     1      
pcore          144     44      2       0       infinite       1      
```
---

#### Compute Node Partitions

|Name  | Purpose | Job Time Limit  |  Memory/Node  | Cores/Node  |
|---- | ---- | ---- | ---- | ---- | ---- |
|standard | Single compute node | 7 days | 256 GB/ 384GB | 28/40 |
|gpu  |  GPU jobs  | 3 days | 256GB | 28 |
|parallel | Multi-node parallel | 3 days | 384 GB | 40 |
|largemem | Memory-intensive jobs | 4 days | 1 TB/1.5 TB  | 16 |
|dev | Short test jobs | 1 hour | 256 GB/ 128 GB | 28/20 |

---

#### Limits on Partitions

```bash
bash-4.2$qlimits
Queue          Maximum      Maximum      Minimum      Maximum       Maximum       Default       Maximum      Minimum     
(partition)    Submit       Cores/User   Cores/Job    Mem/Node(MB)  Mem/Core(MB)  Mem/Core(MB)  Nodes/Job    Nodes/Job   
========================================================================================================================
bii            10000        400                       384000                      8500          112                      
standard       10000        1000                      112000+                     9000          1                        
dev            10000        16                        127000+       9000          9000          2                        
parallel       2000         900                       384000        9000          6000          45           2           
instructional  50           8                         127000                      1000          5                        
largemem       10000        32                        1000000       64000         60000         2                        
gpu            10000        16                        188000+       32000         6000          4                        
bii-gpu        10000                                  384000                      8500          8                        
knl            2000         900          16           180000                      768           8                        
pcore          10000                                  550000                      2000          16             
```

---

#### Checking Your Allocations

- To see how many SUs you have available for running jobs, type `allocations` at the command-line prompt (represented here by `-bash-4.2$`:
=======
To see how many SUs you have available for running jobs, type `allocations` at the command-line prompt (represented here by `-bash-4.2$`):
```bash
bash-4.2$allocations

Allocations available to Misty S. Theatre  (mst3k):

 * robot_build: less than 6,917 service-units remaining.
 * gizmonic-testing: less than 5,000 service-units remaining.
 * crow-lab: less than 2,978 service-units remaining.
 * gypsy: no service-units remaining
```

- For more information about a specific allocation, represented here by `allocation_name`, please run:
```bash
bash-4.2$allocations -a allocation_name
```

---

#### SLURM Scripts
A SLURM script is a bash shell script with SLURM directives (#SBATCH) and command-line instructions for running your program.
```bash
#!/bin/bash
#SBATCH --nodes=1                 #total number of nodes for the job
#SBATCH --ntasks=1                #how many copies of code to run 
#SBATCH --time=1-12:00:00         #amount of time for the whole job
#SBATCH --partition=standard      #the queue/partition to run on
#SBATCH --account=myGroupName     #the account/allocation to use

module purge
module load goolf/7.1.0_3.1.4 R   #load modules that my job needs
Rscript myProg.R                  #command-line execution of my job
```

---

### Submitting a SLURM Job

To submit the SLURM command file to the queue, use the `sbatch` command at the command line prompt.

For example, if the script on the previous slide is in a file named job_script.slurm, we can submit it as
follows:
```bash
bash-4.2$ sbatch job_script.slurm
Submitted batch job 18316
```
The system responds with the **job ID** number.  

---

### Checking Your Job Status

To display the status of only your active jobs,  type:    
```bash 
bash-4.2$squeue –u <your_user_id>
```
The squeue command will show pending jobs and running jobs, but not failed, canceled or completed jobs.

Typing `squeue` alone shows all jobs in all partitions.

Typing `squeue -p` <partition> shows jobs in the specified partition.

---

### Job Status Summaries

To display the status of all jobs,  type:
```bash
bash-4.2$sacct –S <start_date>
```

The `sacct` command lists all jobs (pending, running, completed, canceled, failed, etc.) since the specified date.

---

#### Example Summary

```bash
bash-4.2$ sacct –S 2019-01-29
 
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
---

### Canceling a Job

To delete a job from the queue, use the scancel command with the job ID number at the command line prompt:
```bash
bash-4.2$ scancel 18316
```

To cancel all your jobs, type
```bash
bash-4.2$ scancel –u $USER
```

---

{{< slide background-iframe="https://www.rc.virginia.edu" >}}

<h2><span style="color:orange">Need Help?</span></h2>

---

### Visit Us Online

Research Computing Zoom Office Hours
- Tuesdays: 3 pm – 5 pm
- Thursdays: 10 am – noon

[Contact us](https://www.rc.virginia.edu/support/).

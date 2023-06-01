# Introduction to Rivanna

Rivanna is the university's primary resource for high\-performance computation\. It provides a platform for computationally\-intensive research across disciplines\.

![](img/rivanna.png)

![](img/IntroductionToRivanna_2022_10_171.jpg)

![](img/IntroductionToRivanna_2022_10_172.jpg)

# Terminology

![](img/IntroductionToRivanna_2022_10_173.wmf)

* Node
  * Basic building block of a cluster
  * Usually a specialized computer
* Two types of nodes:
    * <span style="color:#0000FF">Head Node </span> – computer used for logging on and submitting jobs
    * <span style="color:#0000FF">Compute Node </span> \-\-  computer that does most of the work
* Core – an individual processor on a computer

# Rivanna Overview

![](img/IntroductionToRivanna_2022_10_174.wmf)

![](img/IntroductionToRivanna_2022_10_175.jpg)

![](img/IntroductionToRivanna_2022_10_176.png)

# Before we can use the Cluster . . .

* We need to know about:
  * [Allocations & Accounts](slide6.xml)
  * [Connections to the cluster](slide9.xml)
  * [Cluster environment](slide25.xml)
  * [Modules & Partitions](slide34.xml)
  * [Slurm & Job Submissions](slide41.xml)
* The remaining slides will cover
* the basics of these topics\.

![](img/IntroductionToRivanna_2022_10_177.jpg)

# Allocations

# 

# Allocations

* Rivanna is allocated:
  * At the most basic level\, an allocation refers to a chunk of CPU time that you can use to run your computations\.
* Only faculty\, postdocs\, and research staff may request an allocation\.
  * Students must be sponsored by a faculty or research staff\.
  * All individuals on a given allocation share the service units\.
* Allocations may be requested at      	[https://www\.rc\.virginia\.edu/userinfo/rivanna/allocations/](https://www.rc.virginia.edu/userinfo/rivanna/allocations/)

  * Allocations are measured in service units \(SUs\)\, where  __1 SU = 1 core\-hour__

# Getting an account on Rivanna

![](img/IntroductionToRivanna_2022_10_178.jpg)

![](img/IntroductionToRivanna_2022_10_179.png)

Request an Allocation

Receive Allocation and Account\(s\)

Research Computing

![](img/IntroductionToRivanna_2022_10_1710.png)

Principal Investigator \(PI\)

# Connecting & Logging Onto Rivanna

# 

# If You are Off-Grounds . . .

You may need to use the UVa VPN when connecting to Rivanna from an off\-Grounds location\.

To install the VPN on your computer\, go to [https://in\.virginia\.edu/vpn](https://in.virginia.edu/vpn) and follow the instructions\.

During the COVID\-19 outbreak\, we are asking users to connect with the  _UVa_  _ More Secure_  VPN if it is available to them\.

The  _UVa_  _ Anywhere _ VPN can be used if the UVA More Secure Network is not available\.

# How to connect to Rivanna

* There are three ways to connect to Rivanna:
  * Open\-on\-Demand\, a graphical user interface through a web browser
    * examine and manipulate files and submit jobs\.
    * access applications such as Matlab\, Jupyterlab\, and R Studio Server\.
    * [https://rivanna\-portal\.hpc\.virginia\.edu](https://rivanna-portal.hpc.virginia.edu/)
  * FastX Web\, direct access to a desktop for Rivanna[https://rivanna\-desktop\.hpc\.virginia\.edu](https://rivanna-desktop.hpc.virginia.edu/)
  * 3\.   ssh client\, direct access to the command line

# Connecting to Open OnDemand

* To connect to Open OnDemand\, open your web browser and type
    * [https://rivanna\-portal\.hpc\.virginia\.edu](https://rivanna-portal.hpc.virginia.edu/)
    * You will need to authenticate with Netbadge \(i\.e\.\, “Netbadge” in\)\.
* After you log in\, you will see the Dashboard\.
* You can connect to Open OnDemand from off\-Grounds locations without a VPN connection\.

# Open OnDemand Dashboard

![](img/IntroductionToRivanna_2022_10_1711.png)

![](img/IntroductionToRivanna_2022_10_1712.png)

# Interactive Apps

Let’s try FastX for now\.

You must use the UVa VPN to use FastX from off\-Grounds\.

# Log in Using Your Netbadge Credential

If this page does not appear\, make sure that you are connected to the UVa VPN\.\.

# Starting up FastX

Click on the plus sign

![](img/IntroductionToRivanna_2022_10_1713.png)

Select MATE; Click Launch

![](img/IntroductionToRivanna_2022_10_1714.png)

# FastX Desktop

# Now, let’s see how we can connect with an ssh client.

# SSH Clients

Replace mst3k with your user ID\.

* Connecting to Rivanna
  * On a Mac or Linux system\, simply open a Terminal application and type
  * ssh –Y mst3k@rivanna\.hpc\.virginia\.edu
  * You may also want to install XQuartz on your Mac to use graphical tools on Rivanna\.
  * On a Windows system\, open the Command Prompt app and type
  * ssh –Y mst3k@rivanna\.hpc\.virginia\.edu

You must use the UVa VPN when off\-Grounds\.

    * SSH clients mostly use instructions typed at the command prompt\.
    * You will need to become familiar with Linux/Unix commands\.
      * See  _[learning\.rc\.virginia\.edu/tutorials/unix\-tutorial/](http://learning.rc.virginia.edu/tutorials/unix-tutorial/)_  to learn more
* Some useful Unix/Linux commands:

    * $ ls
    * $ pwd
    * $ cd  _folder\_name_
    * $ cp file\_1 file\_2
    * $ rm file\_1
    * $ cd \.\.

# Hostnames for Rivanna

* The hostname for the Interactive frontends:  	 <span style="color:#C00000">rivanna\.hpc\.virginia\.edu</span>  <span style="color:#C00000"> </span>
* <span style="color:#C00000">  </span> \(does round\-robin among the three front\-ends\)
* However\, you also can log onto a specific front\-end:
  * <span style="color:#C00000">rivanna1\.hpc\.virginia\.edu</span>
  * <span style="color:#C00000">rivanna2\.hpc\.virginia\.edu</span>
  * <span style="color:#C00000">rivanna3\.hpc\.virginia\.edu</span>

# Editing Files

# 

# How to edit a file on Rivanna

* You can use:
  * The built\-in editor in Open\-on\-Demand\. Just click on Files on the Dashboard\, highlight the file that you want to edit and click on the edit button\.
  * When in a Terminal Window\, use vi or nano\.
    * vi uses all keyboard shortcuts and may take a while to learn\.
    * nano is a very basic editor\.  To launch\, type: nano myfile\.txt
  * In FastX\, you can use gedit or any of your IDEs\, like Spyder or Rstudio\.

# Cluster Environment

# 

# After you have logged in . . .

You will be in your home directory\, e\.g\. /home/mst3k\.

The home directory on Rivanna has 50GB of storage capacity\.

The home directory is for personal use and is not shareable with other users\.

# Checking your Allocation

* To see how many SUs you have available for running jobs\, type  <span style="color:#0070C0"> __allocations__ </span>  at the command\-line prompt:

    * $ allocations
  * Account                    Balance          Reserved           Available
  * \-\-\-\-\-\-\-\-\-\-\-\-\-\-               \-\-\-\-\-\-\-\-\-\-\-         \-\-\-\-\-\-\-\-\-\-\-\-\-           \-\-\-\-\-\-\-\-\-\-\-\-
  * robot\_build                   25000                     0                 25000
  * gizmonic\-testing       921\.952                     0              921\.952
  * crow\-lab                     928826               6236               922590
  * gypsy                                      0                      0                          0
  * for more information about a specific allocation\, run:
  * 'allocations \-a \<allocation name>‘
  * allocations –a crow\-lab

# Your /scratch Directory

* Each user will have access to 10 TB of  <span style="color:#0070C0"> __temporary__ </span>  storage\.
  * It is located in a subdirectory under /scratch\, and named with your userID
  * e\.g\.\,  cd /scratch/mst3k
  * You are limited to 350\,000 files in your scratch directory\.
  * The /scratch directory is for personal use and is not shareable with other users\.

__Important:   __

/scratch is  <span style="color:#FF0000"> __NOT__ </span>  <span style="color:#FF0000"> </span>  <span style="color:#FF0000"> __permanent __ </span> storage and files that have not been accessed for more than  <span style="color:#FF0000"> __90 days __ </span> will be marked for deletion\.

# Running Jobs from Scratch

* We recommend that you run your jobs out of your /scratch directory for two reasons:
    * /scratch is on GPFS \(a storage system designed specifically for parallel access\)\.
    * /scratch is connected to the compute nodes with Infiniband \(a very fast network connection\)\.

* We also recommend that
  * You keep copies of your programs and data in more permanent locations \(e\.g\.\, your home directory or leased storage\)\.
  * After your jobs finish\, you copy the results to more permanent storage\)\.

# Checking your Storage

* To see how much disk space you have used in your home and scratch directories\, open a Terminal window and type  <span style="color:#0070C0"> __hdquota__ </span>  at the command\-line prompt:

    * $ hdquota
  * Type             Location         Name                   Size   Used   Avail   Use%
  * ===============================================================================
  * home             /home           mst3k                   50G  472M  50G   1%
  * Location        Age\_Limit\(Days\)       Disk\_Limit\(GB\)         Use\(GB\)             File\_Limit          Use
  * ====================================================================================
  * /scratch/mst3k           90                      10240                       0                      350000               3

# Moving data onto Rivanna

* You have several options for transferring data onto your home or /scratch directories\.
  * Use the scp command in a terminal window\.
  * Use a drag\-and\-drop option with MobaXterm \(Windows\) or Cyberduck \(Mac OS\)\.
  * For small files\, use the Upload and Download buttons in the Open onDemand Files App
  * Use the web browser in the FastX desktop to download data from UVA Box\.
  * Use the git clone command to copy git repositories
  * Set up a Globus endpoint on your laptop and use the Globus web interface to transfer files\.
  * \(See [https://www\.rc\.virginia\.edu/userinfo/globus/](https://www.rc.virginia.edu/userinfo/globus/) for details\)

# Modules

# 

# Modules

* Any application software that you want to use will need to be loaded with the  <span style="color:#0070C0"> __module load __ </span> command\.
* For example:
    * module load matlab
    * module load anaconda/2020\.11\-py3\.8
    * module load goolf/7\.1\.0\_3\.1\.4 R/4\.0\.0
* You will need to load the module any time that you create a new shell
    * Every time that you log out and back in
    * Every time that you run a batch job on a compute node

# Module Details

  * <span style="color:#0070C0">module avail </span> – Lists all available modules and versions\.
  * <span style="color:#0070C0">module spider</span>  – Shows all available modules
  * <span style="color:#0070C0">module key keyword </span> – Shows modules with the keyword in the description
  * <span style="color:#0070C0">module list </span> – Lists modules loaded in your environment\.
  * <span style="color:#0070C0">module load </span>  <span style="color:#0070C0">mymod</span>  <span style="color:#0070C0"> </span> – Loads the default module to set up the environment for some software\.
    * module load mymod/N\.M – Loads a specific version N\.M of software mymod\.
    * module load compiler mpi mymod – For compiler\- and MPI\- specific modules\, loads the modules in the appropriate order and\, optionally\, the version\.
  * <span style="color:#0070C0">module purge</span>  <span style="color:#0070C0"> </span> – Clears all modules\.

# Learning more about a Module

* To locate a python module\, try the following:
* To find bioinformatics software packages\, try this:
* The available software is also listed on our website:
* [https://www\.rc\.virginia\.edu/userinfo/rivanna/software/complete\-list/](https://www.rc.virginia.edu/userinfo/rivanna/software/complete-list/)

    * $ module avail python
    * $ module spider python
    * $ module key python

    * $ module key bio

# Partitions (Queues)

# 

# Partitions (Queues)

* Rivanna has several partitions \(or queues\) for job submissions\.
    * You will need to specify a partition when you submit a job\.
    * To see the partitions that are available to you\, type  <span style="color:#0070C0"> __qlist__ </span>  at the command\-line prompt\.

* $ qlist
* Queue	   Total   	Free    	Jobs    	Jobs    	           Time           SU
* \(partition\)    Cores   Cores   	Running 	Pending 	           Limit          Charge
* =================================================================
* bii                   200          180                0                0                7\-00:00:00     1
* standard       200          180                0                0                7\-00:00:00     1
* dev                200          180                0                0                   1:00:00        0
* parallel          200          180                0                0                3\-00:00:00     1
* largemem     200          180                0                0                4\-00:00:00     1
* gpu                  28            28                 0                0                3\-00:00:00     3

# Compute Node Partitions (aka Queues)

| Queue Name | Purpose | Job Time Limit | Memory / Node | Cores / Node |
| :-: | :-: | :-: | :-: | :-: |
| standard | For jobs on a single compute node | 7 days | 256 GB<br />384 GB<br />768 GB | 28<br />40<br />40 |
| gpu | For jobs that can use general purpose graphical processing units (GPGPUs)<br /> (K80 or P100) | 3 days | 256 GB<br />384 GB<br />1 TB | 28<br />40<br />128 |
| parallel | For large parallel jobs on up to 25 nodes (<= 1000 cores/job) | 3 days | 384 GB | 40<br /> |
| largemem | For memory intensive jobs (<= 16 cores/node) | 4 days | 1 TB | 16 |
| dev | To run jobs that are quick tests of code (<= 2 nodes, 8 cores/node) | 1 hour | 128 GB |  4  |

# SLURM scripts

# 

# Slurm

* Slurm manages the hardware resources on the cluster \(e\.g\. compute nodes/cpu cores\, compute memory\, etc\.\)\.
* Slurm allows you to request resources within the cluster to run your code\.
  * It is used for submitting jobs to compute nodes from an access point \(generally called a  _frontend_ \)\.
  * Frontends are intended for editing\, compiling\, and very short test runs\.
  * Production jobs go to the compute nodes through the resources manager\.
* Slurm documentation:
* [https://www\.rc\.virginia\.edu/userinfo/rivanna/slurm/](https://www.rc.virginia.edu/userinfo/rivanna/slurm/)	[http://slurm\.schedmd\.com/documentation\.html](http://slurm.schedmd.com/documentation.html)

# Slurm Script Example for R

* A Slurm script is a bash script with Slurm directives \(\#SBATCH\) and command\-line instructions for running your program\.

\#\!/bin/bash

\#SBATCH \-\-nodes=1              <span style="color:#002060">\#total number of nodes for the job</span>

\#SBATCH \-\-ntasks=1             <span style="color:#002060">\#how many copies of code to run </span>

\#SBATCH \-\-time=1\-12:00:00      <span style="color:#002060">\#amount of time for the whole job</span>

\#SBATCH \-\-partition=standard   <span style="color:#002060">\#the queue/partition to run on</span>

\#SBATCH \-\-account=myGroupName  <span style="color:#002060">\#the account/allocation to use</span>

<span style="color:#C00000">module purge</span>

<span style="color:#C00000">module load </span>  <span style="color:#C00000">goolf</span>  <span style="color:#C00000">/7\.1\.0\_3\.1\.4 R  </span>  <span style="color:#002060">\#load modules that my job needs</span>

<span style="color:#C00000">Rscript</span>  <span style="color:#C00000"> </span>  <span style="color:#C00000">myProg\.R</span>  <span style="color:#C00000">              </span>  <span style="color:#002060">\#command\-line execution of my job</span>

# Slurm Script Example for Python

* A Slurm script is a bash script with Slurm directives \(\#SBATCH\) and command\-line instructions for running your program\.

\#\!/bin/bash

\#SBATCH \-\-nodes=1             	 <span style="color:#002060">\#total number of nodes for the job</span>

\#SBATCH \-\-ntasks=1            	 <span style="color:#002060">\#how many copies of code to run </span>

\#SBATCH \-\-time=1\-12:00:00     	 <span style="color:#002060">\#amount of time for the whole job</span>

\#SBATCH \-\-partition=standard  	 <span style="color:#002060">\#the queue/partition to run on</span>

\#SBATCH \-\-account=myGroupName 	 <span style="color:#002060">\#the account/allocation to use</span>

<span style="color:#C00000">module purge</span>

<span style="color:#C00000">module load anaconda/2020\.11\-py3\.8 	</span>  <span style="color:#002060">\#load modules my job needs</span>

<span style="color:#C00000">python </span>  <span style="color:#C00000">myProg\.py</span>  <span style="color:#C00000">              	</span>  <span style="color:#002060">\#command\-line execution of my job</span>

# Submitting a Slurm Job

To submit the Slurm command file to the queue\, use the  <span style="color:#0070C0"> __sbatch__ </span>  command at the command line prompt\.

For example\, if the script on the previous slide is in a file named job\_script\.slurm\, we can submit it as follows:

\-bash\-4\.1$ sbatch job\_script\.slurm

Submitted batch job 18316

# Checking Job Status

To display the status of only your  _active_  jobs\,  type:

<span style="color:#0033CC">   </span>  <span style="color:#0070C0"> __squeue__ </span>  <span style="color:#0070C0"> __ –u <__ </span>  <span style="color:#0070C0"> __your\_user\_id__ </span>  <span style="color:#0070C0"> __>__ </span>

\-bash\-4\.2$ squeue –u mst3k

JOBID   PARTITION   NAME     USER   ST   TIME   NODES  NODELIST\(REASON\)

standard    job\_sci  mst3k  R    1:45    1     udc\-aw38\-34\-l

The squeue command will show pending jobs and running jobs\, but not failed\, canceled or completed job\.

To display the status of all jobs\,  type:

<span style="color:#0033CC">   </span>  <span style="color:#0070C0"> __sacct__ </span>  <span style="color:#0070C0"> __ –S <__ </span>  <span style="color:#0070C0"> __start\_date__ </span>  <span style="color:#0070C0"> __>__ </span>

\-bash\-4\.2$ sacct –S 2019\-01\-29

3104009      RAxML\_NoC\+   standard  hpc\_build         20  COMPLETED      0:0

3104009\.bat\+      batch             hpc\_build         20  COMPLETED      0:0

3104009\.0    raxmlHPC\-\+             hpc\_build         20  COMPLETED      0:0

3108537      sys/dashb\+        gpu  hpc\_build          1 CANCELLED\+      0:0

3108537\.bat\+      batch             hpc\_build          1  CANCELLED     0:15

3108562      sys/dashb\+        gpu  hpc\_build          1    TIMEOUT      0:0

3108562\.bat\+      batch             hpc\_build          1  CANCELLED     0:15

3109392      sys/dashb\+        gpu  hpc\_build          1    TIMEOUT      0:0

3109392\.bat\+      batch             hpc\_build          1  CANCELLED     0:15

3112064            srun        gpu  hpc\_build          1     FAILED      1:0

3112064\.0          bash             hpc\_build          1     FAILED      1:0

The sacct command lists all jobs \(pending\, running\, completed\, canceled\, failed\, etc\.\) since the specified date\.

# Canceling a Job

To delete a job from the queue\, use the _ _  <span style="color:#0070C0"> __scancel__ </span>  command with the job ID number at the command line prompt:

To cancel all your jobs\, run this command:

\-bash\-4\.2$ scancel 18316

\-bash\-4\.2$ scancel –u $USER

# JupyterLab

# 

# Open OnDemand Dashboard

Open OnDemand is the software that we use to create our web portal to Rivanna\.

As soon as you see the Dashboard \(shown below\)\, you are connected to Rivanna\.

![](img/IntroductionToRivanna_2022_10_1715.png)

The Dashboard gives you links to various applications\.

To see the links\, click on “Interactive Apps” on the menu bar\.

![](img/IntroductionToRivanna_2022_10_1716.png)

# Links to Applications

Today\, we are going to look at JupyterLab and RStudio  Server\.

# JupyterLab

Click on JupyterLab\.

# JupyterLab Web Form

![](img/IntroductionToRivanna_2022_10_1717.png)

The Jupyter Web Form gathers information about the computing resources that you need for your Jupyter Notebook\.

After you fill in the form\, it will re\-populate with the same settings the next time that you connect to it\.

Let’s look at how you would fill it in\!

# Rivanna Partition

![](img/IntroductionToRivanna_2022_10_1718.png)

![](img/IntroductionToRivanna_2022_10_1719.png)

Recall that Rivanna has lots of Compute Nodes\.

The nodes are partitioned \(i\.e\.\, organized\) by the type of processing that they can do\.

Most of the time\, you will select the  <span style="color:#0066FF"> __Standard__ </span>  partition\.

If you are running a deep learning model\, you will want to choose a  <span style="color:#0066FF"> __GPU__ </span>  Partition\.

# Number of Hours

The “Number of hours” is the amount of time that your session will be active\.

Beware\!  When time runs out the session will end without warning\!

![](img/IntroductionToRivanna_2022_10_1720.png)

![](img/IntroductionToRivanna_2022_10_1721.png)

# Allocation

![](img/IntroductionToRivanna_2022_10_1722.png)

The allocation is a special MyGroups group that allows you to have access to Rivanna\.

You must be a member of a Rivanna\-enabled MyGroup to have an active account\.

In general\, your professor or research advisor will add you as a member to an allocation\.

You can have membership in more than one allocation\.

![](img/IntroductionToRivanna_2022_10_1723.png)

# Launch

![](img/IntroductionToRivanna_2022_10_1724.png)

Clicking on the “Launch” button will submit a request for the resources that you want\.

There will be a slight delay before the resources are available\.

![](img/IntroductionToRivanna_2022_10_1725.png)

# Waiting for the Session to Start

The screen will transition from a “Please be patient” statement to a “Connect to Jupyter” button\.

Click on the “Connect to Jupyter” button\.

![](img/IntroductionToRivanna_2022_10_1726.png)

# Hands-on Activity

* Connect to Open OnDemand and start a JupyterLab session\.
  * When it comes up\, you will see a list of files \(if any\) in your home directory and a set of tiles for underlying applications \(e\.g\.\, Python\, R\, Tensorflow\)\.
  * You may see a slightly different set of tiles in your account – there are some customized tiles in this account\.

![](img/IntroductionToRivanna_2022_10_1727.png)

# Deleting Your Session

When you are done with your Jupyter Notebook\, it is very important to delete the session\.

Go back to the browser tab labeled “Interactive Sessions” and click on the red “Delete” button\.

![](img/IntroductionToRivanna_2022_10_1728.png)

# Rstudio Server

# 

# RStudio Server

Return to the Dashboard tab and click on RStudio Server\.

# RStudio Web Form

![](img/IntroductionToRivanna_2022_10_1729.png)

The RStudio Web Form is similar to what you saw with the Jupyter Web Form\.

Let’s take a quick look at some of the fields\.

# R Version

![](img/IntroductionToRivanna_2022_10_1730.png)

We have three versions of R available\.

R/3\.6\.2 is the most stable at this time\.

![](img/IntroductionToRivanna_2022_10_1731.png)

# Rivanna Partition

![](img/IntroductionToRivanna_2022_10_1732.png)

![](img/IntroductionToRivanna_2022_10_1733.png)

The partitions are the same as in the JupyterLab Web Form\, but we recommend mostly  <span style="color:#0066FF">Standard</span> \,  <span style="color:#0066FF">Dev</span> \, or  <span style="color:#0066FF">Instructional </span> for RStudio Server\.

# Number of hours & Allocation

![](img/IntroductionToRivanna_2022_10_1734.png)

The Number of hours and the Allocation are the same as in the JupyterLab Web Form\.

![](img/IntroductionToRivanna_2022_10_1735.png)

![](img/IntroductionToRivanna_2022_10_1736.png)

# Launch

![](img/IntroductionToRivanna_2022_10_1737.png)

Clicking on the “Launch” button will submit a request for the resources that you want\.

Again\, there will be a slight delay before the resources are available\.

![](img/IntroductionToRivanna_2022_10_1738.png)

# Waiting for the Session to Start

![](img/IntroductionToRivanna_2022_10_1739.png)

The screen will transition from a “Please be patient” statement to a “Connect to RStudio Server” button\.

Click on the “Connect to RStudio Server” button\.

![](img/IntroductionToRivanna_2022_10_1740.png)

# Hands-on Activity

* Connect to Open OnDemand and start an RStudio Server session\.
  * When it comes up\, you will see an RStudio interface – just like you would see on your laptop\.

![](img/IntroductionToRivanna_2022_10_1741.png)

# Deleting Your Session

Again\, when you are done with RStudio Server\, it is very important to delete the session\.

Go back to the browser tab labeled “Interactive Sessions” and click on the red “Delete” button\.

![](img/IntroductionToRivanna_2022_10_1742.png)

# What about the other fields?

| Field | Description |
| :-: | :-: |
| Number of cores | Used in parallel processing.  Your code must be modified to take advantage of using multiple cores. |
| Memory Request in GB | When dealing with Big Data, you will need to increase the amount of memory.  My rule-of-thumb:  request 2 to 3 times the size of data that you are reading in or generating. |
| Work Directory | Allows you to change the working directory of a Jupyter Notebook to your /scratch folder. |
| Optional: Slurm Option | Allows you to provide advanced features, like requesting specific nodes or providing a reservation |
| Optional Group | Only needed in you are in more than 16 allocations.  You may need to force Rivanna to see your allocation. |
| Optional: GPU type for GPU partition &  Optional: Number of GPUs | Only needed in you are running on a GPU node.  The “default” for GPU type will put you on the first available GPU node.For now, the number of GPUS should be 1. |

You may have noticed fields on the Web Forms that we left blank or stayed with the default values\.

The most important one will be the Memory Request

# Need help?

__Research Computing Zoom Office Hours__

Tuesdays:	3 pm – 5 pm

Thursdays:	10 am – noon

To connect to the Zoom sessions\, go to [https://www\.rc\.virginia\.edu/support/\#office\-hours](https://www.rc.virginia.edu/support/#office-hours) and click on the “Join us via Zoom” button

# Or, contact us through the forms at:
	https://www.rc.virginia.edu/support/

![](img/IntroductionToRivanna_2022_10_1743.png)


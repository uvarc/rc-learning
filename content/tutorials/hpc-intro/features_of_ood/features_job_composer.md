---
date: "2022-10-01"
title: "Job Composer"
weight: 330
---

The job composer tab allows you to create and submit a job to run on the cluster.

{{< figure src="/tutorials/hpc-intro/img/Features_job_composer.png" caption="OOD Job Composer" >}}

Selecting the default template will automatically create a submission script called ```demo_hello_world.slurm``` located in ```/home/computingID/Rivanna/data/sys/myjobs/projects/default/1``` on the file system:

{{< figure src="/tutorials/hpc-intro/img/featues_template_job.png" caption="Default Template Job" >}}

Before submitting the job, ```your_allocation``` on the ```#SBATCH --account=your_allocation``` line must be replaced with the name of the allocation you're a member of. We will review editing files later. Once the correct allocation name is edited in, you can click "Submit" to queue your job. It will be given a corresponding Job ID, and once it's completed, the Folder contents will now contain a corresponding output file that contains the instructions from the submission script:

{{< figure src="/tutorials/hpc-intro/img/features_job_output.png" caption="Default Template Output" >}}

There are several job templates that can be run in addition to the default hello world option under New Job > From Template:

{{< figure src="/tutorials/hpc-intro/img/features_templates.png" caption="Template Options" >}}

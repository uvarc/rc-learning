---
title: The Open OnDemand Job Composer
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 710

menu:
  rivanna-introduction:
    parent: The Slurm Resource Manager
---

You can use the OOD Job Composer to construct a job script and submit the job. Start by selecting a template script, which usually must be edited for customization to your job requirements.  We can run the demo script with only minor changes so let us start there.  From Job Composer choose New Job, From Default Template.

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_default.png" caption="Default template" >}}

At the bottom left, click the "Open Editor" button.  Change `your_allocation` to your allocation group name.  Click the Save button at the upper left.  Back in the Job Composer, refresh the page and make sure your change has taken effect.  Click the green "Submit" button.  The job status will be shown.  Once it has changed to Completed, note that the Folder Contents section contains a new file.  This will be named something like `slurm-123456.out` where the actual number corresponds to the job ID of your job.  This will contain the output of your job.

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_completed.png" caption="Job completion." >}}

Click on the slurm file and it should open in the editor.  Alternatively, click on "Open Dir" to open the File Explorer in the directory from which OOD ran your job, where you may View or Edit any of the files.

### Canceling a Job

This job will run so quickly you will probably not be able to stop it before it completes, but for other jobs that run longer, if you need to cancel the job, click the yellow "Stop" button.  Do not use Delete for this purpose.

### Deleting a Job

"Deleting" a job removes the files from the directory specified in the "Script Location" entry.  This helps you clean up your home directory.  You may also do this manually through the File Explorer or on FastX.  You may choose to create a directory with a more descriptive name than the OOD default and move your files there.

---
title: Using a Template
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 740

menu:
  rivanna-introduction:
    parent: The Slurm Resource Manager
---

The default demo Slurm script template is very simple and it would be easier to start from something closer to your intentions.  We provide quite a few sample script templates.

{{< info >}}
The example shown here is from a particular time and the templates may be updated.  What you see may not be the same as in these illustrations, but the steps to follow should be the same.
{{< /info >}}

Suppose we wish to run a serial (one core) Python program.  From New Job we select From Template.  We can search on the string `python` to narrow the options. We find `demo-python-serial`.  Select that option. Click "Create New Job" to make the folder and copy the demo files into it.

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_new_template_job.png" caption="Select the template you wish to use." >}}

As before, edit the Slurm script to fill in your correct allocation group name. Also make sure the module loaded is just `anaconda`.
```
module purge
module load anaconda
```

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_python_job.png" >}}

{{< figure src="/notes/rivanna-introduction/imgs/OOD_JC_edit_template_job.png" caption="Select the template you wish to use." >}}

Submit the job.  Once it returns, look at the output.  Oops!

{{< figure src="/notes/rivanna-introduction/img/OOD_JC_slurmout.png" caption="Select the template you wish to use." >}}

An error occurred.  In a real job, your output should have been what you expected, and you can proceded as for the default demo example to manage your result files.


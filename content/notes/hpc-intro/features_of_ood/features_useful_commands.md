---
title: Useful Commands
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 380

menu:
  rivanna-intro:
    parent: Features of Open OnDemand
---

Sometimes it's useful to check how many SUs are still available on your allocation. The ```allocations``` command displays information on your allocations and how many SUs are associated with them:

{{< figure src="/notes/rivanna-intro/img/features_allocations.png" caption="Allocations" >}}

running ```allocations -a <allocation_name>``` provides even more detail on when the allocation was last renewed and its members.



One way to check your storage utilization is with the ```hdquota``` command. This command will show you how much of your home, scratch, and project (if applicable) storage are being utilized. Below is the sample output for ```hdquota```:

{{< figure src="/notes/rivanna-intro/img/features_hdquota.png" caption="Disk Usage" >}}

This is a useful command to check whether or not you're running out of storage space or to see where files need to be cleaned up. For more detailed information on disk utilization you may also use the ```du``` command to investiage specific directories.


To gain information on the different queues you can type ```qlist``` on the command line:

{{< figure src="/notes/rivanna-intro/img/features_qlist.png" caption="Queues" >}}

This will show the list of paritions, their occupancy, and the SU charge rate. You can type ```qlimits``` for information on each queue's limits:

{{< figure src="/notes/rivanna-intro/img/features_qlimits.png" caption="Queue Limits" >}}

Finally, the ```sinfo``` command will provide some more detailed information on the health of each queue and the number of active nodes available. These commands can be useful in diagnosing why a job may not be running, or in better understanding queue usage for more efficient job throughput. More information on hardware specifications and queue information can be found on our [website](https://www.rc.virginia.edu/userinfo/rivanna/overview/#system-details).


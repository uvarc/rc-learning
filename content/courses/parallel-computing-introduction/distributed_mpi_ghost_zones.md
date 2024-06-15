---
title: "Halos and Ghost Zones"
toc: true
type: docs
weight: 130
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

This type of exchange is particularly common in _boundary-value problems_.  We do some kind of computation on the grid, but then the boundary conditions must be applied.  In an undivided domain, the boundaries are set by the model we are solving. When we divide the domains, suddenly we have converted one boundary into many boundaries; each subdomain is disconnected from the others and do not know what has been computed on them. Some information must be exchanged regularly at the boundaries between the subdomains. 

A very widely used method to accomplish this is to extend the subdomain with "ghost" or "halo" zones. These are "fake" zones that function as the new boundaries for the inner grids.

{{< figure src="/courses/parallel-computing-introduction/img/ghost_zones.png" caption="Ghost or halo zones are added to communicate with the neighboring domain." >}}
`

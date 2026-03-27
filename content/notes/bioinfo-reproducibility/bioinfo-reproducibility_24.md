---
title: Running jobs on interactive node
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1250
menu: 
    bioinfo-reproducibility:
---

Run interactively - good for testing

$ ijob -c 1 -A hpc_training -p interactive –v -t 2:00:00

$ cp  <span style="color:#1a1a1a">/project/</span>  <span style="color:#1a1a1a">hpc_training</span>  <span style="color:#1a1a1a">/reproducibility/</span>  <span style="color:#1a1a1a">snakemake</span>  <span style="color:#1a1a1a"> .</span>


Default execution here is local so everything is running in my ijob session on a compute node. If we wanted to have these processes run non-interactively we would want to make sure we are using the executor flag in our snakemake call: "--executor slurm"

Work in scratch


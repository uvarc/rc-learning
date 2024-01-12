---
title: The Modules Environment
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1900
menu: 
    rivanna-command-line:
---

_Environment modules_ are not strictly a part of Unix, but are widely used by many HPC sites, including ours.  Modules enable the user to set complex paths, environment variables, and so forth, simply by loading a module.

The commands are set up automatically when you log in.  Loaded modules only affect the shell in which the command is run.

Modules required for a job must be loaded in the batch _job script_.


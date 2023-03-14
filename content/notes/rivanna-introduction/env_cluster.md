---
title: The Cluster Environment
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 400

menu:
  rivanna-introduction:
    name: The Cluster Environment
---

When you log in to Rivanna you are on one of the _login nodes_ or _frontends_. 
Use of the frontend is restricted to short "housekeeping" tasks such as
   - Writing your programs or scripts
   - Compiling programs
   - Submitting jobs

- You may run _very short_ test runs with a limited number of cores and amount of memory. Your process will be terminated if it exceeds the time or memory limit.

- You may _not_ run multiple processes at once, nor may you run production jobs.

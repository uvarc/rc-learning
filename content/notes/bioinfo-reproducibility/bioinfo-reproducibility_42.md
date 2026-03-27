---
title: Create a New File called main.nf
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2150
menu: 
    bioinfo-reproducibility:
---

We can create a new file called main.nf with these lines.

process HELLO {

script:

"""

echo "Hello World!"

"""

}

workflow {

HELLO()

}

Show and execute main.nf in terminal. Show where the file goes. Went to .command.out file in 'work' directory
 for the specific process


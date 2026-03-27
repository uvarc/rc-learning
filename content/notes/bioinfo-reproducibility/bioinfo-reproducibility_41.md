---
title: A Toy Example
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2100
menu: 
    bioinfo-reproducibility:
---

**Example**

Let's start with a very simple toy example for echo'ing the text "Hello World!" And then we'll build to our bioinformatics example.

First, create a process called HELLO with our shell command:

process HELLO {

script:

"""

echo "Hello World!"

"""

}

Then we execute this process in our workflow:

workflow {

HELLO()

}



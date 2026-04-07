---
title: Nextflow Example
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2100
menu: 
    bioinfo-reproducibility:
---

## Example

Let's start with a very simple toy example for echo'ing the text "Hello World!" And then we'll build to our bioinformatics example.

First, create a process called HELLO with our shell command:

```bash
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
```


## Create a New File called main.nf
We can create a new file called main.nf with these lines.

```bash
process HELLO {

script:

"""

echo "Hello World!"

"""

}

workflow {

HELLO()

}
```

## Make some changes
```bash
**process**  hello {
 **output:**
 path 'hello.txt'

 script:
 """
 echo 'Hello world!' > hello.txt
 """
 }
```
 
We want to send the text to a file called 'hello.txt.' Now we can update our shell command to send the text to a file, and we can add an output in our process to define our file name and since out output is a file, we'll specify the type of output as a path.

Run `main.nf` in terminal and show it still went to 'work' directory

This was better, but we still have to dig around for the file, so let's add one more thing to our process.
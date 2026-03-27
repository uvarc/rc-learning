---
title: Make Some Changes
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2200
menu: 
    bioinfo-reproducibility:
---

__process__  hello {  __output__ : path 'hello.txt' script: """ echo 'Hello world!' > hello.txt """}

We want to send the text to a file called 'hello.txt.' Now we can update our shell command to send the text to a file, and we can add an output in our process to define our file name and since out output is a file, we'll specify the type of output as a path.

Run `main.nf` in terminal and show it still went to 'work' directory

This was better, but we still have to dig around for the file, so let's add one more thing to our process.


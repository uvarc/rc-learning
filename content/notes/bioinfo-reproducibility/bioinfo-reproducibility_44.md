---
title: Add a publishDir 
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2250
menu: 
    bioinfo-reproducibility:
---

Now let's try sending our output to a directory called 'results' - we can add a publishDir to our process and specify the mode "copy" is safest, but you can do other things like move or even create links to the file.
Re-run the main.nf in the terminal and show where the file goes to results but since we did copy, it still does go to work. Point out that we need to be mindful of any extra data we're creating so we don't unnecessarily have duplicates for everything.

__process__  hello { publishDir "results/" , mode: "copy"

__ output__ : path 'hello.txt' script: """ echo 'Hello world!' > hello.txt """}


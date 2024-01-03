---
title: Strings, Single (') and Double (") quotes
date: 2023-11-29-21:03:38Z
type: docs 
weight: 800
menu: 
    bash-scripting:
---


Bash is not very good at math so much of what we do with Bash involves  <span style="color:#FF0000">strings</span> , e.g. a sequence of text and special characters.

__Pair of Single Quotes ''__

preserves the literal value of each character within the quotes.

__Pair of Double Quotes ""__

preserves the literal value of all characters within the quotes with the exception of  <span style="color:#FF0000">$ </span> ,  <span style="color:#FF0000">` </span> , and  <span style="color:#FF0000"></span>  , which retain their special meaning.

<span style="color:#FF0000">         $</span> 		Shell Expansion, e.g. variable name follows

<span style="color:#FF0000">          </span> 		Escape character

<span style="color:#FF0000">`COMMAND`</span> 	Executes COMMAND and returns its output

stream; equiv. to $(COMMAND) in newer versions

{{< figure src=/notes/bash-scripting/img/2018-10-18_bash_wkshp%20%28kah3f%40VirginiaEDU%298.png >}}


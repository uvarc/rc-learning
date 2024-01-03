---
title: Command-Line Arguments
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1850
menu: 
    bash-scripting:
---


Many bash scripts need to read arguments from the command line.  The arguments are indicated by special variables $0, $1, $2, etc.

Command lines arguments are separated by whitespaces:

./Commandline.sh Hello You

$0 is the name of the command/script itself

The subsequent ones are the command line arguments

If you have a variable number of arguments, use the shift built-in to iterate over them in your script.

The special variable $# is the number of arguments (not counting the command name)

$@ expands to array of all command line arguments


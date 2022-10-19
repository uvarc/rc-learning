---
title: "Bash Scripting"
type: article
toc: true
# date: 2015-07-23T21:13:14-05:00
draft: false
---

<p class="lead">Scripts are powerful tool for automating regular, recurring tasks. Use them to make repetitive tasks easier, 
to schedule tasks to run at regular intervals, or to look for certain conditions when they run. Scripts can also written to 
be interactive or non-interactive. The most common scripting tool in Linux systems is the Bourne Again Shell (BASH), but they
can also be written in any other language that can be executed within the shell: C, Python, R, etc.
</p>

Attendees of this workshop should be familiar with basic CLI and environment resources and commands.

## Shell Scripting

Scripts are simple ways of bundling up a series of commands to run in order. You can run your script manually, or scheduled to run automatically.
The examples below are based on `bash`.

The basics of a shell script:

1. Start with a shebang to bash `#!/bin/bash`.
2. Add shell commands in the order you need and expect them to run.
3. Assume the script is running from its current location, so all paths should be explicit. 

Here is a simple example of a non-interactive script that clears the screen, pauses, then displays a quote from an API:

    #!/bin/bash

    clear
    sleep 3

    echo -e "Here is a quote of the day:\n\n"
    qod=`curl -s http://quotes.rest/qod.json | jq -r .contents.quotes[0].quote`
    echo "  " $qod
    echo ""

Copy and paste this code into a plain text file named `script.sh` or `myscript`. Remember that in the Linux world, the suffix
at the end of a file name does not really matter for bash scripting. You may name it with `.sh` at the end to remind yourself
what the file is and what it does.

To run the script, type

    bash myscript.sh

This invokes `bash` to execute your script. Alternatively, you can modify your script to make it executable and run it:

    chmod 755 myscript.sh
    ./myscript.sh



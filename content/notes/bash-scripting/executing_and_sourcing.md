---
title: Executing and Sourcing Scripts
date: 2023-11-29-21:03:38Z
type: docs 
weight: 400
menu: 
    bash-scripting:
---

## Executing a Script

Suppose the previous script was in a file `hello.sh`. By default it is just text.  You must explicitly make it executable:
```bash
chmod u+x hello.sh
```
Then you can just invoke it by name at the command line:
```bash
./hello.sh
```
As an alternative you can run your script by explicitly invoking the bash interpreter
```bash
bash hello.sh
```

## Sourcing a Script

If you wish execute the script as a standalone script, it needs a _shebang_, or you must run `bash myscript.sh as shown above. In both cases, a new shell will be started. Anything in that script will not appear in your current shell.

Sometimes you just want to bundle up some commands that you repeat regularly and execute them within the  _current_  shell.  To do this you should  _not_ includea shebang, and you must  `source` the file.
```bash
. myfile.sh
```
or
```bash
source myfile.sh
```


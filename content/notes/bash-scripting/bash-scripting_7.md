---
title: Executing A Script
date: 2023-11-29-21:03:38Z
type: docs 
weight: 400
menu: 
    bash-scripting:
---


Suppose the previous script was in a file  hello.sh

By default it is just text.  You must explicitly make it executable:

chmod u+x hello.sh

Then you can just invoke it by name at the command line:

./hello.sh

_Alternative:_  Run via bash interpreter

bash hello.sh


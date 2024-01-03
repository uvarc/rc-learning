---
title: Sourcing
date: 2023-11-29-21:03:38Z
type: docs 
weight: 500
menu: 
    bash-scripting:
---


* If you execute the script as a standalone script, it starts a new shell -- that is what the  _shebang_  does -- if you do not use that you can type
  * bash myscript.sh
* Sometimes you just want to bundle up some commands that you repeat regularly and execute them within the  _current_  shell.  To do this you should  _not include _ a  _shebang_ , and you must  _source_  the script
* . <script> or  source <script>


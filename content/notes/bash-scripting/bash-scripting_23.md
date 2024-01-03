---
title: Example Testing File Properties
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1200
menu: 
    bash-scripting:
---


* if [[ -d $to_dir ]]; then
  * if [[ -f $the_file ]]; then
    * cp $the_file $to_dir
  * fi
* else
  * mkdir $to_dir
  * echo “Created $to_dir”
* fi


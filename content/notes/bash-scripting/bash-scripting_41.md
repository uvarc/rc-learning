---
title: String Manipulation Example
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2100
menu: 
    bash-scripting:
---


Create script strings.sh

_#!/bin/bash_

name="Miller"

echo ${name}

<span style="color:#0070C0"># string length</span>

echo "Your name has ${#name} letters."

<span style="color:#0070C0"># clipping string</span>

echo "I turned you into a ${name%"er"}."

<span style="color:#0070C0"># replacing substring</span>

single_repl=${name/"l"/"*"}

echo ${single_repl}

all_repl=${name//"l"/"*"}

echo ${all_repl}


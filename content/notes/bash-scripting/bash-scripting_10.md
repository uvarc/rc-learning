---
title: Variables Placeholders to store values
date: 2023-11-29-21:03:38Z
type: docs 
weight: 550
menu: 
    bash-scripting:
---


The shell permits you to define variables.

__Scalar Variable: __ Holds single value

color _=_ blue

echo  <span style="color:#FF0000">$</span> color

__Array Variable: __ Holds multiple values

colors _=_ (blue red green)

echo  <span style="color:#FF0000">$</span> {colors[@]}  _# prints all values_

echo  <span style="color:#FF0000">$</span> {colors[0]}   _# prints first value_

echo  <span style="color:#FF0000">$</span> {colors[1]}   _# prints second value_

A _ssigning_  a value to a variable.  __Do not put spaces around the __  _=_  __ sign.__

R _eferencing_  a variable. The  <span style="color:#FF0000">$</span>  tells the interpreter to look up the value of the variable symbol.


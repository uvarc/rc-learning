---
title: Arrays
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2150
menu: 
    bash-scripting:
---


* Arrays are zero based so the first index is 0.
* Initialize arrays with a list enclosed in parentheses.
* Obtaining the value of an item in an array requires use of ${}:
  * arr=(blue red green)
* echo ${arr[@]}  <span style="color:#0070C0"># All the items in the array</span>
* ${#arr[@]}  <span style="color:#0070C0"># Number of items in the array </span>
* ${arr[0]}   <span style="color:#0070C0"># Item zero</span>
* <span style="color:#0070C0">     </span> ${arr[1]}   <span style="color:#0070C0"># Item one</span>
* ${#arr[0]}  <span style="color:#0070C0"># Length of item zero</span>


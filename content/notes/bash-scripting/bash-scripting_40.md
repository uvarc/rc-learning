---
title: Replacing Portions of a String
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2050
menu: 
    bash-scripting:
---


* Often a specific portion (substring) of a string needs to be replaced.
  * Replace  _first_  match of substring in string with replacement string
  * ${string/substring/replacement}
  * Replace  _all_  matches of substring in string with replacement string
  * ${string//substring/replacement}


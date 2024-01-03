---
title: Regex Character Sets and Modifiers
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2800
menu: 
    bash-scripting:
---


The character set is the set of characters that must be matched literally.

Modifiers expand the potential matches.

* matches any number of repeats of the pattern before it (note that this is different from its use in the shell)  _including_  0 matches.

? Matches 0 or 1 character (also different from the shell wildcard).

+ matches one or more, but not 0, occurrences.

. matches any single character, except newline


---
title: Regex Ranges and Repetition
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2900
menu: 
    bash-scripting:
---


[] enclose a set of characters to be matched.

- indicates a range of characters (must be a subsequence of the ASCII sequence)

{n} where n is a digit, indicates exactly n repetitions of the preceding character or range.

{n,m} matches n to m occurrences.

{n,} matches n or more occurrences.


---
title: Strings
toc: false
type: docs
draft: false
weight: 50

menu:
    python_introduction:
        weight: 50
---

The `string` type is widely used in Python.  A __string__ consists of a sequence of characters, even if the sequence length is 1--Python does not make a distinction between a character and a one-character string.  The string is a compound type and immutable.  The representation of a single character internally in the computer as a sequence of bits is called the _encoding_.  Individual characters are represented either by the ASCII standard (1 byte per character) or Unicode (2-4 bytes per character).  Strings that are to be treated as Unicode are type `unicode` rather than string, but otherwise behave similarly.  The default encoding may depend on the operating system but in newer Python versions is usually a standard called _utf-8_.  UTF-8 can represent over one hundred thousand characters and can embed different scripts within the same text file.

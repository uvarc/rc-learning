---
title: IO and Exceptions
toc: false
type: docs
draft: false
weight: 80

menu:
    python-introduction:
---

Programs are not very useful if they cannot communicate their results.  They are also not terribly useful if they cannot change their controlling parameters to compute different results.  

We have already been using the `print` function to display results.  This is an example of _console input/output_.  The console is text-based and displays on the screen.  It is capable of both input and output.  So far we have been allowing the intepreter to arrange the appearance of the output, but we can control that with _formatting_. 

Much of the time our programs will be working with _files_ for both input and output.  Files are saved to _storage_ devices such as disk drives or SSDs.  Files must be located by the program, made available for reading and/or writing (opened), and eventually closed.

Exceptions occur due to error conditions anywhere in the code.  They have wide applicability but are frequently encountered when dealing with input/output and files, so we will look at them in this section.


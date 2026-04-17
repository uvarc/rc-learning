---
title: Pattern Rules in Make
date: 2026-04-14T16:42:59Z
type: docs 
weight: 550
menu: 
    building-running-c-cpp-fortran:
---


This is an extension by Gnu make (gmake), but nearly every make, especially on Linux, is gmake now.  They are similar to suffix rules. 

Gmake contains built-in pattern rules so it can handle common cases if you do not write your own rules.  For example, to compile a C code it will by default use

```no-highlight
%.o : %.c
        $(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@
```

A pattern rule that is particularly useful for Fortran 90+:

```no-highlight
%.mod: %.o 
```

This can be helpful because make's default pattern rule for `.mod` is for a programming language called Modula, but Fortran uses `.mod` for compiled module interface files.  Adding this pattern rule overrides the built-in rule for that suffix.

## Make Options

Files with names other than `makefile` or `Makefile` can be used with the -f option
```no-highlight
make -f Make.linux 
```

Make creates the first target in the file unless a specific target is specified.(Some Makefiles are written to require a target.). To generate a different target, provide its name on the command line.

```no-highlight
make pw.x 
```


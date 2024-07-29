---
date: "2021-04-05"
title: "Programming in Modern Fortran"
summary: "This short course is an introduction to programming in modern Fortran.  Experience programming in some other language is helpful but not required."
authors: [kah]
categories: ["Programming", "Fortran"]
tags: [programming, fortran]
weight: 1
---

> I do not know what the scientific programming language of the year 2000 will look like but it will be called Fortran.
<br>
> -- Apocryphal, sometimes attributed to John Backus (inventor of Fortran) or Seymour Cray (inventor of the supercomputer), circa 1985.

Please note that the official spelling is Fortran, not FORTRAN.

Most of the material will be oriented toward the Fortran 2003 standard, which is well-supported in recent compilers and includes a number of constructs for modern programming paradigms.

Fortran was introduced in October 1957.  It was developed by a team at IBM led by John Backus and was among the earliest of the high-level programming languages, which replaced the machine-oriented assembler programming model widely used at the time with more natural human-readable syntax.  Algol and COBOL followed in the next few years.

It is important to keep in mind that computers for which the first languages were written were extremely limited in memory, speed, and disk space.  Typical main memory in the 1960s was measured in kilobytes.  A supercomputer circa 1994 had 2GB of RAM as main memory.  Many of the obsolete constructs were developed to work around these limitations; for instance, `COMMON` allowed memory to be "overbooked." Given its decades of history, many old codes using such obsolete constructs still exist and are in use.

Fortran is still very widely used in many scientific and engineering disciplines. Mechanical engineering, chemistry and chemical engineering, and environmental sciences, particularly atmospheric sciences and oceanography, make significant use of Fortran.  Features of modern Fortran make it very well suited to numerically-intensive programming.  Due to both legacy codes and ongoing development, many scientific programmers must be able to use both C/C++ and Fortran.

{{< figure src="/courses/fortran-introduction/img/Cray-1.jpg" width=500px caption="The first Cray-1 supercomputer had 1 80-MHz core and a maximum of about 8MB of main memory." >}}

The best way to learn a programming language is to use it.  We strongly urge you to attempt the exercises and projects to practice your coding skills.

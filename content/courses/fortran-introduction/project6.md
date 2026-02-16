---
title: "Project 6"
date : "2021-04-05T00:00:00-05:00"
toc: true
type: book
weight: 76
---

Modify your `bird_data` type from [Project 5](/courses/fortran-introduction/project5) to make it a class.  Make all the methods private.  

You may use the same file_utils.f90 and sorters.f90 as before.  

You may find that you need to declare your allocatable bird_list array to be of type `class`.

{{< spoiler text="Sample solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/bird_class.f90" lang="fortran" >}}
{{< code-download file="/courses/fortran-introduction/solns/bird_obs_class.f90" lang="fortran" >}}
{{< /spoiler >}}

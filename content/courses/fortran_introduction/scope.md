---
title: "Variable Scope"
toc: true
type: book
weight: 63

menu:
    fortran_introduction:
        parent: Subprograms
        weight: 63

---


# Variable Scope

In Fortran, scope is defined by the program unit.

A calling unit may have a variable namedx, and a function may also have a variable namedx, and ifxis not an argument to the function then it will be distinct from thexin the calling unit.

x=20.

call sub(x)

etc.

subroutine sub(y)

real, intent(inout) :: y

real                :: x

x=10.

y=30.

end subroutine sub

# CONTAINS and Nested Procedures

Thecontainskeyword extends the scope into the contained program unit.

The end of the “container” must _follow_ the end of the “containee”

A contained subprogram can access _all_ the variables in the container except those that are explicitly passed.

The interface is implicit and should not be made explicit.

Contained procedures are often said to be _nested_ .

Only one level of nesting is permitted.

# Example

programmyprog

implicit none

real  ::x,y,z

x=5.; y=10.

callmysub(z)

contains

subroutinemysub(w)

real, intent(inout) :: w

w=x+y

end subroutinemysub

end programmyprog

# Common

COMMONis a deprecated feature that is frequently seen in older code.  It is a means of providing global variables.  Syntax:

common /comname/ var1, var2, var3

The variables in the common list will be available to _any_ program unit that includes the above line.  Variables in common between two program units should not be passed as subroutine parameters.

Newer code should use modules rather thancommon.

# Pitfalls with Common

Thecommonstatement _must_ be in every program unit that will share the variables, and it _must_ be identical in each one.  It is highly recommended that any code using common put each one into a separate file and use the include statement to merge the files.INCLUDEis a standard Fortran statement, not a preprocessor statement; its syntax is

include 'file.h'

wherefile.hcan be any name (Fortran does not have a rule about file extensions for included files).

COMMONis a frequent source of memory errors.

COMMONmakes interface control difficult to impossible.


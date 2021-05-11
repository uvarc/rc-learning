---
title: "Characters and String"
toc: true
type: docs
weight: 43

menu:
    fortran_introduction:
        name: Characters and Strings
        weight: 43

---

Fortran's support of characters and strings has evolved significantly through the recent standards. The earliest Fortran used _Hollerith codes_ to represent characters.  The `character` type was introduced in the Fortran 77 standard. The original character type is essentially fixed-length string.  Variable-length strings were introduced in the Fortran 2003 standard.  

# Character Comparison Intrinsics

lge(stringA,stringB)

Returns.true.IfstringAis lexically greater than or equal tostringB, otherwise returns.false.

lgt(stringA,stringB)

Returns.true.IfstringAis lexically greater thanstringB, otherwise returns.false.

lle(stringA,stringB)

Returns.true.IfstringAis lexically less than or equal tostringB, otherwise returns.false.

llt(stringA,stringB)

Returns.true.IfstringAis lexically less than or equal tostringB, otherwise returns.false.
                                                
**Exercise 2**
Declare character variables large enough to hold the indicated strings.  Makenewtitleat least 5 characters longer than you think necessary.

title=“This is a string”

subtitle=“Another string”

print *,len(title)

print *, title//”:”//subtitle

newtitle=title//” : “//subtitle

print *,len(newtitle)

print *,len_trim(newtitle)

print *,newtitle(2:4)

!Change “This” to “That” innewtitle

Exercises with conditionals.

Be sure to declare variables appropriately.

a=11.; b=9.; c=45.; n=3

print *, a>b

print *, a<b and c==n

print *, a<b or c==n

print *, a>b or c==n and a<b

print *, (a>b or c==n) and a<b

is_equal= a==b

print *,is_equal



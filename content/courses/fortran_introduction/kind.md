---
title: "Variable Kind"
toc: true
type: docs
weight: 23

menu:
    fortran_introduction:
        name: Variable Kind
        weight: 23

---

Variables in modern Fortran may have a _kind_ associated with them. Kind defines the number of digits and, for floating-point numbers, the exponent range.

Kind is represented by an integer, the `kind value`.

## Specifying Kind Values

### Floating Point

For floating-point numbers, the programmer requests at least a certain number of decimal digits of precision and at least a certain exponent range, and the system matches the request as best it can.
In practice, most systems have single and double precision.  A few offer quad precision (REAL\*16 in older declarations) but it is usually done in software and is _very_ slow.

Compilers still support REAL and DOUBLE PRECISION (REAL\*8 was never standard, but is fairly universally supported).  The advantage to KIND is that it becomes easy to change precision, especially when using a module.

For floating-point numbers, do not assume that the KIND type integer equals the number of bytes, though this typically is the case.  Use intrinsics to obtain or use KIND, or the predefined parameters in the ISO_FORTRAN_ENV module.  Refer to your compiler manual for specifics.

```
rk=SELECTED_REAL_KIND(prec,r)
```
This returns the floating-point kind that has a decimal precison of at least `prec` digits and an exponent range of at least 10<sup>&plusmin;r</sup>.  If this cannot be accommodated it returns a negative number.  For more specific information see the compiler documentation, e.g. for [gfortran](https://gcc.gnu.org/onlinedocs/gfortran/SELECTED_005fREAL_005fKIND.html#SELECTED_005fREAL_005fKIND).

`kind(x)`returns the kind type parameter of `x`.
```
fk=SELECTED_REAL_KIND(12,200)
```

### Integers

For an integer `ik`, 
```
ik=SELECTED_INT_KIND(r)
```
This returns the integer kind that accommodates at least the range -10<sup>r</sup> to 10<sup>r<sup>.  If the range cannot be accommodated on the system, the value of `ik` will be an indicator such as -1.

### Logicals

### Characters

For a specified character set CHARSET
```
ck=SELECTED_CHAR_KIND(CHARSET) 
```
returns the kind value for the character set CHARSET. If the requested character set is not supported, an error indicator (such as -1) is returned. Currently,
most compilers support “ASCII”, “DEFAULT”, and “ISO_10646” (Universal Character Set, UCS-4).

## Determining Kind Values

For a particular variable or literal, the `KIND` function returns the integer kind value.  It is defined for floating point numbers, complex numbers, integers, characters, and logicals.

```
rk=kind(1.0)
ik=kind(1)
ck=kind('ASCII')
lk=kind(.true.)
```

## Declaring KIND Variables

If we are staying within the IEEE 754 standard, the two floating-point kinds supported in hardware can be selected with:
```
INTEGER, PARAMETER ::rk=kind(1.0)
```
or
```
INTEGER, PARAMETER ::rk=kind(1.d0)
```

The kind variable must be a PARAMETER.

Variables are then declared as
```
REAL(rk)  :: r, s, t
```
Literals can be written as
```
1.0_rk
```
Switching between single and double precision is then as easy as replacing the SELECTED_REAL_KIND or KIND statement.  It is best to use a module for this purpose.

## Type Conversions with KIND

The `kind` argument is optional except for `real` converting one floating-point type to another floating-point type.  If absent, each function returns a value of the default kind and appropriate type.  If present, the value returned is also converted to the requested `kind`.
```
aint(a,kind)  !truncates a float
anint(a,kind) !nearest integer
ceiling(a,kind)
cmplx(x,y,kind) !creates a complex number with real and imaginary x,y
floor(a,kind)
int(a,kind)  !truncates, casts to integer
nint(a,kind) !nearest integer, casts to integer
real(a,kind) !converts integer to float or between float types
```

Example for explicit casting with `kind`. Given that `dp` has been declared to match double:
```
x=real(w,dp)
```
converts `w` from single to double precision.


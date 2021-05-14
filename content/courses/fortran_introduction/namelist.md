---
title: "Namelist"
toc: true
type: book
weight: 55

menu:
    fortran_introduction:
        parent: Input/Output
        weight: 55

---

# Fortran Namelist

One of the most convenient I/O statements in Fortran isNAMELIST.  With this statement, parameters in an input file can be specified byname=valueand in any order.

Thenamelistmust be declared.  This is a non-executable statement.  Syntax:

NAMELIST /name/ var1,var2,var3

The name is chosen by the programmer.

Thenamelistis read with a special form of the READ statement

read(iunit, name)

# Namelist Input

The input file containing thenamelistmust follow a specific format.Namelistwas not part of the Fortran 77 standard (it was standardized in Fortran 90) so there is some variation.  However, thenamelistalways starts with

&name

The variable list follows, with each variable on a separate line and consisting of thevarname=valuepair.

In older code, thenamelistfrequently ends with another ampersand (&), or&end.  Also, in Fortran 77 there may be rules about in which column the&can occur.

In Fortran 90, thenamelistis terminated with a forward slash/

# Namelist Example

In the program

NAMELIST /params/ rho,eps, x0

OPEN(10,file='paramlist.txt

')

READ(10,params)

The input file (Fortran 90 format)‏

&params

rho=1.3

eps=1.e-7

x0=0.0

/

# Example

program third

character(len=80)                       ::infile,outfile

character(len=40)                       ::common_name,common_name_adj

character(len=20)                       ::species_id

logical                                 ::file_exists

real,   dimension(:),allocatable::num_obs,years

integer                                 ::ios

nargs=command_argument_count()

if (nargs.ne. 1) then

stop "No data file specified"

else

callget_command_argument(1,infile)

endif

inquire(file=infile,exist=file_exists)

if (.not.file_exists) then

stop "Data file not found"

endif

iunit=11

open(iunit,file=infile,iostat=ios)

if (ios.ne. 0) then

stop "Cannot open file"

endif

# Example (Continued)

nlines=0

do

read(iunit,*,end=10)

nlines=nlines+1

enddo

10 continue

rewind(iunit)

! Subtract header line

nyears=nlines-1

allocate(years(nyears),num_obs(nyears))

!Read header

read(iunit,*)

!Read data (only the values of interest)

!Species info is the same on all lines so wedontmake an array

do n=1,nyears

read(iunit,*,iostat=ios)species_id,common_name_adj,common_name, years(n),num_obs(n)

if (ios.ne. 0) stop "Error reading file"

enddo

!Adjust years to absolute number (known in advance)

years=years+1900

print *, "Read observation ",num_obs(85)," for year ",years(85)

end program

# Exercise

Write a program that creates a file mydata.txt containing four rows consisting of

1, 2, 3

4, 5, 6

7, 8, 9

10, 11, 12

Rewind the file and read the data back.  Write a loop to add 1 to each value and print each row to the console.


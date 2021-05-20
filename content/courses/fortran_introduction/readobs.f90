# Example
```fortran
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


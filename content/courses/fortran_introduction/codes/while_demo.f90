program demo
implicit none
    integer  :: x, y, z
    x=-20
    y=-10
    do while (x<0 .and. y<0)
        x=10-y
        y=y+1
        z=0
    enddo
    z=1
    print *, x, y, z
end program

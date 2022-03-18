program splitter

    character(len=256)              :: input, temp
    character(len=80), dimension(:), allocatable :: output
    character                       :: delimiter, ch
    integer                         :: i, ndelims, nchars, chunk

    input="Sample ;string ;for ; this; project \n"
    delimiter=';'
    temp=""

    !First count how many delimiters

    do i=1,len(input)
        if (input(i:i)==delimiter) then
            ndelims=ndelims+1
        endif
    enddo

    !Allocate the array
    allocate(output(ndelims+1))

    chunk=1
    nchars=0
    do i=1,len(input)
        ch=input(i:i)
        nchars=nchars+1
        if (ch==delimiter) then
            output(chunk)=trim(temp)
            temp=""
            nchars=0
            chunk=chunk+1
        endif
        temp(nchars:nchars)=ch
    enddo
    output(chunk)=trim(temp)

    write(*,'(a)',advance="no") "["
    do i=1,size(output)-1
        write(*,'(a,a)',advance="no") trim(output(i)),","
    enddo
    write(*,*) trim(output(size(output))),"]"

end program

   

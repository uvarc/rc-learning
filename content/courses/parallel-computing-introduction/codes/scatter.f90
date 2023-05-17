program scatterthem
    use mpi
    implicit none

    real, dimension(100) :: values
    real, dimension(:), allocatable :: myvals
    integer              :: rank, nprocs, ierr
    integer              :: sendcount, i, n

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    !For illustration only, don't hardcode nprocs
    if (mod(100,nprocs)/=0) then
        if (rank==0) then
            print*, "For this simple example, nprocs must evenly divide 100"
        endif
        call MPI_Finalize(ierr)
        stop 
    endif

    ! Only root has to know the global values
    if (rank==0) then
        do i=1,100
            values(i)=i
        enddo
    endif

    sendcount=100/nprocs
    allocate(myvals(sendcount))

    call MPI_Scatter(values,sendcount,MPI_REAL,myvals,sendcount,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    !Forces each process to write separately and in order
    !Printing here is to demonstrate how the data are distributed
    do n=0,nprocs-1
        call MPI_Barrier(MPI_COMM_WORLD,ierr)
        if (n==rank) then
            write(6,'(i4,a)',advance='no') rank,":"
            do i=1,size(myvals)-1
                write(6,'(f5.0)',advance='no') myvals(i)
            enddo
            write(6,'(f5.0)') myvals(size(myvals))
        endif
    enddo

    call MPI_Finalize(ierr)

end program

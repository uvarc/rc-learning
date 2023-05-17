program gatherthem
    use mpi
    implicit none

    real, dimension(:), allocatable :: myvals
    real, dimension(:), allocatable :: allvals
    integer              :: i,n,nprow
    integer              :: rank, ncount, nprocs, ierr

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    ncount=10

    if (rank==0) then
        allocate(allvals(nprocs*ncount))
    endif
        
    allocate(myvals(ncount))
    ! Load data into local array
    myvals(1)=(rank+1)
    do i=2,ncount
        myvals(i)=myvals(i-1)+rank+1.
    enddo

    call MPI_Gather(myvals,ncount,MPI_REAL,allvals,ncount,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    nprow=ncount
    if (rank == 0) then
        n=1
        do i=1,nprocs
           write(6,'(20f6.0)') allvals(n:n+nprow-1)
           n=n+nprow
        enddo
        write(6,'(20f6.0)') allvals(n:)
    endif

    call MPI_Finalize(ierr)

end program
        


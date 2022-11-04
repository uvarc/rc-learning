program gatherthem
    use mpi
    implicit none

    real, dimension(101) :: values, allvals
    real, dimension(:), allocatable :: myvals
    integer, dimension(8):: sendcounts,offsets,displs
    integer              :: i,n,nprow
    integer              :: start_index,end_index
    integer              :: rank, nprocs, ierr

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    !For illustration only, don't hardcode nprocs
    if (nprocs /= 8) then
        stop "Example requires 8 processes"
    endif

    do i=1,101
        values(i)=i-1
    enddo
    allvals=0.0

    !Hand-distributing the numbers
    sendcounts=[12,12,11,12,13,9,10,8]
    offsets=[0,2,3,1,4,1,1,2]

    displs(1)=offsets(1)
    do i=2,nprocs
        displs(i)=displs(i-1)+sendcounts(i-1)+offsets(i)
    enddo

    allocate(myvals(sendcounts(rank+1)))
    ! Load data into local array
    start_index=displs(rank+1)+1
    end_index=displs(rank+1)+sendcounts(rank+1)
    myvals=values(start_index:end_index)

    call MPI_Gatherv(myvals,sendcounts(rank+1),MPI_REAL,allvals,sendcounts,displs,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    nprow=101/nprocs
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
        


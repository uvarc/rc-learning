program scatterthem
    use mpi
    implicit none

    real, dimension(101) :: values
    real, dimension(:), allocatable :: myvals
    integer, dimension(8):: sendcounts,offsets,displs
    integer              :: i,n,nprow
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

    nprow=101/nprocs
    if (rank == 0) then
        n=1
        do i=1,nprocs
           write(6,'(20f5.0)') values(n:n+nprow-1)
           n=n+nprow
        enddo
        write(6,'(20f5.0)') values(n:)
    endif

    !Hand-distributing the numbers
    sendcounts=[12,12,11,12,13,9,10,8]
    offsets=[0,2,3,1,4,1,1,2]

    displs(1)=offsets(1)
    do i=2,nprocs
        displs(i)=displs(i-1)+sendcounts(i-1)+offsets(i)
    enddo

    allocate(myvals(sendcounts(rank+1)))

    call MPI_Scatterv(values,sendcounts,displs,MPI_REAL,myvals,sendcounts(rank+1),MPI_REAL,0,MPI_COMM_WORLD,ierr)

    !Forces each process to write separately and in order
    !Printing here is to demonstrate how the data are distributed
    do n=0,nprocs-1
        call MPI_Barrier(MPI_COMM_WORLD,ierr)
        if (n==rank) then
            write(6,'(i4)',advance='no') rank
            do i=1,size(myvals)-1
                write(6,'(f5.0)',advance='no') myvals(i)
            enddo
            write(6,'(f5.0)') myvals(size(myvals))
        endif
    enddo

    call MPI_Finalize(ierr)

end program
        


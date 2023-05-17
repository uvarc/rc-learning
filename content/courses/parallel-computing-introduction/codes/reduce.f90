program reduction
    use mpi
    implicit none

    real, dimension(:), allocatable :: myvals
    real                 :: mysum, total
    integer              :: i,n,nprow
    integer              :: rank, ncount, nprocs, ierr

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    ncount=100
        
    allocate(myvals(ncount))
    ! Load data into local array
    myvals(1)=1
    do i=2,ncount
        myvals(i)=myvals(i-1)+1.
    enddo
    myvals=(rank+1)*myvals

    !Local sum
    mysum=sum(myvals)

    call MPI_Reduce(mysum,total,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    
    print*,rank,mysum,total

    call MPI_Finalize(ierr)

end program
        


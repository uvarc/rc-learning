program hello
use mpi

    integer :: myrank, nprocs
    integer :: err

    call MPI_INIT(err)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, err)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, err)

    if ( myrank == 0 ) then
        print *, 'Running on ',nprocs,' Processes'
    endif

    print *, 'Greetings from process ', myrank
    call MPI_FINALIZE(err)


end program

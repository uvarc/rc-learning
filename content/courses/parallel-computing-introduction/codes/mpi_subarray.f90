program mpi_subarray
    use mpi_f08
    implicit none

    integer, parameter :: root = 0
    integer :: ierr, rank, size
    integer :: nrl, ncl
    integer :: sub_rows, sub_cols
    integer :: starts(2), subsizes(2), wsizes(2)
    integer :: i, j
    integer :: lb, ubr, ubc
    integer, allocatable :: w(:,:)
    type(MPI_Datatype)::sendtype,recvtype

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rank)
    call MPI_Comm_size(MPI_COMM_WORLD, size)

    ! For simplicity, run with 2 processes
    if (size /= 2) then
        if (rank == root) print *, "Please run with 2 processes."
        call MPI_Finalize()
        stop
    end if

    ! Define the full array dimensions
    nrl = 6
    ncl = 8

    ! Define the subarray dimensions
    sub_rows = 3
    sub_cols = 4

    lb=0
    ubr=nrl-1
    ubc=ncl-1

    allocate(w(lb:ubr, lb:ubc))

    ! Fill the arrays.  All zeros on rank 1.
    if (rank == root) then
        do i = lb, ubr
            do j = lb, ubc
                w(i, j) = i * 100 + j
            end do
        end do
    else
        w=0
    end if

    ! Set up subarrays
    wsizes = [nrl, ncl]                 ! Full array size
    subsizes = [ sub_rows, sub_cols ]   ! Subarray size

    ! Send array
    starts   = [ 2, 3 ]                 ! Starting indices

    ! Subarray type requires counting from 0
    starts=starts-lb

    call MPI_Type_create_subarray(2, wsizes, subsizes, starts, &
                                  MPI_ORDER_FORTRAN, MPI_INTEGER, sendtype)
    call MPI_Type_commit(sendtype)

    ! Receive array
    starts=[ 3,2 ]-lb

    call MPI_Type_create_subarray(2, wsizes, subsizes, starts, &
                                  MPI_ORDER_FORTRAN, MPI_INTEGER, recvtype, ierr)
    call MPI_Type_commit(recvtype)

    ! Send subarray from root to rank 1
    if (rank == root) then
        print *, "Rank", rank, "sending array:"
        do i = lb, ubr
            write(*,'(*(i6.3))') w(i,:)
        end do
        call MPI_Send(w, 1, sendtype, 1, 0, MPI_COMM_WORLD)
    else
        call MPI_Recv(w, 1 , recvtype, root, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        ! Print received subarray
        print *, "Rank", rank, "received subarray:"
        do i = lb, ubr
            write(*,'(*(i6.3))') w(i,:)
        end do
    end if

    call MPI_Type_free(sendtype)
    call MPI_Type_free(recvtype)
    call MPI_Finalize()

end program mpi_subarray

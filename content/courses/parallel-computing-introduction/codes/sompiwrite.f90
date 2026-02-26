program mpiwrite

  use mpi
  implicit none

  integer, parameter:: ngrid=2
  integer, parameter:: jd=3, kd=3, ld=3, nlocq=3, nqc=-1
  integer :: nq

  integer, parameter :: refmach=1, alpha=2, rey=3, time=4, gaminf=5
  integer, parameter :: beta=6, tinf=7

  integer, dimension(jd,kd,ld,nlocq) :: q

  integer :: intsize
  integer :: subarray

  integer :: fileh
  integer(kind=MPI_Offset_kind) :: offset

  integer :: comsize, rank, ierr

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  nq = nlocq * comsize
  q = rank

  ! create a subarray; each processor gets its own q-slice of the
  ! global array
  call MPI_Type_create_subarray (4, [jd, kd, ld, nq], [jd, kd, ld, nlocq], &
                                    [0, 0, 0, nlocq*rank], &
                                    MPI_ORDER_FORTRAN, MPI_INTEGER, subarray,  ierr)
  call MPI_Type_commit(subarray, ierr)

  call MPI_File_open(MPI_COMM_WORLD, 'mpi.dat',         &
                     MPI_MODE_WRONLY + MPI_MODE_CREATE, &
                     MPI_INFO_NULL, fileh, ierr )

  ! the header size is:
  !  1 field of 1 integer ( = 4*(1 + 1 + 1) = 12 bytes )
  ! +1 field of 5 integers( = 4*(1 + 5 + 1) = 28 bytes )
  ! +1 field of 7 integers( = 4*(1 + 7 + 1) = 36 bytes )
  ! +first bookend of array size = 4 bytes
  offset = 12 + 28 + 36 + 4

  print *, size(q),jd*kd*ld*nlocq, offset, jd*kd*ld*nq*4
  ! rank 1 writes the header and footer
  if (rank == 0) then
      call MPI_File_write(fileh, [4, ngrid, 4], 3, MPI_INTEGER, &
                          MPI_STATUS_IGNORE, ierr)
      call MPI_File_write(fileh, [20, jd, kd, ld, nq, nqc, 20], 7, MPI_INTEGER,&
                          MPI_STATUS_IGNORE, ierr)
      call MPI_File_write(fileh, &
                       [28, refmach, alpha, rey, time, gaminf, beta, tinf, 28],&
                         9, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

      call MPI_File_write(fileh, [jd*kd*ld*nq*4], 1, MPI_INTEGER,  &
                         MPI_STATUS_IGNORE, ierr)
      call MPI_File_seek(fileh, offset+jd*kd*ld*nq*4, MPI_SEEK_CUR, ierr)
      call MPI_File_write(fileh, [jd*kd*ld*nq*4], 1, MPI_INTEGER,  &
                         MPI_STATUS_IGNORE, ierr)
  endif

  ! now everyone dumps their part of the array
  call MPI_File_set_view(fileh, offset, MPI_INTEGER, subarray,   &
                                'native', MPI_INFO_NULL, ierr)
  call MPI_File_write_all(fileh, q, jd*kd*ld*nlocq, MPI_INTEGER, &
                                MPI_STATUS_IGNORE, ierr)

  call MPI_File_close(fileh, ierr)

  CALL MPI_Finalize(ierr)

end program mpiwrite

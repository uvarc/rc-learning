program mpiwrite
   use mpi_f08
   implicit none

   integer            :: i
   character(len=80)  :: arg
   integer            :: numargs

   integer            :: rank, nprocs
   integer            :: mpi_err
   integer, parameter :: root=0
   type(MPI_Status)   :: mpi_stat
   type(MPI_File)     :: fh
   integer            :: amode
   integer            :: tsize
   INTEGER(KIND=MPI_OFFSET_KIND) :: fsize, offset=0
   character(len=24)  :: fname
   character(len=1)   :: my_char
   integer            :: nreps

   character(len=:), allocatable :: gu,u
   character(len=256) :: rbuf

   ! check number of parameters and read in filename
   ! all ranks do this, avoids broadcast
   numargs=command_argument_count()
   if (numargs .lt. 1) then
      stop 'USAGE: output-file'
   else
      call get_command_argument(1,fname)
   endif

   !Initialize MPI
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

   my_char=char(iachar('A')+rank)

   amode=ior(MPI_MODE_CREATE, MPI_MODE_WRONLY)
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)

   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   endif

   nreps=20
   do i=1,nreps
      offset=rank+(i-1)*nprocs
      call MPI_FILE_WRITE_AT(fh, offset, my_char, 1, MPI_CHARACTER, mpi_stat)
   enddo

   call MPI_FILE_CLOSE(fh)

   ! Read as ordinary file
   ! Still can't read variable-length strings in Fortran
   if (rank==root) then
       rbuf=' '
       open(unit=10,file=fname,status='unknown')
       read(10,*) rbuf
       gu=trim(rbuf)
       write(*,'(a,a)') "Read at root ",gu
       close(10)
   endif

   !All processes read entire file
   !Blocking so will wait for root to finish above read
   amode=MPI_MODE_RDONLY
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)
   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   endif

   !Reading into "big enough" buffer means we don't need to get the size,
   !but this is the syntax.  Note type of fsize in declarations
   call MPI_FILE_GET_SIZE(fh,fsize)
   call MPI_TYPE_SIZE(MPI_CHARACTER,tsize)
   if (rank==root) then
       write(*,*) "File size is ",fsize," Type size is ",tsize," bytes"
   endif

   rbuf=' '
   call MPI_FILE_READ_ALL(fh, rbuf, len(rbuf), MPI_CHARACTER, mpi_stat)
   gu=trim(rbuf)
   write(*,'(a,i4.4,a,a)') 'Full file at rank ',rank,' ',gu

   !Read back the MPI file portion for reach rank
   rbuf=' '
   amode=MPI_MODE_RDONLY
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)
   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   else
       do i=1,nreps
          call MPI_FILE_READ_AT(fh, offset, rbuf(i:i), 1, MPI_CHARACTER, mpi_stat)
       enddo
   endif
   write(*,'(a,i4.4,a,a)') 'Rank ',rank,' ',trim(rbuf)

   call MPI_FILE_CLOSE(fh)

   call MPI_Finalize()

end program

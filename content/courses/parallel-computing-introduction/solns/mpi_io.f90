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
   integer            :: tsize, nvals
   integer(kind=MPI_OFFSET_KIND) :: fsize, offset=0
   character(len=24)  :: fname
   double precision   :: my_val
   double precision, parameter   :: pi=4.*atan(1.d0)
   double precision, allocatable, dimension(:) :: gu
   double precision   :: u

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

   my_val=(rank+1)*pi

   call MPI_TYPE_SIZE(MPI_DOUBLE_PRECISION,tsize)

   amode=ior(MPI_MODE_CREATE, MPI_MODE_WRONLY)
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)

   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   endif

   offset=rank*tsize
   call MPI_FILE_WRITE_AT(fh, offset, my_val, 1, MPI_DOUBLE_PRECISION, mpi_stat)
   call MPI_FILE_CLOSE(fh)

   !All processes read entire file
   amode=MPI_MODE_RDONLY
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)
   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   endif

   call MPI_FILE_GET_SIZE(fh,fsize)
   nvals=fsize/tsize
   allocate(gu(nvals))

   call MPI_FILE_READ_ALL(fh, gu, size(gu), MPI_DOUBLE_PRECISION, mpi_stat)
   write(*,'(a,i4.4,a,*(g11.6))') 'Full file at rank ',rank,'   ',gu
   call MPI_FILE_CLOSE(fh)

   !Read back the MPI file portion for reach rank
   amode=MPI_MODE_RDONLY
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)
   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   endif
   call MPI_FILE_READ_AT(fh,offset,u,1,MPI_DOUBLE_PRECISION, mpi_stat)
   write(*,'(a,i4.4,a,g0.6)') 'Rank ',rank,' ',u

   call MPI_FILE_CLOSE(fh)

   call MPI_Finalize()

end program

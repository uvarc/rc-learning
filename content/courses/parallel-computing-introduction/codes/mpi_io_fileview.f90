program mpiwrite
   use mpi_f08
   implicit none

   integer            :: i,j
   character(len=80)  :: arg
   integer            :: numargs

   integer            :: nrows, ncols
   integer            :: nrl, ncl, lrow, lcol
   integer            :: N, M

   integer            :: rank, nprocs
   integer            :: mpi_err
   integer, parameter :: root=0
   type(MPI_Status)   :: mpi_stat
   type(MPI_File)     :: fh
   type(MPI_Datatype) :: locarray
   integer            :: amode
   integer            :: lusize
   integer, dimension(2) :: gdims, ldims, starts
   integer(kind=MPI_OFFSET_KIND) :: offset=0
   integer, allocatable, dimension(:,:) :: loc_u, u, rbuf
   character(len=24)  :: fname
   character(len=80)  :: myfile

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

   ! Hard-code sizes so we can see what we're doing
   ! Row/column layout for ranks
   nrows=4
   ncols=4

   nrl = 4
   ncl = 4

   N=nrl*nrows
   M=nrl*ncols

   !Set up the topology
   lrow=rank/ncols
   lcol=mod(rank,ncols)

   allocate(loc_u(nrl,ncl))
   loc_u=(rank+1)

   !Write each segment to conventional file (the "old way")
   write(myfile,'(a,i2.2)') fname(1:len_trim(fname)),rank

   open(unit=10,file=myfile,status='unknown')
   do i=1,nrl
      write (10,'(*(g0,1x))') loc_u(i,:)
   enddo
   close(10)

   gdims=[N,M]
   ldims=[nrl,ncl]
   starts=[ncl*lrow,nrl*lcol]
   write(*,*) rank,starts

   call MPI_TYPE_CREATE_SUBARRAY(2, gdims, ldims, starts, MPI_ORDER_FORTRAN,   &
                                                          MPI_INTEGER, locarray)

   call MPI_TYPE_COMMIT(locarray)

   amode=ior(MPI_MODE_CREATE, MPI_MODE_WRONLY)
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)

   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for writing"
   endif

   offset=0
   call MPI_FILE_SET_VIEW(fh, offset, MPI_INTEGER, locarray,                   &
                                    'native', MPI_INFO_NULL)

   call MPI_FILE_WRITE_ALL(fh,loc_u, size(loc_u), MPI_INTEGER, mpi_stat)
   call MPI_FILE_CLOSE(fh)

   !All processes read file
   amode=MPI_MODE_RDONLY
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)
   if ( mpi_err /= MPI_SUCCESS) then
       call MPI_FINALIZE()
       stop "Unable to open MPI file for reading"
   endif

   allocate(u(nrl,ncl),rbuf(nrl,ncl))
   rbuf=0
   lusize=size(u)

   call MPI_FILE_SET_VIEW(fh, offset, MPI_INTEGER, locarray,                   &
                                    'native', MPI_INFO_NULL)

   call MPI_FILE_READ_ALL(fh, rbuf, lusize, MPI_INTEGER, mpi_stat)
   call MPI_FILE_CLOSE(fh)

  !Usual trick to print one rank at a time
   if (rank==0) then
       write(*,*) 'Result for rank 0'
       do j=1,nrl
          write(*,'(*(i10))') rbuf(j,:)
       enddo
       do i=1,nprocs-1
          u=0
          call MPI_Recv(u,lusize, MPI_INTEGER,MPI_ANY_SOURCE,                  &
                                 MPI_ANY_TAG, MPI_COMM_WORLD,mpi_stat)
          write(*,*) 'Result for rank',mpi_stat%MPI_SOURCE
          do j=1,nrl
             write(*,'(*(i10))') u(j,:)
          enddo
       enddo
   else
       call MPI_Send(rbuf,lusize,MPI_INTEGER,0,rank,MPI_COMM_WORLD)
   endif

   call MPI_TYPE_FREE(locarray)

   call MPI_Finalize()

end program

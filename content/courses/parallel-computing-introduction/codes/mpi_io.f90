program mpiwrite
   use mpi_f08

   integer            :: N, M
   integer            :: i,j
   character(len=80)  :: arg
   integer, allocatable, dimension(:,:)  :: loc_u

   integer            :: rank, nprocs, nrows, ncols
   integer            :: lrow, lcol, nrl, ncl
   integer, parameter :: root=0
   type(MPI_Status)   :: mpi_stat
   type(MPI_Datatype) :: locarr
   type(MPI_File)     :: fh
   integer            :: amode
   integer            :: mpi_err, gmpi_err
   integer(kind=MPI_OFFSET_KIND) :: disp=0
   character(len=24)  :: fname
   integer            :: ndims=2
   integer, dimension(2) :: ldims, gdims, start_arr
   character(len=36)  :: myfile


   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

   ! check number of parameters and read in epsilon, filename, optionally size
   ! all ranks do this, avoids broadcast
   numargs=command_argument_count()
   if (numargs .lt. 2) then
      call MPI_Finalize()
      stop 'USAGE: output-file <nrows> <ncols>'
   else
      call get_command_argument(1,fname)

      if (numargs==1) then
         nrows=4
         ncols=4
      endif
      if (numargs==2) then
         call get_command_argument(2,arg)
         read(arg,*) nrows
         ncols=nrows
      endif
      if (numargs==3) then
         call get_command_argument(2,arg)
         read(arg,*) nrows
         call get_command_argument(3,arg)
         read(arg,*) ncols
      endif
   endif
   !Check for correct decomposition
   if ( nrows*ncols /= nprocs ) then
       call MPI_Finalize()
       stop "nrows*ncols must equal number of processes"
   endif

   !Grid coordinates
   lrow=rank/ncols
   lcol=mod(rank,ncols)

  
   !Hardcode each local array to be relatively small so we can see 
   !what we're doing
   nrl=4
   ncl=4

   !Global array size
   N=nrl*nrows
   M=ncl*ncols

   ! Set up values
   allocate(loc_u(nrl,ncl))
   do i=1,nrl
      do j=1,ncl
         loc_u(i,j)=(rank+1)*(i+j)
      enddo
   enddo

   gdims=[N,M]
   ldims=[nrl,ncl]
   start_arr=[ncl*lrow,nrl*lcol]
   print *, rank, lrow, lcol, start_arr

   write(myfile,'(a,i2.2)') trim(fname),rank
   open(10,file=myfile)
   write(10,*) rank
   do i=1,nrl
       write(10,*) loc_u(i,:)
    enddo

   !Define a subarray for each local array within the global array
   call MPI_TYPE_CREATE_SUBARRAY(ndims, gdims, ldims, start_arr,               &
                                 MPI_ORDER_FORTRAN, MPI_INTEGER, locarr)
   call MPI_TYPE_COMMIT(locarr)

   amode=ior(MPI_MODE_CREATE, MPI_MODE_WRONLY)
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)

   call MPI_Allreduce(mpi_err, gmpi_err,1,MPI_INTEGER, MPI_LOR, MPI_COMM_WORLD)

   if ( gmpi_err /= MPI_SUCCESS ) then
      stop "Unable to open MPI file, terminating"
   endif

   !Need a header for the sizes, only root should write this
   if ( rank==0 ) then
       call MPI_File_write(fh, [N, M], 2, MPI_INTEGER, MPI_STATUS_IGNORE)
   endif

   !Everybody write its section
   disp=2*sizeof(N)

   call MPI_FILE_SET_VIEW(fh,disp,MPI_INTEGER,locarr,"native", MPI_INFO_NULL)
   call MPI_FILE_WRITE_ALL(fh, loc_u, size(loc_u), MPI_INTEGER, mpi_stat)

   call MPI_FILE_CLOSE(fh)


   call MPI_Type_free(locarr)
   call MPI_Finalize()

end program

program mpiwrite
   use mpi_f08
   implicit none

   integer            :: N, M
   integer            :: i,j
   character(len=80)  :: arg
   integer, allocatable, dimension(:,:)  :: u, gu
   integer            :: numargs

   integer            :: rank, nprocs, nrows, ncols
   integer            :: lrow, lcol
   integer            :: nrl, ncl, nr_total, nc_total, nghosts
   integer, parameter :: root=0
   type(MPI_Status)   :: mpi_stat
   type(MPI_Datatype) :: locarr, fullarr
   type(MPI_File)     :: fh
   integer            :: amode
   integer            :: mpi_err, gmpi_err
   integer(kind=MPI_OFFSET_KIND) :: disp=0
   character(len=24)  :: fname
   integer            :: ndims=2
   integer, dimension(2) :: starts,sizes,subsizes
   integer, dimension(2) :: gstarts,gsizes,gsubsizes
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

   N=nrl*nrows
   M=ncl*ncols

   nghosts=2
   nr_total=nrl+2*nghosts
   nc_total=ncl+2*nghosts

   ! Set up values
   allocate(u(0:nr_total-1,0:nc_total-1))

   u=-9
   do i=nghosts,nrl+nghosts-1
      do j=nghosts,ncl+nghosts-1
         u(i,j)=rank
      enddo
   enddo

   !array sizes
   gsizes=[N,M]
   sizes=[nr_total,nc_total]

   write(myfile,'(a,i2.2)') trim(fname),rank
   open(10,file=myfile)
   write(10,*) rank
   do i=1,nrl
       write(10,*) u(i,:)
    enddo

   starts=[nghosts,nghosts]
   subsizes=[nrl,ncl]
   !Define a subarray for each local array
   !Size includes ghost zones, starts picks out locations
   call MPI_TYPE_CREATE_SUBARRAY(ndims, sizes, subsizes, starts,               &
                                 MPI_ORDER_FORTRAN, MPI_INTEGER, locarr)
   call MPI_TYPE_COMMIT(locarr)

   !Create the subarray for the global file view
   !Excludes ghost zones
   !Remember that subarry starts assume 0 lower bound like C
   gsizes=[N,M]
   gstarts=[lrow*nrl,lcol*ncl]
   gsubsizes=[nrl,ncl]
   call MPI_TYPE_CREATE_SUBARRAY(ndims, gsizes, gsubsizes, gstarts,            &
                                 MPI_ORDER_FORTRAN, MPI_INTEGER, fullarr)
   call MPI_TYPE_COMMIT(fullarr)

   amode=ior(MPI_MODE_CREATE, MPI_MODE_WRONLY)
   call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)

   call MPI_Allreduce(mpi_err,gmpi_err,1,MPI_INTEGER, MPI_BOR, MPI_COMM_WORLD)

   if ( gmpi_err /= MPI_SUCCESS ) then
      stop "Unable to open MPI file, terminating"
   endif

   !Need a header for the sizes, only root should write this
   !if ( rank==0 ) then
   !    call MPI_File_write(fh, [N, M], 2, MPI_INTEGER, MPI_STATUS_IGNORE)
   !endif

   call MPI_FILE_SET_VIEW(fh,disp,MPI_INTEGER,fullarr,"native", MPI_INFO_NULL)
   call MPI_FILE_WRITE_ALL(fh, u, 1, locarr, mpi_stat)

   call MPI_FILE_CLOSE(fh)

   !Read it back in
   if (rank==root) then
      allocate(gu(0:N-1,0:M-1))
      print *, 'allocated gu', size(gu)
      amode=MPI_MODE_RDONLY
      print *, 'Opening file'
      call MPI_FILE_OPEN(MPI_COMM_WORLD,trim(fname),amode,MPI_INFO_NULL,fh,mpi_err)
      print *, 'Opened file ',trim(fname)
      if ( mpi_err /= MPI_SUCCESS) then
          stop "Unable to open MPI file for reading"
      endif
      print *, "Starting to read"

      call MPI_FILE_READ(fh, gu, size(gu), MPI_INTEGER, mpi_stat)

   endif


   call MPI_Type_free(locarr)
   call MPI_Type_free(fullarr)

   call MPI_Finalize()

end program

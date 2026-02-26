program halo
   use mpi

   double precision, allocatable, dimension(:,:)  :: u,w
   integer            :: N=12
   double precision   :: topBC, bottomBC, leftBC, rightBC
   integer            :: i

   integer            :: nr, nc, nrl, ncl
   integer            :: rank, nprocs, tag=0
   integer            :: ierr
   integer, dimension(MPI_STATUS_SIZE) :: mpi_stat
   integer, parameter :: root=0
   integer            :: left, right

   !Initialize MPI, get the local number of columns
   call MPI_INIT(ierr)
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

   !For this case
   !Usually we'd read in nr and nc; they represent the global grid size RxC.
   nr=N
   nc=N

   !Generally we'd just use nr and not duplicate; this is for clarity later
   nrl=nr

   ! weak scaling
   !ncl = nc
   ! strong scaling
   if (mod(nc,nprocs).ne.0) then
       call MPI_FINALIZE(ierr)
       stop 'Not an even division of the array'
   else
       ncl = nc/nprocs
   endif

   ! Find my neighbors
   if ( rank .eq. root ) then
      left = MPI_PROC_NULL
   else
      left = rank - 1
   endif

   if ( rank .eq. nprocs-1 ) then
      right = MPI_PROC_NULL
   else
      right = rank + 1
   endif

   !In this code, u is only used for verification of results
   allocate(w(0:nrl+1,0:ncl+1),u(0:nrl+1,0:ncl+1))

   ! Initialize the array
   w(:,:)=50.
   !Set up rows to exchange
   w(:,1)=dble(rank+1)*2
   w(:,ncl)=dble(rank+1)*2.5

   ! Boundary conditions
   topBC=0.d0
   bottomBC=200.d0
   leftBC=100.d0
   rightBC=100.d0

   !Fortran stitches by column
   w(0,:)=topBC
   w(nrl+1,:)=bottomBC
   if (rank==0) then
      w(1:nrl,0)=leftBC
   else if (rank==nprocs-1) then
      w(1:nrl,ncl+1)=rightBC
   endif

   write(*,*) "Topology ",rank, left, right

   !starting
   !This forces the output to show one rank at a time. It's not efficient.
   if (rank==0) then
       write(*,*) 'Initial for rank 0'
          do j=0,nrl+1
             write(*,'(*(f10.2))') w(j,:)
          enddo
       do i=1,nprocs-1
          u=0.d0
          call MPI_Recv(u,size(u),MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,         &
                        MPI_ANY_TAG, MPI_COMM_WORLD,mpi_stat,ierr)
          write(*,*) 'Initial for rank',mpi_stat(MPI_SOURCE)
          do j=0,nrl+1
             write(*,'(*(f10.2))') u(j,:)
          enddo
       enddo
   else
       call MPI_Send(w,size(w),MPI_DOUBLE_PRECISION,0,rank,MPI_COMM_WORLD,ierr)
   endif

   call MPI_Barrier(MPI_COMM_WORLD,ierr)

   ! Exchange halo values
   call MPI_SENDRECV(w(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,         &
                     w(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,        &
                                        MPI_COMM_WORLD,mpi_stat,ierr)


   call MPI_SENDRECV(w(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,        &
                     w(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,         &
                                        MPI_COMM_WORLD,mpi_stat,ierr)

   !result
   if (rank==0) then
       write(*,*) 'Result for rank 0'
          do j=0,nrl+1
             write(*,'(*(f10.2))') w(j,:)
          enddo
       do i=1,nprocs-1
          u=0.d0
          call MPI_Recv(u,size(u),MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,         &
                        MPI_ANY_TAG, MPI_COMM_WORLD,mpi_stat,ierr)
          write(*,*) 'Result for rank',mpi_stat(MPI_SOURCE)
          do j=0,nrl+1
             write(*,'(*(f10.2))') u(j,:)
          enddo
       enddo
   else
       call MPI_Send(w,size(w),MPI_DOUBLE_PRECISION,0,rank,MPI_COMM_WORLD,ierr)
   endif

   call MPI_FINALIZE(ierr)

end program 


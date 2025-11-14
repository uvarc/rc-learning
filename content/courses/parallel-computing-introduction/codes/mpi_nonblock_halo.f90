program halo
   use mpi_f08

   double precision, allocatable, dimension(:,:)  :: w,u
   integer            :: N=12
   double precision   :: topBC, bottomBC, edgeBC
   integer            :: i

   integer            :: nr, nc, nrl, ncl
   integer            :: rank, nprocs, tag=0
   type(MPI_Status),  dimension(:), allocatable :: mpi_status_arr
   type(MPI_Request), dimension(:), allocatable :: mpi_requests
   integer            :: nrequests
   integer, parameter :: root=0
   integer            :: left, right
   integer            :: usize
   type(MPI_Status)   :: check_stat

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

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
       call MPI_FINALIZE()
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

   allocate(w(0:nrl+1,0:ncl+1))

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


   ! Exchange halo values
   nrequests=4
   allocate(mpi_requests(nrequests),mpi_status_arr(nrequests))

   call MPI_Irecv(w(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,           &
                                     MPI_COMM_WORLD,mpi_requests(1))

   call MPI_Irecv(w(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,            &
                                     MPI_COMM_WORLD,mpi_requests(2))


   call MPI_Isend(w(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,            &
                                     MPI_COMM_WORLD,mpi_requests(3))

   call MPI_Isend(w(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,           &
                                     MPI_COMM_WORLD,mpi_requests(4))

   call MPI_Waitall(nrequests,mpi_requests,mpi_status_arr)

   !Spot-check result
   ! u only used here, strictly only rank 0 needs it

      if (rank==0) then

          allocate(u(0:nrl+1,0:ncl+1))
          usize=size(u)

          write(*,*) 'Result for rank 0'
          do j=0,nrl+1
             write(*,'(*(f10.2))') w(j,:)
          enddo
       do i=1,nprocs-1
          u=0.d0
          call MPI_Recv(u,usize,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,         &
                        MPI_ANY_TAG, MPI_COMM_WORLD,check_stat)
          write(*,*) 'Result for rank',check_stat%MPI_SOURCE
          do j=0,nrl+1
             write(*,'(*(f10.2))') u(j,:)
          enddo
       enddo
   else
       call MPI_Send(w,size(w),MPI_DOUBLE_PRECISION,0,rank,MPI_COMM_WORLD)
   endif

   call MPI_FINALIZE()

end program 


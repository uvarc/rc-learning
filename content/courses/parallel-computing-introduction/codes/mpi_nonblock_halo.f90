program halo
   use mpi_f08

   double precision, allocatable, dimension(:,:)  :: w
   integer            :: N=500
   double precision   :: topBC, bottomBC, edgeBC
   integer            :: i

   integer            :: nr, nc, nrl, ncl
   integer            :: rank, nprocs, tag=0
   integer            :: errcode, ierr
   type(MPI_Status),  dimension(:), allocatable :: mpi_status_arr
   type(MPI_Request), dimension(:), allocatable :: mpi_requests
   integer            :: nrequests
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

   allocate(w(0:nrl+1,0:ncl+1))

   ! Boundary conditions
   topBC=0.d0
   bottomBC=100.d0
   edgeBC=100.d0

   !Set boundary conditions
   if (rank==0) then
      w(:,0)=edgeBC
   endif
   if (rank==nprocs-1) then
      w(:,ncl+1)=edgeBC
   endif
   w(nrl+1,:)=bottomBC
   w(0,:)=topBC

   ! Initialize the array interior
   w(1:nrl,1:ncl)=50.

   ! Exchange halo values
   nrequests=4
   allocate(mpi_requests(nrequests),mpi_status_arr(nrequests))

   call MPI_Irecv(w(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,           &
                                     MPI_COMM_WORLD,mpi_requests(1),ierr)

   call MPI_Irecv(w(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,            &
                                     MPI_COMM_WORLD,mpi_requests(2),ierr)


   call MPI_Isend(w(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,            &
                                     MPI_COMM_WORLD,mpi_requests(3),ierr)

   call MPI_Isend(w(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,           &
                                     MPI_COMM_WORLD,mpi_requests(4),ierr)

   call MPI_Waitall(nrequests,mpi_requests,mpi_status_arr)

   !Spot-check result
   do i=0,nprocs-1
      if (i==rank) then 
          write (*,*) rank, w(nrl/2,0), w(nrl/2,ncl+1)
      endif
   enddo

   call MPI_FINALIZE(ierr)

end program 


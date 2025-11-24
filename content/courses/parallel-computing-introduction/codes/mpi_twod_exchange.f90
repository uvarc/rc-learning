program twod_exchange
   use mpi_f08
   implicit none

   integer            :: N, M
   integer            :: i,j

   integer            :: rank, nprocs, tag=0
   integer            :: nproc_rows, nproc_cols, my_row, my_col, nrl, ncl
   integer            :: left, right, up, down
   integer            :: uwsize

   double precision, dimension(:,:), allocatable  :: w, u
   double precision   :: topBC, bottomBC, leftBC, rightBC

   type(MPI_Status)   :: mpi_stat

   type(MPI_Datatype) :: row

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

!This example exchanges data among four rectangular domains with halos.
!Most real codes use squares, but we want to illustrate how to use different
!dimensions.

!Divide up the processes.  Either we require a perfect square, or we
!must specify how to distribute by row/column.  In a realistic program,
!the process distribution (either the total, for a perfect square, or
!the rows/columns) would be read in and we would need to check that the number
!of processes requested is consistent with the decomposition.

   N = 8
   M = 12

   nproc_rows=2
   nproc_cols=3

   if (nproc_rows*nproc_cols /= nprocs) then
       call MPI_Finalize()
       stop "Number of rows times columns does not equal nprocs"
   endif

   if (mod(N,nproc_rows)==0 .and. mod(M,nproc_cols)==0) then
       nrl = N/nproc_rows
       ncl = M/nproc_cols
   else
       call MPI_Finalize()
       stop "Number of ranks should divide the number of rows evenly."
   endif

   allocate(w(0:nrl+1, 0:ncl+1),u(0:nrl+1,0:ncl+1))
   w=reshape([(i,i=1,(nrl+2)*(ncl+2))],(/nrl+2,ncl+2/))
   w=(rank+1)*w

!Set up the topology assuming processes numbered left to right by row
   my_row=rank/nproc_cols
   my_col=mod(rank,nproc_cols)

   ! Setting up the neighbors for each process
   if (my_row == 0) then
       up = MPI_PROC_NULL
   else 
       up = rank - nproc_cols
   endif

   if (my_row == nproc_rows-1) then
       down = MPI_PROC_NULL
   else 
       down = rank + nproc_cols
   endif

   if (my_col == 0) then
       left = MPI_PROC_NULL
   else
       left = rank-1
   endif

   if (my_col == nproc_cols-1) then
       right = MPI_PROC_NULL
   else
       right = rank+1
   endif

 ! Boundary conditions

   topBC=0.d0
   bottomBC=200.d0
   leftBC=100.d0
   rightBC=100.d0

   if (my_row==0) then
      w(0,:)=topBC
   endif
   if (my_row==nproc_rows-1) then
      w(nrl+1,:)=bottomBC
   endif
   if (my_col==0) then
      w(:,0)=leftBC
   endif
   if (my_col==nproc_cols-1) then
      w(:,ncl+1)=rightBC
   endif

   call MPI_Type_vector(ncl+2, 1, nrl+2, MPI_DOUBLE_PRECISION, row)
   call MPI_Type_commit(row)

   tag=0

   write(*,*) "Topology ",rank, left, right

   !starting
   !This forces the output to show one rank at a time. It's not efficient.
   uwsize=size(u)
   if (rank==0) then
       write(*,*) 'Initial for rank 0'
          do j=0,nrl+1
             write(*,'(*(f10.2))') w(j,:)
          enddo
       do i=1,nprocs-1
          u=0.d0
          call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,         &
                        MPI_ANY_TAG, MPI_COMM_WORLD,mpi_stat)
          write(*,*) 'Initial for rank',mpi_stat%MPI_SOURCE
          do j=0,nrl+1
             write(*,'(*(f10.2))') u(j,:)
          enddo
       enddo
   else
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,0,rank,MPI_COMM_WORLD)
   endif

   call MPI_Barrier(MPI_COMM_WORLD)


   ! Exchange halo values

   ! Send left and right

   call MPI_SENDRECV(w(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,     &
                     w(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,    &
                                         MPI_COMM_WORLD,mpi_stat)
   call MPI_SENDRECV(w(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,    &
                     w(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,     &
                                         MPI_COMM_WORLD,mpi_stat)

   ! Send up and down

   call MPI_SENDRECV(w(1,0),1, row, up,tag, w(nrl+1,0),1, row,down,tag,     &
                                                  MPI_COMM_WORLD,mpi_stat)
   call MPI_SENDRECV(w(nrl,0),1,row, down,tag, w(0,0)    ,1,row, up,tag,    &
                                                  MPI_COMM_WORLD,mpi_stat)


   !result
   if (rank==0) then
       write(*,*) 'Result for rank 0'
          do j=0,nrl+1
             write(*,'(*(f10.2))') w(j,:)
          enddo
       do i=1,nprocs-1
          u=0.d0
          call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,         &
                        MPI_ANY_TAG, MPI_COMM_WORLD,mpi_stat)
          write(*,*) 'Result for rank',mpi_stat%MPI_SOURCE
          do j=0,nrl+1
             write(*,'(*(f10.2))') u(j,:)
          enddo
       enddo
   else
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,0,rank,MPI_COMM_WORLD)
   endif


   call MPI_Type_free(row)

   call MPI_Finalize()

end program

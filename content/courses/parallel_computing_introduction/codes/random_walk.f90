program random_walk
use random
implicit none

   integer              :: nargs
   character(len=10)    :: ns,nexp
   integer              :: m, n, ntrials, nsteps, step
   real, dimension(2)   :: pos, move
   real                 :: distance, avg

   nargs=command_argument_count()
   if ( nargs .ne. 1 ) then
      write(*,*) 0, 0., 0.
      stop
   else
      call get_command_argument(1,ns)
      read(ns,'(i10)') nsteps
   endif

   call set_random_seed()
   pos=[0.,0.]

   avg=0.

   do n=1,nsteps

      step=randint(1,4)
      if (step==1) then
         move=[0.,1.]
      else if (step==2) then
         move=[0.,-1.]
      else if (step==3) then
         move=[1.,0.]
      else
         move=[-1.,0.]
      endif

      pos=pos+move
   enddo

   distance=sqrt(pos(1)**2+pos(2)**2)

   write(*,*) nsteps, sqrt(real(nsteps)), distance

end program

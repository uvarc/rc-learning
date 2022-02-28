program read_params
implicit none

   real   ::  rho, eps, x0
   namelist /params/ rho,eps, x0

   open(10,file='paramlist.txt')
   read(10,params)

   print *, "Rho=",rho," eps=",eps," x0=",x0

end program


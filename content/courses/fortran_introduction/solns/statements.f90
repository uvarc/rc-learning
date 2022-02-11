program simple
! Play around with expressions
implicit none

real     ::  x, Xs
integer  ::  num_1, num_2

   x=17.
   Xs=11.
   num_1=10
   num_2=14

   print *, x
   print *, Xs/x
   print *, int(Xs/x)
   print *, int(Xs)/int(x)
   print *, Xs/x + x
   print *, Xs/(x+x)
   print *, x/num_1
   print *, num_1/num_2
   print *, num_2/num_1

end program

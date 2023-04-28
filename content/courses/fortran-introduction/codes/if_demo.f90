program ifdemo
implicit none
   
   logical :: L1=.true., L2=.true.

   if (L1) then
      print *, "The if"
   else if (L2) then
      print *, "The else if"
   else
      print *, "The else"
   endif

end program  

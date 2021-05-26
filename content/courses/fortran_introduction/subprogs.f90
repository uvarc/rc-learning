PROGRAM  myprog
   REAL     :: var1, var2, var3
   REAL     :: z
   REAL     :: myfunc

   var1=11.; var2=12.; var3=13.
   z=4.*myfunc(var1,var2,var3)
   PRINT *, z
   CALL mysub(var1,var2,var3)
   PRINT *, var3

END PROGRAM

REAL FUNCTION myfunc(param1,param2,param3)
   REAL, INTENT(IN) :: param1, param2, param3
   if (param3 /= 0) then
      myfunc=param1*param2/param3
   else
      myfunc=0.
   endif
END FUNCTION

SUBROUTINE mysub(param1,param2,param3)
   REAL, INTENT(IN)   :: param1, param2
   REAL, INTENT(OUT)  :: param3
   param3=param1-param2
END SUBROUTINE


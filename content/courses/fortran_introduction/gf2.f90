program test_rank
            integer :: a
            real, dimension(2,3)  :: b
            real, dimension(2,3)  :: c
            b=2.
            c=1.
            b(1,2)=4.
            c(1,2)=-1.

            print *, merge(b,c,c<0)
          end program test_rank


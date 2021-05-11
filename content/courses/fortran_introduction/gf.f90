program test_all
            logical l
            call section
            contains
              subroutine section
                integer a(2,3), b(2,3)
                a = 1
                b = 1
                b(2,2) = 2
                print *, all(a .eq. b, 1)
                print *, all(a .eq. b, 2)
                print *, all(a .eq. b)
              end subroutine section
          end program test_all


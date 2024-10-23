PROGRAM test_sum
INTEGER, DIMENSION(2)  :: S
INTEGER, DIMENSION(12) :: X
INTEGER, DIMENSION(4,3):: A

    x=[(i,i=1,12)]
    s=[3,4]
    A=RESHAPE(x,(/4,3/))
    print *, SIZE(A),SHAPE(x),SHAPE(A)

END PROGRAM



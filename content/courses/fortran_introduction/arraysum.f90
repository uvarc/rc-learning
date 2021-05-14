PROGRAM test_sum
INTEGER, DIMENSION(2)  :: S
INTEGER, DIMENSION(12) :: X
INTEGER, DIMENSION(4,3):: A

    x=[(i,i=1,12)]
    print *, SUM(x)   
    s=[4,3]
    A=RESHAPE(x,s)
    print *, SUM(A) 
    print *, SUM(A,1)
    print *, SUM(A,2)

END PROGRAM



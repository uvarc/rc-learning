      program test
      integer a
      dimension a(5,5)
      integer i, j, l, m, y
      do 1 i=1,5
         do 2 j=1,5
            a(i,j)=i*(j+1)/i
  2      continue
  1   continue
      call absmax(a,5,5,l,m,y)
      print *, y
      print *, l,m
      end

      SUBROUTINE ABSMAX(A,N,M,I,K,Y)
      INTEGER N, M
      INTEGER A
      DIMENSION A(N,M)
      INTEGER Y
      INTEGER I, K
      INTEGER P, Q
C  THE ABSOLUTE GREATEST ELEMENT OF ARRAY A OF SIZE NxM IS COMPUTED AND RETURNED
C  IN Y. THE CORRESPONDING LOCATION SUBSCRIPTS ARE RETURNED IN I AND K.
      Y=0
      I=1
      K=1
      DO 200 P=1,N
          DO 100 Q=1,M
             IF ( ABS(A(P,Q)) .LE. Y ) GO TO 10
                Y=ABS(A(P,Q))
                I=P
                K=Q
   10        CONTINUE
  100     CONTINUE
  200 CONTINUE
      END


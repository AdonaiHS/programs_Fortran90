PROGRAM exer4

IMPLICIT NONE

!* É o mesmo código do exercício 5.3

REAL, DIMENSION(-100:100, -100:100) :: V, Vn, Ex, Ey
REAL :: deltaV
INTEGER :: i, j

DO i = -100, 0
   V(i,-100) = 0
   V(-100,i) = 0
   Vn(i,-100) = 0
   Vn(-100,i) = 0
END DO

deltaV = 1

DO i = -99, 0
   DO j = -99, 0
      IF((j >= -40) .AND. (i == -45)) THEN
      V(i,j) = 1
      ELSE
      V(i,j) = 0.5
      END IF
   END DO
END DO

DO WHILE (deltaV >= 0.001)
   DO i = -99, 0
      DO  j = -99, 0
         IF((j >= -40) .AND. (i == -45)) THEN
            Vn(i,j) = 1
         ELSE IF((j == 0) .AND. (i == 0)) THEN
            Vn(i,j) = 0.25*(2*V(i-1,j) + 2*V(i,j-1))
         ELSE IF((i == 0) .AND. (j .NE. 0)) THEN
            Vn(i,j) = 0.25*(V(i,j+1) + V(i,j-1))
         ELSE IF((j == 0) .AND. (i .NE. 0)) THEN
            Vn(i,j) = 0.25*(V(i-1,j) + V(i+1,j) + 2*V(i,j-1)) 
         ELSE
            Vn(i,j) = 0.25*(V(i+1,j) + V(i-1,j) + V(i,j+1) + V(i,j-1))
         END IF
      END DO
   END DO
   
   deltaV = 0

   DO i = -99, 0
      DO j = -99, 0
         deltaV = deltaV + ABS(Vn(i,j) - V(i,j))
      END DO
   END DO

   V = Vn   

END DO

! Primeira reflexão:

DO j = -100, 0
   DO i = -100, 0
      V(i,-j) = V(i,j)
   END DO
END DO 

! Segunda reflexão:

DO i = -100, 0
   DO j = -100, 100
      V(-i,j) = -V(i,j)
   END DO
END DO

! Cálculo do campo elétrico:

DO i = -99, 99
   DO j = -99, 99
      Ex(i,j) = (V(i-1,j) - V(i+1,j))*75
      Ey(i,j) = (V(i,j-1) - V(i,j+1))*75
   END DO
END DO

OPEN (UNIT = 18, FILE = 'capacitorV.out')
OPEN (UNIT = 19, FILE = 'capacitorE.out')

DO i = -100, 100
   DO j = -100, 100
      WRITE(18, *) i, j, V(i,j)
   END DO
   WRITE(18, *) " "
END DO

DO i = -90, 90, 10
   DO j = -90, 90, 10
      WRITE(19, *) i, j, Ex(i,j), Ey(i,j)
   END DO
   WRITE(19, *) " "
END DO

CLOSE (UNIT = 18)
CLOSE (UNIT = 19)
  
END PROGRAM exer4

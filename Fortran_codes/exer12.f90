PROGRAM exer12

IMPLICIT NONE

!* É exatamente o mesmo código do exercício 5.11

REAL(8), DIMENSION (0:4) :: deltaZ, B
REAL(8) :: Zmin, Zmax, X
INTEGER :: i, j

Zmin = -10
Zmax = 10
X = 1

DO i = 0, 3 
   deltaZ(i) = 0.001*10**(i)
END DO

deltaZ(4) = 2

DO i = 0, 3
   B(i) = 0
   DO j = 1, 20000/(10**(i))
      B(i) = B(i) + f(Zmin + j*deltaZ(i),X)*deltaZ(i)
   END DO
END DO

B(4) = 0
DO j = 1, 10
   B(4) = B(4) + f(Zmin + j*deltaZ(4),X)*deltaZ(4)
END DO

OPEN (UNIT = 120, FILE = 'mag.out')

DO i = 0, 4
   WRITE(120, *) deltaZ(i), B(i)
END DO

CLOSE (UNIT = 120)  

CONTAINS

FUNCTION f(z,x)

IMPLICIT NONE

REAL(8) :: f
REAL(8), INTENT(IN) :: z, x

f = x/((z*z + x*x)**(1.5))

END FUNCTION f

END PROGRAM exer12


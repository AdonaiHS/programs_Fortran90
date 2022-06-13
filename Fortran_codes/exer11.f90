PROGRAM exer11

IMPLICIT NONE

! O fio está no eixo Z e possui comprimento Zmin + Zmax

! X = distância radial até o fio
! Bsimp = Campo magnético calculado pelo m[étodo de Simpson
! Bnorm = Campo magnético calculado pelo método de discretização
! da lei de Biot-Savart (normal)
! Zmin = posição do extremo inferior do fio
! Zmax = posição do extremo superior do fio
! deltaZ = tamanho (passo) dos "pedacinhos" de fio que contribuem
! para o campo magnético total.
! Spar = Soma dos elementos com índice par no método de Simpson
! Simpar = Soma dos elementos com índice ímpar no método de Simpson
! i, j = iteradores

REAL(8), DIMENSION (1:20) :: X, BSimp, BNorm
REAL(8) :: Zmin, Zmax, deltaZ, Spar, Simpar
INTEGER :: i, j

! especificações do fio e do passo

Zmin = - 10
Zmax = 10
deltaZ = 0.01

! Posições em que será calculado o campo magnético 

DO i = 1, 20
   X(i) = 0.05 + (0.05*(i-1))
END DO

! Método de Simpson

DO i = 1, 20
  
   Simpar = 0
   DO j = 1, 1999, 2
      Simpar = Simpar + (deltaZ/3)*4*f(Zmin + j*deltaZ,X(i))
   END DO

   Spar = 0
   DO j = 2, 1998, 2
      Spar = Spar + (deltaZ/3)*2*f(Zmin + j*deltaZ,X(i))
   END DO
   
   BSimp(i) = (deltaZ/3)*(f(Zmin,X(i)) + f(Zmax,X(i))) + Simpar + Spar

END DO

! Integral Usual (normal)

DO i = 1, 20

   BNorm(i) = 0
   DO j = 0, 2000 
      BNorm(i) = BNorm(i) + f(Zmin + j*deltaZ,X(i))*deltaZ
   END DO

END DO

OPEN (UNIT = 110, FILE = 'Bsimpson.out')
OPEN (UNIT = 111, FILE = 'Bnormal.out')

DO i = 1, 20
   WRITE(110, *) i*0.05, BSimp(i)
   WRITE(111, *) i*0.05, BNorm(i)
END DO

CLOSE (UNIT = 110)
CLOSE (UNIT = 111)

CONTAINS

! Esta função é apenas a forma da expressão para o campo 
! magnético, foi usada apenas para fins de simplificação.

FUNCTION f(z,x)

IMPLICIT NONE

REAL(8) :: f
REAL(8), INTENT(IN) :: z, x

f = x/((z*z + x*x)**(1.5))

END FUNCTION f

END PROGRAM exer11

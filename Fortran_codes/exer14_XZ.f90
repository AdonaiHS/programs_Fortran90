PROGRAM exer14_XZ

IMPLICIT NONE

! O selenóide possui 80 voltas igualmente espaçadas e se 
! estende de z=-2 até z=2. Possui raio r=1 e a corrente é
! tal que mu_0*I/4*pi = 0.001.

! X = vetor posição onde se está calculando o campo B
! Xlinha = vetor posição do elemento de fio
! versor = versor que dá a direção da corrente no fio
! L = X - Xlinha
! eixoX = coordenada x do ponto em que se calcula o campo B
! eixoZ = coordenada z do ponto em que se calcula o campo B
! Bx, Bz = componentes x e z do campo magnético B
! inc = ângulo de inclinação que o fio enrolado faz com a 
! horizontal (plano XY)
! deltaX = intervalos em que se calcula o campo B
! deltaT = passo para a integração numérica na coordenada
! theta
! deltar = comprimento do elemento de fio
! absL = |L|
! mfloat = iterador "m" como número real
! i, j, k, m = iteradores

REAL, DIMENSION(1:3) :: X, Xlinha, versor, L
REAL, DIMENSION(-8:8) :: eixoX
REAL, DIMENSION(-8:8) :: eixoZ
REAL, DIMENSION(-8:8, -8:8) :: Bx, Bz
REAL, PARAMETER :: pi = 3.1416, inc = 0.007957812536
REAL :: deltaX, deltaT, deltar, absL, mfloat
INTEGER :: i, j, k, m

deltaT = 0.0001
deltaX = 0.3

! O comprimento do fio não é exatamente deltaT, deve-se
! considerar a componente z

deltar = deltaT*SQRT(1+(TAN(inc))**2)

! Definição dos pontos onde será calculado o campo B

DO i = -8, 8
   eixoX(i) = deltaX*i
END DO
DO i = -8, 8
   eixoZ(i) = deltaX*i
END DO

DO i = -8, 8
   DO j = -8, 8
      X(1) = eixoX(i)
      X(2) = 0
      X(3) = eixoZ(j)

      Bx(i,j) = 0
      Bz(i,j) = 0

      ! Dado um ponto (deltaX*i,deltaX*j) o campo magnético neste ponto
      ! é calculado aqui:

      DO m = 0, 79
      mfloat = m
      DO k = 0, 62832
         Xlinha(1) = COS(k*deltaT)
         Xlinha(2) = SIN(k*deltaT)
         Xlinha(3) = -2 + 0.05*mfloat + 0.05*((k*deltaT)/(2*pi))
                  
         versor(1) = - COS(inc)*SIN(k*deltaT)
         versor(2) = COS(inc)*COS(k*deltaT)
         versor(3) = SIN(inc)

         L = X - Xlinha
         absL = SQRT( L(1)**2 + L(2)**2 + L(3)**2 )
   
         Bx(i,j) = Bx(i,j) + 0.001*(deltar/(absL**3))*( versor(2)*L(3) - versor(3)*L(2) )
         Bz(i,j) = Bz(i,j) + 0.001*(deltar/(absL**3))*( versor(1)*L(2) - versor(2)*L(1) )
      END DO
      END DO

   END DO
END DO

OPEN (UNIT = 141, FILE = 'planoXZ.out')

DO i = -8, 8
   DO j = -8, 8
      WRITE(141, *) eixoX(i), eixoZ(j), Bx(i,j), Bz(i,j)
   END DO
   WRITE(141, *) ' '
END DO

CLOSE (UNIT = 141)

END PROGRAM exer14_XZ


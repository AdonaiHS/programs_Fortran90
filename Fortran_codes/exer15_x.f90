PROGRAM exer15_x

IMPLICIT NONE

! As bobinas estão em z = 0.5 e z= - 0.5 e possuem raio r = 1

! X = vetor posição do ponto em que se deseja calcular o campo B
! Xlinhaj = vetor posição do elemento de fio da bobina j
! Lj = X - Xlinhaj
! Tversor = versor theta do sistema de coordenadas esféricas e 
! cilíndricas
! eixoX = coordenada x do ponto em que se está calculando B
! Bxj, Byj, Bzj = Componentes x, y e z do campo B gerado pela 
! bobina j
! Btotalj = Campo B total gerado pela bobina j
! Bxtotal, Bytotal, Bztotal = Componentes x, y e z do campo B
! total
! B = Campo magnético total
! deltaX = intervalo espacial entre os pontos em que será calculado
! o campo B
! deltaT = passo para a integração numérica da coordenada theta
! absLj = |Lj|
! i, j = iteradores

REAL, DIMENSION(1:3) :: X, Xlinha1, Xlinha2, L1, L2, Tversor
REAL, DIMENSION(-20:20) :: eixoX, Bx1, By1, Bz1, Btotal1, Bx2, By2, Bz2, Btotal2, B, Bxtotal, Bytotal, Bztotal
REAL :: deltaX, deltaT, absL1, absL2
REAL, PARAMETER :: pi = 3.1416
INTEGER :: i, j

deltaT = 0.0001
deltaX = 0.1

! Definição dos pontos onde será calculado o campo B

DO i = -20, 20
   eixoX(i) = deltaX*i
END DO

DO i = -20, 20
   X(1) = eixoX(i)
   X(2) = 0
   X(3) = 0
   
   Bx1(i) = 0
   By1(i) = 0
   Bz1(i) = 0
   Bx2(i) = 0
   By2(i) = 0
   Bz2(i) = 0
   
   ! Em uma posição (x,0,0) qualquer do eixo X:
   
   DO j = 0, 62832
      Xlinha1(1) = COS(j*deltaT)  
      Xlinha1(2) = SIN(j*deltaT)
      Xlinha1(3) = - 0.5

      Xlinha2(1) = COS(j*deltaT)
      Xlinha2(2) = SIN(j*deltaT)
      Xlinha2(3) = 0.5
      
      Tversor(1) = - SIN(j*deltaT)
      Tversor(2) = COS(j*deltaT)
      Tversor(3) = 0

      L1 = X - Xlinha1
      absL1 = SQRT( L1(1)**2 + L1(2)**2 + L1(3)**2 )

      L2 = X - Xlinha2
      absL2 = SQRT( L2(1)**2 + L2(2)**2 + L2(3)**2 )

      Bx1(i) = Bx1(i) + (deltaT/(absL1**3))*( Tversor(2)*L1(3) - Tversor(3)*L1(2) )
      By1(i) = By1(i) + (deltaT/(absL1**3))*( Tversor(3)*L1(1) - Tversor(1)*L1(3) )
      Bz1(i) = Bz1(i) + (deltaT/(absL1**3))*( Tversor(1)*L1(2) - Tversor(2)*L1(1) )

      Bx2(i) = Bx2(i) + (deltaT/(absL2**3))*( Tversor(2)*L2(3) - Tversor(3)*L2(2) )
      By2(i) = By2(i) + (deltaT/(absL2**3))*( Tversor(3)*L2(1) - Tversor(1)*L2(3) )
      Bz2(i) = Bz2(i) + (deltaT/(absL2**3))*( Tversor(1)*L2(2) - Tversor(2)*L2(1) )
   END DO

   Bxtotal(i) = Bx1(i) + Bx2(i)
   Bytotal(i) = By1(i) + By2(i)
   Bztotal(i) = Bz1(i) + Bz2(i)

   Btotal1(i) = SQRT( Bx1(i)**2 + By1(i)**2 + Bz1(i)**2 )
   Btotal2(i) = SQRT( Bx2(i)**2 + By2(i)**2 + Bz2(i)**2 )
   
END DO

B = Btotal1 + Btotal2

OPEN (UNIT = 155, FILE = 'eixoX.out')
OPEN (UNIT = 156, FILE = 'direcoes.out')

DO i = -20, 20
   WRITE(155, *) eixoX(i), B(i), Btotal1(i), Btotal2(i)
   WRITE(156, *) eixoX(i), 0, 0, Bztotal(i)
END DO

CLOSE (UNIT = 155)
CLOSE (UNIT = 156)

END PROGRAM exer15_x


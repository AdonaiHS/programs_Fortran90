PROGRAM exer15_z

IMPLICIT NONE

! As bobinas estão em z = 0.5 e z= - 0.5 e possuem raio r = 1

! Z = vetor posição do ponto em que se deseja calcular o campo B
! ao longo do eixo Z
! z_Bx, z_By, z_Bz  = componentes x, y e z do campo B
! z_bobj = campo total gerado pela bobina j
! z_Btotal = Campo magnético total
! deltaZ = intervalo espacial entre os pontos em que será calculado B
! deltatheta = passo para a integração numérica da coordenada theta
! pos = posição do elemento de fio
! Sx_theta, Sy_theta, Sz_theta = soma das contribuições de cada
! deltatheta
! i, j = iteradores

REAL(8), DIMENSION(-15:15) :: Z, z_Bx, z_By, z_Bz, z_bob1, z_bob2, z_Btotal
REAL(8) :: deltaZ, deltatheta, pos, Sx_theta, Sy_theta, Sz_theta
REAL(8), PARAMETER :: Pi = 3.1416
INTEGER :: i, j

pos = 0.5
deltaZ = 0.1
deltatheta = 0.0001

! Definição dos pontos onde será calculado o campo B

DO i = -15, 15
   Z(i) = i*deltaZ
END DO

Sx_theta = 0
Sy_theta = 0
Sz_theta = 0
DO i = 0, 62832
   Sx_theta = Sx_theta + deltatheta*COS(i*deltatheta)
   Sy_theta = Sy_theta + deltatheta*SIN(i*deltatheta)
   Sz_theta = Sz_theta + deltatheta
END DO

DO i = -15, 15
   
   ! Componente X do campo no eixo Z:
   z_Bx(i) = Z(i)*fb(Z(i),pos)*Sx_theta + Z(i)*fb(Z(i),-pos)*Sx_theta
   ! Componente Y do campo no eixo Z:
   z_By(i) = Z(i)*fb(Z(i),pos)*Sy_theta + Z(i)*fb(Z(i),-pos)*Sy_theta
   ! Componente Z do campo no eixo Z:
   z_Bz(i) = fb(Z(i),pos)*Sz_theta + fb(Z(i),-pos)*Sz_theta
      
END DO

DO i = -15, 15
   z_Btotal(i) = SQRT(z_Bx(i)**2 + z_By(i)**2 + z_Bz(i)**2)
   z_bob1(i) = SQRT( (Z(i)*fb(Z(i),pos)*Sx_theta)**2 + (Z(i)*fb(Z(i),pos)*Sy_theta)**2 + (fb(Z(i),pos)*Sz_theta)**2 )
   z_bob2(i) = SQRT( (Z(i)*fb(Z(i),-pos)*Sx_theta)**2 + (Z(i)*fb(Z(i),-pos)*Sy_theta)**2 + (fb(Z(i),-pos)*Sz_theta)**2 ) 
END DO

OPEN (UNIT = 150, FILE = 'eixoZ.out')

DO i = -15, 15
   WRITE(150, *) Z(i), z_Btotal(i), z_bob1(i), z_bob2(i)
END DO

CLOSE (UNIT = 150)

CONTAINS

FUNCTION fb(z,r)

IMPLICIT NONE

REAL(8), INTENT(IN) :: z, r
REAL(8) :: fb

fb = ( 1 + (z + r)**2 )**(-1.5)

END FUNCTION fb

FUNCTION gb(x,t)

IMPLICIT NONE

REAL(8), INTENT(IN) :: x, t
REAL(8) :: gb

gb = ( SQRT(SIN(t)**2 + (x - COS(t))**2 + 0.25) )**(3)

END FUNCTION gb

END PROGRAM exer15_z


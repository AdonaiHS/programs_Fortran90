PROGRAM exer8

IMPLICIT NONE

! V = Potencial
! Vn = Potencial novo (serve como "atualização" de V)
! rho = densidade de carga elétrica na posição (i,j)
! deltaV = Checa a"suavidade" do potencial
! i, j = iteradores

REAL(8), DIMENSION(-100:100, -100:100) :: V,Vn,rho
REAL(8) :: deltaV
INTEGER :: i, j

! É necessário que deltaV se inicie com um valor maior que 0.001

deltaV = 1

! Condição de potencial V = 0 nas bordas da caixa 

DO i = -100, 100
   V(i,-100) = 0
   V(i,100) = 0
   V(-100,i) = 0
   V(100,i) = 0
   Vn(i,-100) = 0 
   Vn(i,100) = 0
   Vn(-100,i) = 0
   Vn(100,i) = 0
END DO

! Imposição de rho =/= apenas para o ponto (75,0)

DO i = -100, 100
   DO j = -100, 100
      IF(i == 75 .AND. j == 0) THEN
         rho(i,j) = 1
      ELSE
         rho(i,j) = 0
      END IF
   END DO
END DO

! "Chute inicial" para o potencial elétrico

DO i = -99, 99
   DO j = -99, 99
      V(i,j) = 0.5
   END DO
END DO

! Aqui ocorre o processo iterativo para a suavização do potencial

DO WHILE (deltaV >= 0.001)
   DO i = -99, 99
      DO j = -99, 99
         Vn(i,j) = 0.25*(V(i-1,j) + V(i+1,j) + V(i,j-1) + V(i,j+1)) + 0.25*rho(i,j)
      END DO
   END DO

   deltaV = 0

   DO i = -99, 99
      DO j = -99, 99
         deltaV = deltaV + ABS(Vn(i,j) - V(i,j))
      END DO
   END DO

   V = Vn

END DO

OPEN (UNIT = 80, FILE = 'chargeV.out')

DO i = -100, 100
   DO j = -100, 100
      WRITE(80, *) i, j, V(i,j)
   END DO
   WRITE(80, *) " "
END DO

CLOSE (UNIT = 80)

END PROGRAM exer8

PROGRAM exer6

IMPLICIT NONE

! O para-raios é uma barra metálica com espessura 10 e que vai
! de -100 até 80 de comprimento, e está a um potencial elétrico
! constante V = 0 (aterrado).

! V = Potencial
! Vn = Potencial novo (serve como "atualização" de V)
! Ex, Ey = Componentes X e Y do campo elétrico, respectivamente
! deltaV = Checa a"suavidade" do potencial
! i, j = iteradores

REAL, DIMENSION(-100:100, -100:100) :: V,Vn,Ex,Ey
REAL :: deltaV
INTEGER  i,j

! Condição de potencial V = 1 na parte superior e V = 0 nas demias
! bordas:

DO i = -100,100
   V(i,-100) = 0
   V(-100,i) = 0
   V(100,i) = 0
   V(i,100) = 1
   Vn(i,-100) = 0
   Vn(-100,i) = 0
   Vn(100,i) = 0
   Vn(i,100) = 1
END DO

! É necessário que deltaV se inicie com um valor maior que 0.001

deltaV = 1

DO i = -99, 99
   DO j = -99, 99
      IF((j >= -99) .AND. (j <= 80) .AND. (i >= -5) .AND. (i <= 5)) THEN
      V(i,j) = 0
      ELSE
      V(i,j) = 0.5
      END IF
   END DO
END DO

! Aqui ocorre o processo de suavização do potencial:

DO WHILE (deltaV >= 0.001)
   DO i = -99, 99
      DO j = -99, 99
         IF((j >= -99) .AND. (j <= 80) .AND. (i >= -5) .AND. (i <= 5)) THEN
         Vn(i,j) = 0
         ELSE
         Vn(i,j) = 0.25*(V(i+1,j) + V(i-1,j) + V(i,j+1) + V(i,j-1))
         END IF
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

! Cálculo do campo elétrico

DO i = -99, 99
   DO j = -99, 99
      Ex(i,j) = (V(i-1,j) - V(i+1,j))*75
      Ey(i,j) = (V(i,j-1) - V(i,j+1))*75
   END DO
END DO

OPEN (UNIT = 50, FILE = 'pararaioV.out')
OPEN (UNIT = 51, FILE = 'pararaioE.out')

DO i = -100, 100
   DO j = -100, 100
      WRITE(50,*) i, j, V(i,j)
   END DO
   WRITE(50,*) " "
END DO

DO i = -90, 90, 10
   DO j = 0, 90, 10
      WRITE(51,*) i, j, Ex(i,j), Ey(i,j)
   END DO
   WRITE(51,*) " "
END DO

CLOSE (UNIT = 50)
CLOSE (UNIT = 51)

END PROGRAM exer6


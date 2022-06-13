PROGRAM exer3

IMPLICIT NONE

! As placa possuem 80 de comprimento, e potencial V = 1 e V = -1,
! e estão separadas por uma distânica de 50 unidades.

! V = Potencial
! Vn = Potencial novo (serve como "atualização" de V)
! Ex, Ey = Componentes X e Y do campo elétrico, respectivamente
! deltaV = Checa a"suavidade" do potencial
! i, j = iteradores

REAL, DIMENSION(-100:100, -100:100) :: V, Vn, Ex, Ey
REAL :: deltaV
INTEGER :: i, j

! Condição de potencial V = 0 nas bordas

DO i = -100, 0
   V(i,-100) = 0
   V(-100,i) = 0
   Vn(i,-100) = 0
   Vn(-100,i) = 0
END DO

! É necessário que deltaV se inicie com um valor maior que 0.001 

deltaV = 1

! Condição de potencial V = 1 e V = -1 nas placas, além do 
! "chute" inicial

DO i = -99, 0
   DO j = -99, 0
      IF((j >= -40) .AND. (i == -25)) THEN
      V(i,j) = 1
      ELSE
      V(i,j) = 0.5
      END IF
   END DO
END DO

! Aqui ocorre o processo iterativo de suavização do potencial:

DO WHILE (deltaV >= 0.001)
   DO i = -99, 0
      DO  j = -99, 0
         IF((j >= -40) .AND. (i == -25)) THEN
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
      Ex(i,j) = (V(i-1,j) - V(i+1,j))*100
      Ey(i,j) = (V(i,j-1) - V(i,j+1))*100
   END DO
END DO

OPEN (UNIT = 13, FILE = 'capacitorV.out')
OPEN (UNIT = 23, FILE = 'capacitorE.out')

DO i = -100, 100
   DO j = -100, 100
      WRITE(13, *) i, j, V(i,j)
   END DO
   WRITE(13, *) " "
END DO

DO i = -90, 90, 10
   DO j = -90, 90, 10
      WRITE(23, *) i, j, Ex(i,j), Ey(i,j)
   END DO
   WRITE(23, *) " "
END DO

CLOSE (UNIT = 13)
CLOSE (UNIT = 23)
  
END PROGRAM exer3      





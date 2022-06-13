PROGRAM exer1

IMPLICIT NONE

! A placa possui dimensões 200x200, possui potencial V = 0 nas bordas
! e V = 1 na região central 80x80.

! V = Potencial
! Vn = Potencial novo (serve como "atualização" de V)
! Ex, Ey = Componentes X e Y do campo elétrico, respectivamente
! deltaV = Checa a"suavidade" do potencial na placa
! i, j = iteradores

REAL(8),DIMENSION(-100:100, -100:100) :: V, Vn, Ex, Ey
REAL(8) :: deltaV
INTEGER :: i, j

! É necessário que deltaV inicie como um valor maior que 0.001:

deltaV = 1

! Condição do potencial ser sempre 0 nas bordas da placa:

DO i = -100,100
   V(i,-100) = 0
   V(i,100) = 0
   V(-100,i) = 0
   V(100,i) = 0
   Vn(i,-100) = 0
   Vn(i,100) = 0
   Vn(-100,i) = 0
   Vn(100,i) = 0
END DO

! Dá a condição do potencial ser 1 no quadrado 40x40 no cento da placa
! assim como o chute inicial para V:

DO i = -99, 99
   DO j = -99, 99
      IF(i >= -40 .AND. i <= 40 .AND. j >= -40 .AND. j <= 40) THEN
         V(i,j) = 1
      ELSE
         V(i,j) = 0.5
      END IF
   END DO
END DO

! Aqui ocorre o processo iterativo até encontrar uma função "suave" para
! o potencial V:

DO WHILE (deltaV >= 0.001)
   DO i = -99, 99
      DO j = -99, 99
         IF(i >= -40 .AND. i <= 40 .AND. j >= -40 .AND. j<= 40) THEN
            Vn(i,j) = 1
         ELSE
            Vn(i,j) = 0.25*(V(i-1,j) + V(i+1,j) + V(i,j-1) + V(i,j+1))
         END IF
      END DO
   END DO

   ! Antes de calcular deltaV é necessário igualá-lo a 0:

   deltaV = 0

   DO i = -99, 99
      DO j = -99, 99
         deltaV = deltaV + ABS(Vn(i,j) - V(i,j))
      END DO
   END DO

   ! Atualização do valor de V:

   V = Vn

END DO

! Com o potencial calculado é possível calcular o campo elétrico:

DO i = -99, 99
   DO j = -99, 99
      Ex(i,j) = (V(i-1,j) - V(i+1,j))*100
      Ey(i,j) = (V(i,j-1) - V(i,j+1))*100
   END DO
END DO

! Escreve os resultados em uma tabela nos arquivos prismV.out (potencial)
! e prismE.out (campo elétrico) que é então utilizado para se construir
! os gráficos:

OPEN (UNIT = 10, FILE = 'prismV.out')
OPEN (UNIT = 20, FILE = 'prismE.out')

DO i = -100, 100
   DO j = -100, 100
      WRITE(10, *) i, j, V(i,j)
   END DO
   WRITE(10, *) " "
END DO

DO i = -90, 90, 10
   DO j = -90, 90, 10
      WRITE(20, *) i, j, Ex(i,j), Ey(i,j)
   END DO
   WRITE(20, *) " "
END DO

CLOSE (UNIT = 10)
CLOSE (UNIT = 20)

END PROGRAM exer1


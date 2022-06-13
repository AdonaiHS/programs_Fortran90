PROGRAM exer9

IMPLICIT NONE

! A carga é uma esfera de raio r =5 com densidade de carga 
! uniforme e está no centro de uma caixa metálica esférica
! de raio r = 100

! U = Potencial multoplicado pela coordenada raial r
! Un = Potencial novo (serve como "atualização" de U)
! V = Potencial
! rho = densidade de carga no ponto (i,j)
! deltaV = Checa a"suavidade" do potencial
! i, j = iteradores

REAL(8), DIMENSION(0:100) :: U, Un, V, rho
REAL(8) :: deltaV
INTEGER :: r

! Imposição de potencial nulo nas bordas da caixa

U(100) = 0

! É necessário que deltaV inicie com um valor maior que 0.001

deltaV = 1

! Construção da função densidade de carga

DO r = 0, 100
   IF(r >= 0 .AND. r <= 5) THEN
      rho(r) = 1
   ELSE
      rho(r) = 0
   END IF
END DO

! "Chute inicial" para o potencial

DO r = 5, 99
   U(r) = 0.5
END DO

! Aqui ocorre o processo iterativo para a suavização do potencial

DO WHILE(deltaV >= 0.001)
   DO r = 5, 99
      IF(r == 5) THEN
         Un(r) = U(r+1) + 0.5*r*rho(r)
      ELSE
         Un(r) = 0.5*(U(r+1) + U(r-1)) + 0.5*r*rho(r)
      END IF
   END DO

   deltaV = 0

   DO r = 5, 99
      deltaV = deltaV + ABS(Un(r) - U(r))
   END DO

   U = Un

END DO

! Obtenção da expressão do potencial V = U/r

DO r = 5, 100
   V(r) = U(r)/r
END DO

OPEN (UNIT = 90, FILE = 'radialV.out')

DO r = 5, 100
   WRITE(90, *) r, V(r)
END DO

CLOSE (UNIT = 90)

END PROGRAM exer9


! 2020 - 19 - 5
! Dev_phi1.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE DE N CUERPOS
! Calcula la fuerza entre dos particulas

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: 
! gfortran -Wall -pedantic -std=f95 -c Dev_phi2.f90

FUNCTION Dw2(m1,m2,l1,l2,th1,th2,w1,w2)
  IMPLICIT NONE
  
  ! Definimos variables de entrada
  REAL(8), INTENT(IN) :: m1,m2,l1,l2,th1,th2,w1,w2
  
  ! Variables internas:
  REAL(8) :: Dw2, g
  g = 981
  
  Dw2 = (2*SIN(th1-th2)*(w1*w1*l1*(m1+m2)+g*(m1+m2)*COS(th1)+w2*w2*l2*m2*COS(th1-th2)))/(l2*(2*m1+m2-m2*COS(2*th1-2*th2)))

END FUNCTION Dw2

 
! 2020 - 19 - 5
! derivada_pool.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE DE PENDULO DOBLE
! MODULO DE INTERFASE DE FUNCIÓNES

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008

! REQUIERE:
! Dev_phi1.f90, Dev_phi2.f90

! Instrucciónes de compilación:
! gfortran -Wall -pedantic -std=f95 -c Dev_phi1.f90
! gfortran -Wall -pedantic -std=f95 -c Dev_phi2.f90
! gfortran -Wall -pedantic -std=f95 -c derivadas_pool.f90

MODULE derivadas_pool

 ! Derivada de phi 1
  INTERFACE
    FUNCTION Dw1(m1,m2,l1,l2,th1,th2,w1,w2)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: m1,m2,l1,l2,th1,th2,w1,w2
      REAL(8) :: Dw1
    END FUNCTION Dw1
  END INTERFACE
  
   ! Derivada de phi 1
  INTERFACE
    FUNCTION Dw2(m1,m2,l1,l2,th1,th2,w1,w2)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: m1,m2,l1,l2,th1,th2,w1,w2
      REAL(8) :: Dw2
    END FUNCTION Dw2
  END INTERFACE

END MODULE derivadas_pool

! 2020 - 19 - 5
! penduloD2.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! SIMULACIÓN DEL PENDULO DOBLE

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008

! Rquiere: derivadas_pool.f90, Dev_.f90

! gfortran -Wall -pedantic -std=f95 -c Dev_phi1.f90
! gfortran -Wall -pedantic -std=f95 -c Dev_phi2.f90
! gfortran -Wall -pedantic -std=f95 -c derivadas_pool.f90
! gfortran -Wall -pedantic -std=f95 -c penduloD2.f90
! gfortran -Wall -pedantic -std=f95 -o penduloD penduloD2.o derivadas_pool.o Dev_phi1.o Dev_phi2.o
! ./penduloD
PROGRAM pendulo_D
  USE derivadas_pool
  IMPLICIT NONE
  
  ! Definimos variables del problema
  REAL (8), ALLOCATABLE :: Pendulo(:,:)
  INTEGER (8):: pasos, datos
  REAL (8):: Dt, m1, m2, l1, l2, th1, th2, w1, w2
  
  ! Variables para rk4
  REAL(8) :: a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,f1,f2,f3,f4
  REAL(8) :: x1, x2, y1, y2, t
  ! Definimos variables auxiliares
  INTEGER(8):: i
  
  ! Variables de control
  INTEGER(4) :: err
  
  ! Abrimos el archivo de configuración (12)
  OPEN (12, FILE='rk4.config', STATUS='old', IOSTAT=err)
  IF (err .ne. 0) STOP 'rk4.config is missing'
  
  READ(12,*)
  READ(12,*) pasos
  READ(12,*) Dt
  READ(12,*) datos
  CLOSE(12)
  
  ! Leemos las condiciones iniciales  
  OPEN (14, FILE='condiciones_iniciales.config', STATUS='old', IOSTAT=err)
  IF (err .ne. 0) STOP 'condiciones.config is missing'
  
  READ(14,*)
  READ(14,*) m1  ! masa 1
  READ(14,*) m2  ! masa 2
  READ(14,*) l1  !longitud 1
  READ(14,*) l2  !longitud 2
  READ(14,*) th1 ! angulo 1
  READ(14,*) th2 ! angulo 2
  READ(14,*) w1  ! velocidad angular 1
  READ(14,*) w2  ! velocidad angular 2
  CLOSE(14)

  ! Preparamos las variables para hacer el calculo
  
  ALLOCATE(Pendulo(4,pasos+2))
  Pendulo = 0
  
  Pendulo(1,1) = th1
  Pendulo(2,1) = th2
  Pendulo(3,1) = w1
  Pendulo(4,1) = w2

  ! Abrimos el acrchivo donde escribiremos los resultados
  OPEN (16, FILE='Posiciones_rectangulares.dat', STATUS='new',IOSTAT=err)
  IF (err .ne. 0) STOP 'Posiciones_rectangulares.dat exists already'
  WRITE (16,*) '# Posisiones para el pendulo doble'
  OPEN (18, FILE='Posisiones_polares.dat', STATUS='new', IOSTAT=err)
  IF (err .ne. 0) STOP 'Posisiones_polares.dat exists already'
  WRITE (18,*) '#coordenadas polares para el pendulo doble'
  ! Escribimos la pocision inicial 

  PRINT *, '***********************************************************************'
  PRINT *, '  Simulando el movimiento del pendulo doble'
  PRINT *, '  Delta t= ', Dt
  PRINT *, '  Realizando', pasos, 'iteraciones'
  PRINT *, '  Procesando...'
  
  
  ! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  ! Esta parte hace rk4
  t = 0
  DO i=1, pasos
    t = t + Dt
    
    a1 = Dt*Pendulo(3,i)
    b1 = Dt*Pendulo(4,i)
    c1 = Dw1(m1,m2,l1,l2,Pendulo(1,i),Pendulo(2,i),Pendulo(3,i),Pendulo(4,i))
    d1 = Dw2(m1,m2,l1,l2,Pendulo(1,i),Pendulo(2,i),Pendulo(3,i),Pendulo(4,i))
    e1 = Dt*c1
    f1 = Dt*d1
  
    a2 = Dt*(Pendulo(3,i)+0.5*c1)
    b2 = Dt*(Pendulo(4,i)+0.5*d1)
    c2 = Dw1(m1,m2,l1,l2,Pendulo(1,i)+(0.5*a1),Pendulo(2,i)+(0.5*b1),Pendulo(3,i)+(0.5*e1),Pendulo(4,i)+(0.5*f1))
    d2 = Dw2(m1,m2,l1,l2,Pendulo(1,i)+(0.5*a1),Pendulo(2,i)+(0.5*b1),Pendulo(3,i)+(0.5*e1),Pendulo(4,i)+(0.5*f1))
    e2 = Dt*c2
    f2 = Dt*d2
  
    a3 = Dt*(Pendulo(3,i)+0.5*c2)
    b3 = Dt*(Pendulo(4,i)+0.5*d2)
    c3 = Dw1(m1,m2,l1,l2,Pendulo(1,i)+(0.5*a2),Pendulo(2,i)+(0.5*b2),Pendulo(3,i)+(0.5*e2),Pendulo(4,i)+(0.5*f2))
    d3 = Dw2(m1,m2,l1,l2,Pendulo(1,i)+(0.5*a2),Pendulo(2,i)+(0.5*b2),Pendulo(3,i)+(0.5*e2),Pendulo(4,i)+(0.5*f2))
    e3 = Dt*c3
    f3 = Dt*d3
    
    a4 = Dt*(Pendulo(3,i)+c3)
    b4 = Dt*(Pendulo(4,i)+d3)
    c4 = Dw1(m1,m2,l1,l2,Pendulo(1,i)+(a3),Pendulo(2,i)+(b3),Pendulo(3,i)+(e3),Pendulo(4,i)+f3)
    d4 = Dw2(m1,m2,l1,l2,Pendulo(1,i)+(a3),Pendulo(2,i)+(b3),Pendulo(3,i)+(e3),Pendulo(4,i)+f3)
    e4 = Dt*c4
    f4 = Dt*d4
    
    ! Resultados
    Pendulo(1,i+1) = Pendulo(1,i)+ (0.1666666666)*(a1+2*a2+2*a3+a4)
    Pendulo(2,i+1) = Pendulo(2,i)+ (0.1666666666)*(b1+2*b2+2*b3+b4)
    Pendulo(3,i+1) = Pendulo(3,i)+ (0.1666666666)*(e1+2*e2+2*e3+e4)
    Pendulo(4,i+1) = Pendulo(4,i)+ (0.1666666666)*(f1+2*f2+2*f3+f4)
    
    IF (MOD(i,(pasos/datos))==0) THEN
    
      x1 = l1*SIN(Pendulo(1,i+1))
      y1 = -l1*COS(Pendulo(1,i+1))
      x2 = x1+l2*SIN(Pendulo(2,i+1))
      y2 = y1-l2*COS(Pendulo(2,i+1))
      
      WRITE (16,*) t,x1,y1,x2,y2
      WRITE (18,*) t,Pendulo(:,i)
         
    END IF
    
    IF (MOD(i,(pasos/20))==0) THEN
      WRITE(*, fmt='(1x,a,i0)', advance='no') '%'
    END IF
  END DO
  DEALLOCATE(Pendulo) 
  CLOSE(16)
  CLOSE(18)
  PRINT *, 'Fin'
  PRINT *, '***********************************************************************'
END PROGRAM pendulo_D

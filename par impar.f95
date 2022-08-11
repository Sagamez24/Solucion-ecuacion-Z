  program par_impar
  implicit none
  
  integer:: i,j,s,p,n,mL, mk, L(10), k(10), u_mas(30), u_menos(30),v_mas(30), v_menos(30)
   double precision::suma,suma1, suma_z3, suma_z
   integer, parameter::    iFilas = 8
   integer, parameter:: jColumnas = 5
!  real:: k(30),L(30),v_mas(30),v_menos(30)
!   double precision,dimension (iFilas)::z_uno
   double precision,dimension (iFilas)::z
   double precision,dimension (iFilas):: prod_uno
   double precision,dimension (iFilas):: prod_dos
   double precision, dimension(iFilas, jColumnas):: Tabla_dos
   double precision, dimension (30, 2):: Tabla_vectores
  
!=====================================================================================================================   

  write(*,*) 'escribe la dimension de n'
  read(*,*)n

   if (MOD(n, 2).eq. 0) then
     write(*,*) 'el valor de n es par'
     mL = (n-2)/2
     mk = (n-2)/2
     write(*,*) 'mL y mk tendran los valores de',mL, mk
   else
     write(*,*) 'el valor de n es impar'
     mL = (n-3)/2
     mk = mL+1
     write(*,*) 'm tendra un valor de',mL, mk
   end if

   pause
!   stop
  
   do i = 1, mL
     write(*,*) 'dijite los valores que tendra L'
     read(*,*) L(i)
   end do 

   do i = 1, mL
     write(*, 2) (L(i))
   2 format (I4, f3.1)
   end do

   do i = 1, mk
     write(*,*) 'dijite los valores que tendra k'
     read(*,*) k(i)
   end do
   
   do i = 1, mk
     write(*, 3) (k(i))
   3 format (I4, f3.1)
   end do
   
!========================================================================================

  Write(*,*) 'dados los valores de L y k elejidos construya los vectores + y -'
  
  if (MOD(n, 2).eq. 0) then
    do i = 1, n  
      write(*,*) 'organice v_mas = (L(1),k(1)..k(mk),-L(1),-k(1),..-k(mk))'  
      read(*,*) v_mas(i)
    end do 
   
   do i = 1, n
     Tabla_vectores(i,1) = v_mas(i)
   end do 
    
    do i = 1, n  
      write(*,*) 'organice v_menos = (0,0,L(1)..L(mL),-L(1)..-L(mL))'  
      read(*,*) v_menos(i)
  end do

  do i = 1, n
     Tabla_vectores(i,2) = v_menos(i)
  end do 

  write(*,10) ((Tabla_vectores(i,j), j = 1, 2), i = 1, n)
   10 format(1x, 2f8.4)

   write(*,*) 'Esta nueva tabla tiene los valos de los vetores v mas y v menos al cuadrado, biseversa y el valor de z'

  do s = 1, n
    prod_uno(s) = v_mas(s)*(v_menos(s))**2
    Tabla_dos(s, 1) =  prod_uno(s)
  end do

  suma = 0  
  do s = 1, n
    suma = suma + prod_uno(s)
!    Tabla_dos(7, 1) =  suma
  end do

  do s = 1, n
    prod_dos(s) = ((v_mas(s))**2)*v_menos(s)
    Tabla_dos(s, 2) =  prod_dos(s)
  end do
    
  suma1 = 0  
  do s = 1, n
    suma1 = suma1 + prod_dos(s)
!    Tabla_dos(7, 2) =  suma1
  end do

! justo aqui incia el desarrollo para z
  
  do s = 1, n
    z(s) = (suma*v_mas(s))-(suma1*v_menos(s))
    Tabla_dos(s, 3) = z(s)
  end do  
 
  suma_z3 = 0
  do s = 1, n
    suma_z3 = suma_z3 + (z(s))**3  
!  write(*,*) suma_z3  
  Tabla_dos(s, 4) = suma_z3
  end do 

  suma_z = 0
  do s = 1, n
    suma_z = suma_z + z(s)
  Tabla_dos(s, 5) = suma_z
!  write(*,*) suma_z 
 end do 
  
  Write(*, 7 ) ( ( Tabla_dos(s,p), p = 1, 5 ), s = 1, n )
  7  Format(5x, 5f16.3)
  
   
  else
    do i = 1, n  
      write(*,*) 'organice u_mas = (0,k(1)..k(mk),-k(1),..-k(mk))'  
      read(*,*) u_mas(i)
    end do 

    do i = 1, n
     Tabla_vectores(i,1) = u_mas(i)
    end do
   
    do i = 1, n  
      write(*,*) 'organice u_menos = (L(1)..L(mL),k(1),0,-L(1)..-L(mL),k(1))'  
      read(*,*) u_menos(i)   
    end do

    do i = 1, n
     Tabla_vectores(i,2) = u_menos(i)
    end do
   
   write(*,11) ((Tabla_vectores(i,j), j = 1, 2), i = 1, n)
   11 format(1x, 2f8.4) 
   

   write(*,*) 'Esta nueva tabla tiene los valos de los vetores u mas y u menos al cuadrado, biseversa y el valor de z'

  do s = 1, n
    prod_uno(s) = u_mas(s)*(u_menos(s))**2
    Tabla_dos(s, 1) =  prod_uno(s)
  end do

  suma = 0  
  do s = 1, n
    suma = suma + prod_uno(s)
!    Tabla_dos(7, 1) =  suma
  end do

  do s = 1, n
    prod_dos(s) = ((u_mas(s))**2)*u_menos(s)
    Tabla_dos(s, 2) =  prod_dos(s)
  end do
    
  suma1 = 0  
  do s = 1, n
    suma1 = suma1 + prod_dos(s)
!    Tabla_dos(7, 2) =  suma1
  end do

! justo aqui incia el desarrollo para z
  
  do s = 1, n
    z(s) = (suma*u_mas(s))-(suma1*u_menos(s))
    Tabla_dos(s, 3) = z(s)
  end do  
 
  suma_z3 = 0
  do s = 1, n
    suma_z3 = suma_z3 + (z(s))**3  
!  write(*,*) suma_z3  
  Tabla_dos(s, 4) = suma_z3
  end do 

  suma_z = 0
  do s = 1, n
    suma_z = suma_z + z(s)
  Tabla_dos(s, 5) = suma_z
!  write(*,*) suma_z 
 end do 
  
  Write(*, 8 ) ( ( Tabla_dos(s,p), p = 1, 5 ), s = 1, n )
  8  Format(5x, 5f16.3)
 end if
!========================================================================================== 

  if (suma_z3 /= 0 .AND. suma_z /= 0  ) then
    write(*,*) 'El conjunto no es una solucion quiral '
  else
    write(*,*) 'el conjunto es una solucion quiral (columna 3)'
  end if 
  
   end program par_impar
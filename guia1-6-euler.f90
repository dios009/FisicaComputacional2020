! Sistema lineal de ecuaciones diferenciales ordinarias acopladas , oscilador armonico
! x'=y , y'=-x

program problema6
    implicit none
    integer, parameter      :: pr=selected_real_kind(13) !precision doble
    
    real(pr)                :: t,x,y,h !variables generales
    real(pr)                :: m1, m2, m3, m4, l1, l2, l3, l4 !variables de los metodos
    
    integer(pr)                 :: n,i, k
    
    !Abro los archivos a donde voy a mandar los resultados
        open(37,file='euler.dat',status='replace')
    
    
    !defino el numero de particiones y el tamanio del step
    do k=1,3
        n=10**k
        h=10._pr/real(n,pr) !t_final es 10
        print*,'Numero de particiones', n
        print*
        print*,'tamanio de step', h
        print*
    
    !***************************
    !Euler
    !***************************
    
    !Reseteo valores iniciales
        t=0._pr
        x=1._pr
        y=1._pr
        i=0
        write(37,'(I5,2x,3(E15.6,2x))') t,x,y,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
            do i=1,n
                m1=h*g(y)
                l1=h*f(x)
            
                t=t+h
                x=x+m1
                y=y+l1
                print*, t,x,y
                write(37,'(I5,2x,3(E15.6,2x))') t,x,y,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
            end do
    !    print*,'EULER:', t,x,y
!        print*,'ErrEuler:', abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
!        write(37,'(I5,2x,3(E15.6,2x))') k,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
    
    enddo
    
    !cierro los archivos de salida
        close(37)

    
    contains
    
    !Aca va la definicion de las funciones
    
    
        real(pr) function f(x)
            real(pr)        :: x
            f=-x
        end function
    
        real(pr) function g(x)
            real(pr)        :: x
            g=x
        end function
    
    end program problema6
    

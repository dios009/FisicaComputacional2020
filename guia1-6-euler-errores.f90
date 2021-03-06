! Sistema lineal de ecuaciones diferenciales ordinarias acopladas , oscilador armonico
! x'=y , y'=-x
!Calculo solamente los errores ABSOLUTOS

program problema6
    implicit none
    integer, parameter      :: pr=selected_real_kind(6) !precision single
    
    real(pr)                :: t,x,y,h !variables generales
    real(pr)                :: m1, m2, m3, m4, l1, l2, l3, l4 !variables de los metodos
    
    integer(pr)                 :: n,i, k
    
    !Abro los archivos a donde voy a mandar los resultados
        open(37,file='euler-errores-single.dat',status='replace')
    
    
    !defino el numero de particiones y el tamanio del step
    do k=2,5
        n=10**k
        h=10._pr/real(n,pr) !t_final es 10
        print*,'Numero de particiones', n
        print*
        print*,'tamanio de step', h
        print*
    
    !***************************
    !EULER
    !***************************
    
    !Reseteo valores iniciales
        t=0._pr
        x=1._pr
        y=1._pr
        i=0
        do i=1,n
            m1=h*g(y)
            l1=h*f(x)
        
            t=t+h
            x=x+m1
            y=y+l1
                print*, t,x,y
                write(37,*) t,abs((sin(t)+cos(t)-x)),abs((cos(t)-sin(t)-y))
            end do
    
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
    

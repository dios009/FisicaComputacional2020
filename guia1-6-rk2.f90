! Sistema lineal de ecuaciones diferenciales ordinarias acopladas , oscilador armonico
! x'=y , y'=-x

program problema6
    implicit none
    integer, parameter      :: pr=selected_real_kind(13) !precision doble
    
    real(pr)                :: t,x,y,h !variables generales
    real(pr)                :: m1, m2, m3, m4, l1, l2, l3, l4 !variables de los metodos
    
    integer(pr)                 :: n,i, k
    
    !Abro los archivos a donde voy a mandar los resultados
        open(37,file='rk2.dat',status='replace')
    
    
    !defino el numero de particiones y el tamanio del step
    do k=2,5
        n=10**k
        h=10._pr/real(n,pr) !t_final es 10
        print*,'Numero de particiones', n
        print*
        print*,'tamanio de step', h
        print*
    
    !***************************
    !RK2
    !***************************
    
    !Reseteo valores iniciales
        t=0._pr
        x=1._pr
        y=1._pr
        i=0
        write(37,*) t,x,y,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
        do i=1,n
            m1=h*g(y)
            m2=h*g(y+0.5_pr*m1)
            l1=h*f(x)
            l2=h*f(x+0.5_pr*m1)
        
            t=t+h
            x=x+(0.5_pr*m1+0.5_pr*m2)
            y=y+(0.5_pr*l1+0.5_pr*l2)
                print*, t,x,y
                write(37,*) t,x,y,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
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
    
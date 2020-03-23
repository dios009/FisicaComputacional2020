! Sistema lineal de ecuaciones diferenciales ordinarias acopladas , oscilador armonico
! x'=y , y'=-x

program problema6
implicit none
integer, parameter      :: pr=selected_real_kind(13) !precision doble

real(pr)                :: t,x,y,h !variables generales
real(pr)                :: m1, m2, m3, m4, l1, l2, l3, l4 !variables de los metodos

integer(pr)                 :: n,i, k

!Abro los archivos a donde voy a mandar los resultados
    open(37,file='err_euler.dat',status='replace')
    open(38,file='err_rk2.dat',status='replace')
    open(39,file='err_rk4.dat',status='replace')


!defino el numero de particiones y el tamanio del step
do k=1,10
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

        do i=1,n
            m1=h*g(y)
            l1=h*f(x)
        
            t=t+h
            x=x+m1
            y=y+l1
        end do
!    print*,'EULER:', t,x,y
    print*,'ErrEuler:', abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
    write(37,'(I5,2x,3(E15.6,2x))') k,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))

!***************************
!Runge-Kutta 2
!***************************

!Reseteo valores iniciales
    t=0._pr
    x=1._pr
    y=1._pr

        do i=1,n
            m1=h*g(y)
            m2=h*g(y+0.5_pr*m1)
            l1=h*f(x)
            l2=h*f(x+0.5_pr*m1)
        
            t=t+h
            x=x+(0.5_pr*m1+0.5_pr*m2)
            y=y+(0.5_pr*l1+0.5_pr*l2)
!        print*,'RK2  :', i,t,x,y
        end do
!    print*,'RK2  :', t,x,y
    print*,'ErrRK2   :', abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
    write(38,'(I5,2x,3(E15.6,2x))') k,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))


!***************************
!Runge-Kutta 4
!***************************

!Reseteo valores iniciales
    t=0._pr
    x=1._pr
    y=1._pr

        do i=1,n
            m1=h*g(y)
            m2=h*g(y+0.5_pr*m1)
            m3=h*g(y+0.5_pr*m2)
            m4=h*g(y+m3)
            l1=h*f(x)
            l2=h*f(x+0.5_pr*m1)
            l3=h*f(x+0.5_pr*m2)
            l4=h*f(x+m3)

            t=t+h
            x=x+(m1+2._pr*m2+2._pr*m3+m4)*(1._pr/6._pr)
            y=y+(l1+2._pr*l2+2._pr*l3+l4)*(1._pr/6._pr)
!        print*,'RK4  :', i,t,x,y
        end do
!    print*,'RK4  :', t,x,y
!    print*,'posta:', t,sin(t)+cos(t),cos(t)-sin(t)
    print*,'ErrRK4   :', abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))
    write(39,'(I5,2x,3(E15.6,2x))') k,abs((sin(t)+cos(t)-x)/(sin(t)+cos(t))),abs((cos(t)-sin(t)-y)/(cos(t)-sin(t)))


enddo

!cierro los archivos de salida
    close(37)
    close(38)
    close(39)


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

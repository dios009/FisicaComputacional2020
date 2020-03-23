program problema4
    implicit none
!use precision, only:  pr=>dp
    integer, parameter      :: pr=selected_real_kind(15)
    integer(pr)                 :: i
    real(pr)                :: exacto, traperror, simperror

    exacto = 1._pr - exp(-1._pr)   !  valor exacto de la integral

    print*, 'Valor exacto =', exacto
    print*,''

    open(37,file='err_trap.dat',status='replace')
    open(38,file='err_simp.dat',status='replace')


    do i  =2,28
        call f(2**i,traperror,simperror)
        write(37,'(I5,2x,3(E15.6,2x))') i,traperror
        write(38,'(I5,2x,3(E15.6,2x))') i,simperror
    end do

    close(37)
    close(38)

contains

    SUBROUTINE f(N,terror,serror)
        IMPLICIT NONE
     
        INTEGER(pr)                 :: N, i
        real(pr), dimension(N+1) :: eval2, trap, simpson
        real(pr)                    :: h2
        real(pr),INTENT(OUT)        :: terror,serror
             
        h2=1._pr/real(N,pr)

        print *,'Numero de regiones',N

        DO i = 1, N+1
           eval2(i) = exp(-real(i-1.,pr)*h2)
        END DO

        trap(1)=0.5_pr
        trap(N+1)=0.5_pr
        DO i = 2, N-1
            trap(i) = 1._pr
        END DO

        simpson(1)=1._pr/3._pr
        simpson(N+1)=1._pr/3._pr
        DO i = 2, N,2
            simpson(i) = 4._pr/3._pr
        END DO
        DO i = 3, N-1,2
            simpson(i) = 2._pr/3._pr
        END DO

        terror=abs((exacto-h2*dot_product(trap, eval2))/exacto)
        serror=abs((exacto-h2*dot_product(simpson, eval2))/exacto)

        print*,'Trapezoidal', h2*dot_product(trap, eval2) , 'ErrorAbs =', terror , abs(exacto-h2*dot_product(trap, eval2))
        print*,'Simpson', h2*dot_product(simpson, eval2), 'ErrorAbs =', serror , abs(exacto-h2*dot_product(simpson, eval2))
        print*, ''

        END SUBROUTINE
     

end program problema4
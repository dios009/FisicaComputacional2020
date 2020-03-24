program eps
implicit none
integer, parameter :: pr=selected_real_kind(13)
real(pr)        ::  ep, one
integer         :: i

ep=1._pr
do i=1,150
    ep=ep/2._pr
    one=1._pr + ep
    if (one==1._pr) then
        write(*,*) 'el epsilon de la maquina es:',2_pr*epsilon
        exit
    endif
    
enddo

write(*,*) 'el epsilon de la maquina es:', epsilon(ep)

end program eps
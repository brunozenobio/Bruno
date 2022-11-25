program name
    use iso_fortran_env, only: wp => real64
    use interpol,only:  pwlfit
    use interpol,only:  pwleval
    implicit none(type,external)
    real(wp),allocatable :: x(:),y(:),c(:,:),z(:)
    real(wp) :: eabs,pz,fz,erel
    integer i,k,n
    read(*,*) n
    x=[0.0_wp,1.0_wp,4.0_wp,9.0_wp,16.0_wp]
    y=[0.0_wp,1.0_wp,2.0_wp,3.0_wp,4.0_wp]
    z = [ 0.5_wp, 2.0_wp, 7.0_wp, 15.0_wp]
    call pwlfit(x,y,c)
    write(*,'(a)') '# pol interpolante: x|y|c'
    do i = 1, size(x)
        write(*,'(3(2x,g0.5))') x(i),y(i),c(i,:)
    end do
    write(*,*) 'c:'
    do i = 1, size(x)
        write(*,*) c(i,:)
    end do
    ! Evaluar el polinomio en los puntos pedidos
    write(*,*)
    write(*,'(a)') '# eval pol: z | p(z)| f(z) | e_abs | e_rel'
    do i = 1, size(z)
        pz = pwleval(x, c, z(i))
        fz = sqrt(z(i))
        eabs = abs( pz - fz )
        erel = eabs / fz
        write(*,'(a,5(2x,g0.5))') '#', z(i), pz, fz, eabs, erel
    end do
contains

    subroutine get_filename(filename)
        ! Devuelve el nombre del archivo pasado en la línea de comandos en
        ! una variable caracter del tamaño apropiado.
        character(:), allocatable, intent(out) :: filename
        integer :: ilen
        if ( command_argument_count() /= 1 ) then
            write(*,'(a)') 'Uso: programa archivo_datos'
            stop
        endif
        call get_command_argument(1,length=ilen)
        allocate(character(ilen) :: filename)
        call get_command_argument(1,filename)
    end subroutine get_filename
    

end program name

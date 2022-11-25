program name
    use iso_fortran_env, only: wp => real64
    use interpol,only:  polyfit
    use interpol,only:  polyeval
    implicit none(type,external)
    real(wp),allocatable :: x(:),y(:),c(:),z(:)
    real(wp) :: eabs,pz,fz,erel
    integer i,k,n
    character(:), allocatable :: filename
    !call get_filename(filename)
    ! Abrir archivo en modo solo lectura
    !open(8,file=filename,status='old',action='read',iostat=k)
    !if ( k /= 0 ) then
        !write(*,'(2a)') 'No se puede leer el archivo ', filename
        !stop
    !endif
    read(*,*) n
    z = [ 0.5_wp, 2.0_wp, 7.0_wp, 15.0_wp, 25.0_wp, 64.0_wp ]
    allocate(x(n),y(n),c(n))
    do i = 1, n, 1
        read(*,*) x(i),y(i)
    end do
    call polyfit(x,y,c)
    write(*,'(a)') '# pol interpolante: x|y|c'
    do i = 1, size(x)
        write(*,'(3(2x,g0.5))') x(i),y(i),c(i)
    end do
    ! Evaluar el polinomio en los puntos pedidos
    write(*,*)
    write(*,'(a)') '# eval pol: z | p(z)| f(z) | e_abs | e_rel'
    do i = 1, size(z)
        pz = polyeval(x, c, z(i))
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

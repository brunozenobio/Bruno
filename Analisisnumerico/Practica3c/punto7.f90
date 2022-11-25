program name
    use iso_fortran_env,only:wp => real64
    use f95_lapack, only:la_geev
    implicit none(type,external)
    real(wp), allocatable :: c(:)
    real(wp), allocatable :: a(:,:)
    real(wp), allocatable :: zr(:),zi(:)
    integer :: n,i,ok
    call get_coefs(c)
    ! El algoritmo falla si el coeficiente principal es cero.
    if (c(1) == 0.0_wp) then
        write(*,*) 'Error: coeficiente principal nulo'
        stop
    end if
    ! Asignar memoria para la matriz acompañante y vectores
    ! de las componentes reales e imaginarias de los autovalores
    n = size(c) - 1
    allocate(a(n,n), zr(n), zi(n))
    ! Construir la matriz acompañante
    a = 0.0_wp
    do i = 1, n-1
        a(i+1,i) = 1.0_wp
    end do
    a(:,n) = -c(n+1:2:-1)/c(1)
    ! Calcular los autovalores
    call la_geev(a,zr,zi,info=ok)
    ! Imprimir los autovalores (que son los ceros del polinomio)
    if (ok /=0) then
        write(*, '(a,i0)') 'Error = ', ok
    else
        write(*,'(a,i0)') '# Número de raíces = ', n
        write(*,'(a,2x,a,15x,a)') '#', 'parte real','parte imaginaria'
        write(*,'(2(es23.16,2x))') (zr(i), zi(i), i=1,n)
    end if
contains
    subroutine get_coefs(c)
    ! Devuelve en un arreglo los coeficientes de un polinomio
    ! leidos en la línea de comandos por orden decreciente.
    ! p(x)= c(1)*x**n + c(2)*x**(n-1) +...+ c(n+1)
    real(wp), allocatable, intent(out) :: c(:)
    integer :: n, len
    character(:), allocatable :: arg
    n = command_argument_count() - 1
    if ( n <= 0 ) then
        write(*,'(a)') 'Uso: pzero a(n) a(n-1) ... a(1) a(0)'
        stop
    endif
    allocate(c(n+1))
    do i=1,n+1
        call get_command_argument(i, length=len)
        allocate(character(len) :: arg )
        call get_command_argument(i,arg)
        read(arg,*,iostat=ok) c(i)
        if ( ok /= 0 ) then
            write(*,'(a,i0,a)') 'Coeficiente ', n+1-i, ' inválido.'
            stop
        end if
        deallocate(arg)
    end do
end subroutine get_coefs
    

end program name

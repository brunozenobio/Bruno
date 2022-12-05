program name
    use iso_fortran_env,only:wp => real64
    use f95_lapack, only: la_gels
    implicit none
    real(wp), allocatable :: a(:,:)
    real(wp), allocatable :: x(:), y(:)
    integer :: n, m, i, kode
    real(wp), parameter :: pi = 4.0_wp*atan(1.0_wp)
    ! Datos
    x = [ 0.0_wp, 2.0_wp, 4.0_wp, 6.0_wp, 8.0_wp, 10.0_wp ]
    y = [ 1.0_wp, 1.6_wp, 1.4_wp, 0.6_wp, 0.2_wp, 0.8_wp ]
    ! Construir la matriz de diseño
    m = size(y)
    n = 3
    allocate(a(m,n))
    a(:,1) = 1.0_wp
    a(:,2) = sin(pi*x/6.0_wp)
    a(:,3) = cos(pi*x/6.0_wp)
    ! Resolver el problema de mínimos cuadrados
    call la_gels(a, y, info=kode)
    if (kode == 0) then
    write(*,*) '# Número de datos = ', m
    write(*,*) '# Número de parametros = ', n
    write(*,*) '# Parámetros:'
    do i=1,n
    write (*,'(i0,2x,g0.5)') i, y(i)
    end do
    else
    write(*,*) 'Error = ', kode
    end if
end program name

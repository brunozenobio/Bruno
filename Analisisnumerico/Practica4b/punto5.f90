program name
    use iso_fortran_env,only:wp => real64
    use f95_lapack, only: la_gels
    implicit none
    real(wp), allocatable :: a(:,:)
    real(wp), allocatable :: x(:), y(:)
    integer :: n, m, i, kode
    real(wp), parameter :: pi = 4.0_wp*atan(1.0_wp)
    ! Datos
    x = [ 1.0_wp, 1.25_wp, 1.5_wp, 1.75_wp, 2.0_wp]
    y = [ log(5.1_wp),log(5.79_wp), log(6.53_wp), log(7.45_wp), log(8.46_wp)]
    ! Construir la matriz de diseño
    m = size(y)
    n = 2
    allocate(a(m,n))
    a(:,1) = 1.0_wp
    a(:,2) = x
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

program name
    use iso_fortran_env,only:wp => real64
    use f95_lapack, only:la_syev
    implicit none(type,external)
    integer i,n,k
    integer, parameter :: max_iter=100
    real(wp),parameter :: tol=5.0e-9_wp
    real(wp), allocatable :: a(:,:),w(:)
    read(*,*) n
    allocate(a(n,n),w(n))
    do i=1,n
        read(*,*) a(i,:)
    enddo
    call la_syev(a,w)
    write(*,*) w(:)


end program name
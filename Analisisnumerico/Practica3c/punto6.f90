program name
    use iso_fortran_env,only:wp => real64
    use f95_lapack, only:la_geev
    implicit none(type,external)
    integer i,n,k,info
    integer, parameter :: max_iter=100
    real(wp),parameter :: tol=5.0e-9_wp
    real(wp), allocatable :: a(:,:),wr(:),wi(:)
    read(*,*) n
    allocate(a(n,n),wr(n),wi(n))
    do i=1,n
        read(*,*) a(i,:)
    enddo
    call la_geev(a,wr,wi)
    write(*,*) wr(:),wi(:)


end program name
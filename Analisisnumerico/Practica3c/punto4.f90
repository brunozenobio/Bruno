program name
    use iso_fortran_env,only:wp => real64
    use qr, only:qr_gramschmidt
    implicit none(type,external)
    integer i,n,k
    integer, parameter :: max_iter=100
    real(wp),parameter :: tol=5.0e-9_wp
    real(wp), allocatable :: a(:,:),r(:,:),d(:),d0(:)
    n=2
    allocate(a(n,n),r(n,n),d(n),d0(n))
    do i=1,n
        read(*,*) a(i,:)
    enddo
    d0 = [ ( a(i,i), i=1,n) ]
    do k=1,max_iter
        call qr_gramschmidt(a,r)
        a = matmul(r,a)
        d = [ ( a(i,i), i=1,n) ]
        write(*,'(a,i0)') 'iter = ', k
        do i=1,n
            write(*,'(*(g0.5,2x))') a(i,:)
        end do
       
        if (maxval(abs(d-d0)) < maxval(abs(d))*tol) exit
        d0 = d
    end do
    

end program name

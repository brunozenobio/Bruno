program punto2
    use iso_fortran_env,only: wp => real64 
    use power_method,only:power_li
    implicit none
    real(wp),allocatable :: a(:,:),x(:)
    real(wp) tol,lambda
    integer clave,iter,n,i
    tol=5.0e-8_wp
    iter=1000
    n=3
    allocate(a(n,n),x(n))
    do i=1,n
        read(*,*) a(i,:),x(i)
    enddo
    call power_li(a,x,tol,iter,lambda,clave)
    if(clave==0) then
        write(*,*) 'Los autovalores y atuvovecores con el error ingresado se dieron en '
        write(*,*) 'autovalores'
        write(*,*) lambda
        write(*,*) 'autovectores'
        do i=1,n
            write(*,*) x(i)
        enddo
    else
        write(*,*) 'Error ', clave
    endif
end program punto2

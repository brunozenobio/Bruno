program name
    use iso_fortran_env, only: wp => real64
    use interpol,only:  polyfit
    use interpol,only:  polyeval
    implicit none(type,external)
    real(wp),allocatable :: x(:),y(:),c(:)
    real(wp) :: eabs,pz,fz,erel,z
    real(wp) :: a,b,h
    integer i,k,n
    character(:), allocatable :: filename
    read(*,*) n
    a=-1.0_wp
    b=1.0_wp
    h=(b-a)/(n-1)
    allocate(x(n),y(n))
    do i = 1, n, 1
        x(i)=a+h*(i-1)
        y(i)=f(x(i))
    end do
    call polyfit(x,y,c)
    write(*,'(a)') '# x|y|c'
    do i=1,n
        write(*,'(3(g0.5,2x))') x(i),y(i),c(i)
    end do
    ! Obtener los valores del polinomio interpolante
    ! en los puntos intermedios de la grilla y calcular
    ! el error absoluto y relativo
    write(*,'(a)') '# z | p(z) | f(z) | err_abs | err_abs'
    do i = 1, n-1
        z = (a + h/2.0_wp) + h*(i-1)
        pz = polyeval(x, c, z)
        fz = f(z)
        eabs = abs( fz - pz )
        erel = eabs / abs(fz)
        write(*,'(a,5(g0.5,2x))') '# ', z, pz, fz, eabs, erel
    end do
    open(unit=10, file='datos.dat')
    do i = 1, n, 1
        write(10,*) x(i),polyeval(x, c, x(i))
    end do
contains
    real(wp) function f(x)
        real(wp), intent(in) :: x
             f = 1.0_wp/(1.0_wp + 25.0_wp*x**2)
    end function f
    

end program name
program name
    use iso_fortran_env, only: wp => real64
    use interpol,only:  pwlfit
    use interpol,only:  pwleval
    implicit none(type,external)
    real(wp),allocatable :: x(:),y(:),c(:,:)
    real(wp) :: eabs,lz,fz,erel,z
    real(wp) :: a,b,h,pi
    integer i,k,n
    character(:), allocatable :: filename
    !f'' es menor a 50 por lo tanto sabiendo que |f-L|«=M2/8*h**2 y teniendo la cota para el error absouto y siendo que n=h/n-1 entonces n=1540
    n=1450
    a=-1.0_wp
    b=1.0_wp
    pi=acos(-1.0_wp)
    h=(b-a)/(n-1)
    allocate(x(n),y(n))
    do i = 1, n, 1
        x(i)=a+h*(i-1)
        y(i)=f(x(i))
    end do
    call pwlfit(x,y,c)
    n = 11
    h = (b-a)/(n-1)
    ! Obtener los valores del polinomio interpolante
    ! en los puntos intermedios de la grilla y calcular
    ! el error absoluto y relativo
    write(*,'(a)') '# z | p(z) | f(z) | err_abs | err_rel'
    do i = 1, n-1
        z = (a + h/2.0_wp) + h*(i-1)
        lz = pwleval(x, c, z)
        fz = f(z)
        eabs = abs( fz - lz )
        erel = eabs / abs(fz)
        write(*,'(a,5(g0.5,2x))') '# ', z, lz, fz, eabs, erel
    end do
contains
    real(wp) function f(x)
        real(wp), intent(in) :: x
             f = 1.0_wp/(1.0_wp + 25.0_wp*x**2)
    end function f
    

end program name
program name
    use iso_fortran_env,only:wp => real64
    use rk4_methods,only:rk4,euler
    implicit none(type,external)
    !!!!!!!!CONSTANTES!!!!!!!!!!!
    real(wp)        :: l,g,m
    !!!!!!!!CONDICIONES INICIALES!!!!!!!!!
    real(wp)        :: veltheta0,y0,t0,tf
    !!!!!!!VARIABLES DE BUCLES!!!!!!!
    integer         ::  i
    !!!!!!!INTERVALOS TEMPORALES!!!!!!!!!
    real(wp)        :: t,h,k
    integer         :: numpasos
    !!!!!!VARIABLE!!!!!!!!!!!!!!!
    real(wp)        :: y,veltheta,yf
    !!!!!!!!!!!!DEFINO LAS CONSTANTES
    g=-9.81_wp
    l=1.0_wp
    m=1.0_wp
    !!!!!!!!!!!!DEFINO LAS CONDICIONES INICIALES
    k=0.0_wp
    veltheta0=0.0_wp
  
  
  
  
  
  
  
  
    y0=1.0_wp
    tf=10.0_wp
    h=0.41_wp 
    t0=k
    do i = 0, 25, 1
        t=k+i*h
        call rk4(f,t0,y0,t,yf)
        write(*,*) t,yf
        y0=yf
        t0=t
    end do
contains
    real(wp) function f(x,y)
        real(wp), intent(in) :: x,y
        real(wp) :: g,l
        f=-5*y
    end function f
end program name


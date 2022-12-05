program name
    use iso_fortran_env,only: wp =>real64
    implicit none
    !Condiciones iniciales
    real(wp)             ::theta0,omega0,t0=0.0_wp,tf=100.0_wp
    !Constantes
    real(wp),parameter   :: g=-9.8_wp,l=1.0_wp
    !Variables 
    real(wp)             :: t,omega,theta,paso
    !Coordenadas cartesianas
    real(wp)             :: x,y
    !Pasos
    integer              :: nupasos
    !Variables de do
    integer              :: i
    theta0=acos(-1.0_wp)/4
    nupasos=10000
    omega0=0.0_wp
    paso=(tf-t0)/nupasos
    open(10,file='datospen.dat')
    do i=0,nupasos
        t=t0+i*paso
        omega=omega0+paso*f(theta0,g,l)
        theta=theta0+paso*omega0
        x=l*sin(theta)
        y=l*cos(theta)
        write(10,*) t,x,y
        omega0=omega
        theta0=theta
    enddo
contains
    real(wp) function f(y,g,l)
        real(wp), intent(in) ::y
        real(wp) :: g,l
        f=-g/l*sin(y)
    end function f
end program name

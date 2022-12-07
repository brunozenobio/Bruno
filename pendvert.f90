program name
    use iso_fortran_env,only: wp =>real64
    implicit none
    !Condiciones iniciales
    real(wp)             ::y0,dy0,t0=0.0_wp,tf=100.0_wp
    !Constantes
    real(wp),parameter   :: g=-9.8_wp,l=1.0_wp,gamma=0.5_wp
    !Variables 
    real(wp)             :: t,dy,paso,y,y1,dy1
    !Coordenadas cartesianas
    real(wp)             :: xx,yy,h,k1,k2,k3,k4
    real(wp)             :: l1,l2,l3
    !Pasos
    integer              :: nupasos
    !Variables de do
    integer              :: i
    y0=acos(-1.0_wp)/4
    nupasos=10000
    dy0=0.0_wp
    paso=(tf-t0)/nupasos
    h=paso
    open(10,file='datosprk.dat')
    y1=y0-dy0*h+h*h*f(y0,dy0)/2
    do i=0,nupasos
        y1=y0
        y0=y0+h*dy0+h**2*f(y0,dy0)/2
        dy0=dy0+h*(f(y1,dy1)+f(y0,dy0))/2
        t=t0+i*paso
        xx=l*sin(y0)
        yy=-l*cos(y0)
        write(10,*)t,',',xx,',',yy
    enddo
contains
    real(wp) function f(y,x)
        real(wp), intent(in) ::y,x
        real(wp),parameter :: g=9.81_wp,l=1.0_wp,gamma=1.0_wp
        f=-g/l*sin(y)-gamma*l*x
    end function f
end program name

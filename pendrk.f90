program name
    use iso_fortran_env,only: wp =>real64
    implicit none
    !Condiciones iniciales
    real(wp)             ::y0,dy0,t0=0.0_wp,tf=100.0_wp
    !Constantes
    real(wp),parameter   :: g=-9.8_wp,l=3.0_wp,gamma=0.0_wp
    !Variables 
    real(wp)             :: t,dy,paso,y
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
    do i=0,nupasos
        t=t0+i*paso
        k1=paso*dy0
        l1=paso*f(y0,dy0)
        k2=paso*(dy0+l1/2)
        l2=paso*f(y0+k1/2,dy0+l1/2)
        k3=paso*(dy0-l1+2*l2)
        l3=paso*f(y0-k1+2*k2,dy0-l1+2*l2)
        y=y0+(k1+4*k2+k3)/6
        dy=dy0+(l1+4*l2+l3)/6
        xx=l*sin(y)
        yy=-l*cos(y)
        write(10,*)t,',',xx,',',yy
        dy0=dy
        y0=y
    enddo
contains
    real(wp) function f(y,x)
        real(wp), intent(in) ::y,x
        real(wp),parameter :: g=9.81_wp,l=3.0_wp,gamma=0.1_wp
        f=-g/l*sin(y)-gamma*l*x
    end function f
end program name

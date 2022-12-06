program name
    use iso_fortran_env,only: wp =>real64
    implicit none
    !Condiciones iniciales
    real(wp)             ::y0,dy0,t0=0.0_wp,tf=100.0_wp
    !Constantes
    real(wp),parameter   :: g=-9.8_wp,l=1.0_wp,gamma=0.5_wp
    !Variables 
    real(wp)             :: t,dy,paso,y
    !Coordenadas cartesianas
    real(wp)             :: xx,yy,h,k1,k2,k3,k4
    !Pasos
    integer              :: nupasos
    !Variables de do
    integer              :: i
    y0=acos(-1.0_wp)/4
    nupasos=10000
    dy0=0.0_wp
    paso=(tf-t0)/nupasos
    open(10,file='datosprk.dat')
    do i=0,nupasos
        t=t0+i*paso
        h=tf
        k1=h*f(dy0,y0)
        k2=h*f(dy0+k1/2.0_wp,y0+k1/2.0_wp)
        k3=h*f(dy0+k1/2.0_wp,y0+k2/2.0_wp)
        k4 = h * f( dy0+h, y0+k3 )
        y = y0 + ( k1 + 2.0_wp * k2 + 2.0_wp * k3 + k4 )/6.0_wp
        xx=l*sin(y)
        yy=l*cos(y)
        write(10,*) t,',',xx,',',yy
        dy0=dy
        y0=y
    enddo
contains
    real(wp) function f(x,y)
        real(wp), intent(in) ::y,x
        real(wp),parameter :: g=-9.81_wp,l=1.0_wp,gamma=1_wp
        f=-g/l*sin(y)-gamma*l*x
    end function f
end program name

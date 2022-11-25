program name
    use iso_fortran_env,only:wp => real64
    use rk_methods,only:rk4
    implicit none(type,external)
    !!Condiciones iniciales
    real(wp)    :: thetai,vi,ti,g,l,dt,deltat,vtheta,t,theta,tf
    integer     :: n,i
    thetai=acos(-1.0_wp)/4.0_wp
    vi=0.0_wp
    ti=0.0_wp
    !!Constantes
    g=-9.81_wp
    l=1.0_wp
    !!TiEMPO
    dt=0.01_wp


    n=100000
    tf=100

    deltat=(tf-ti)/n
    do i=0,n
        t=ti+i*deltat
        call rk4(f,ti,vi,tf,vtheta)
        !call(h,ti,thetai,tf,theta)
        write(*,*) theta,vtheta
    enddo

contains
    real(wp) function f(t,x)
        real(wp), intent(in) :: x,t
        real(wp)             ::g,l
            g=-9.81_wp
            l=1.0_wp
            f = -g/l*sin(x)
    end function f    
    real(wp) function h(t,x)
        real(wp), intent(in) :: x,t
            h= x
    end function h   
end program name


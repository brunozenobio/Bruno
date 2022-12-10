program name
    use iso_fortran_env,only: wp =>real64
    implicit none
    real(wp) :: pi,l1,l2,m1,m2
    real(wp) :: y10,y20,dy10,dy20
    real(wp) :: y1,y2,dy1,dy2,xx1,xx2,yy1,yy2
    real(wp) :: a11,a21,a31,b11,b21,b31,a12,a22,a32,b12,b22,b32
    real(wp) :: a41,b41,a42,b42
    real(wp) :: tf,ti,paso,t
    integer  :: nupasos,i
    open(10,file='datospd.dat')
    pi=acos(-1.0_wp)
    m1=3.0_wp
    m2=1.0_wp
    l1=1.0_wp
    l2=1.0_wp
    y10=pi/2
    y20=-pi/8.0_wp
    dy10=1.0_wp
    dy20=0.0_wp
    paso=0.01
    nupasos=1000
    do i=0,nupasos
        t=paso*i
        a11=paso*dy10
        a12=paso*dy20
        b11=paso*f1(y10,dy10,y20,dy20)
        b12=paso*f2(y10,dy10,y20,dy20)
        a21=paso*(dy10+b11/2.0_wp)
        a22=paso*(dy20+b12/2.0_wp)
        b21=paso*f1(y10+a11/2.0_wp,dy10+b11/2.0_wp,y20,dy20)
        b22=paso*f2(y10+a11/2.0_wp,dy10+b11/2.0_wp,y20+&
       &a21/2.0_wp,dy20+b21/2.0_wp)
        a31=paso*(dy10+b21/2.0_wp)
        a32=paso*(dy20+b22/2.0_wp)
        b31=paso*f1(y10+a21/2.0_wp,dy10+b21/2.0_wp,y20+&
       &a22/2.0_wp,dy20+b22/2.0_wp)
        b32=paso*f2(y10+a21/2.0_wp,dy10+b21/2.0_wp,y20+&
       &a22/2.0_wp,dy20+b22/2.0_wp)
        a41=paso*(dy10+a31)
        a42=paso*(dy20+a32)
        b41=paso*f1(y10+a31/2.0_wp,dy10+b31/2.0_wp,y20+&
       &a32/2.0_wp,dy20+b32/2.0_wp)
        b42=paso*f2(y10+a31/2.0_wp,dy10+b31/2.0_wp,y20+&
       &a32/2.0_wp,dy20+b32/2.0_wp)
        dy1=dy10+1.0_wp/6.0_wp*(b11+2*b21+2*b31+b41)
        dy2=dy20+1.0_wp/6.0_wp*(b12+2*b22+2*b32+b42)
        y1=y10+1.0_wp/6.0_wp*(a11+2*a21+2*a31+a41)
        y2=y20+1.0_wp/6.0_wp*(a12+2*a22+2*a32+a42)
        xx1=l1*sin(y1)
        yy1=-l1*cos(y1)
        xx2=xx1+l2*sin(y2)
        yy2=yy1-l2*cos(y2)
        write(10,*)t,',',xx1,',',yy1,',',xx2,',',yy2
        y10=y1
        y20=y2
        dy10=dy1
        dy20=dy2
    enddo
contains
    real(wp) function f1(x1,v1,x2,v2)
        real(wp) :: x1,v1,x2,v2
        real(wp) :: g,m1,m2,l1,l2,delta,den1
        g=9.8_wp
        m1=3.0_wp
        m2=1.0_wp
        l1=1.0_wp
        l2=1.0_wp
        delta=x2-x1
        den1=(m1+m2)*l1-m2*l1*cos(delta)*cos(delta)
        f1=((m2*l1*v1**2*sin(delta)*cos(delta)+m2*g*sin(x2)*cos(delta)&
       &+m2*l2*v2**2*sin(delta)-(m1+m2)*g*sin(x1)))/den1
    end function f1
    real(wp) function f2(x1,v1,x2,v2)
        real(wp) :: x1,v1,x2,v2
        real(wp) :: g,m1,m2,l1,l2,den2,delta,den1
        g=9.8_wp
        m1=3.0_wp
        m2=1.0_wp
        l1=1.0_wp
        l2=1.0_wp
        delta=x2-x1
        den1=(m1+m2)*l1-m2*l1*cos(delta)*cos(delta)
        den2=(l2/l1)*den1
        f2=((-m2*l2*v2**2*sin(delta)*cos(delta)+(m1+m2)*g*sin(x1)*& 
        &cos(delta)-(m1+m2)*l1*v1**2*sin(delta)-(m1+m2)*g*sin(x2))/den2)
    end function f2
end program name

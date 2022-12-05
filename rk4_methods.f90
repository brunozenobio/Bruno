module rk4_methods
      use iso_fortran_env, only: wp => real64
      implicit none
      abstract interface
        ! Definición de la derivada y’ como una función de x e y.
        real(wp) function deriv(x, y)
        import :: wp
        real(wp), intent(in) :: x,y
        end function deriv
      end interface
contains
      subroutine euler( f, xi, yi, xf, yf )
          ! Aproximación numérica yf de la solución del problema de valor inicial
          !
          ! y’ = f(x,y), y(xi) = yi
          !
          ! en xf, por un paso del método de Euler.
          procedure(deriv) :: f
          real(wp), intent(in) :: xi
          real(wp), intent(in) :: yi
          real(wp), intent(in) :: xf
          real(wp), intent(out) :: yf
          yf = yi + (xf - xi) * f(xi,yi)
      end subroutine euler
      subroutine eulermod( f, xi, yi, xf, yf )
          ! Aproximación numérica yf de la solución del problema de valor inicial
          !
          ! y’ = f(x,y), y(xi) = yi
          !
          ! en xf, por un paso del método de Euler modificado.
          procedure(deriv) :: f
          real(wp), intent(in) :: xi
          real(wp), intent(in) :: yi
          real(wp), intent(in) :: xf
          real(wp), intent(out) :: yf
          real(wp) :: h
          real(wp) :: k1
          h = xf - xi
          k1 = h * f(xi, yi)
          yf = yi + h * f( xi+h/2.0_wp, yi+k1/2.0_wp )
      end subroutine eulermod
      subroutine rk4( f, xi, yi, xf, yf )
          ! Aproximación numérica yf de la solución del problema de valor inicial
          !
          ! y’ = f(x,y), y(xi) = yi
          !
          ! en xf, por un paso del método de Runge-Kutta de cuarto orden.

          procedure(deriv) :: f
          real(wp), intent(in) :: xi
          real(wp), intent(in) :: yi
          real(wp), intent(in) :: xf
          real(wp), intent(out) :: yf
          real(wp) :: h
          real(wp) :: k1, k2, k3, k4
          h = xf - xi
          k1 = h * f( xi, yi )
          k2 = h * f( xi+h/2.0_wp, yi+k1/2.0_wp )
          k3 = h * f( xi+h/2.0_wp, yi+k2/2.0_wp )
          k4 = h * f( xi+h, yi+k3 )
          yf = yi + ( k1 + 2.0_wp * k2 + 2.0_wp * k3 + k4 )/6.0_wp
      end subroutine rk4
end module rk4_methods
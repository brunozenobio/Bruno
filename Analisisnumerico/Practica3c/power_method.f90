    module power_method
        use iso_fortran_env, only: wp => real64
        implicit none
    contains
        subroutine power_li(a,x,tol,iter,lambda,clave)
            ! Calculo del autovalor dominante y su respectivo autovector
            ! por el método de la potencia (versión norma infinito)
            !
            ! Argumentos de la subrutina:
            !
            ! Matriz del problema
            real(wp), intent(in) :: a(:,:)
            ! Vector inicial (entrada) / Estimación del autovector (salida)
            real(wp), intent(inout) :: x(:)
            ! Tolerancia prefijada para el criterio de paro
            real(wp), intent(in) :: tol
            ! Máximo de iteraciones (entrada) /iteraciones realizadas (salida)
            integer , intent(inout) :: iter
            ! Estimación del autovalor dominante
            real(wp), intent(out) :: lambda
            ! Clave de error: 0 ok, =/ 0 iteraciones máximas alcanzado
            integer, intent(out) :: clave
            ! Variables locales
            integer :: i
            integer :: p
            real(wp) :: lambda0
            real(wp) :: y(size(x))
            ! Procedimiento
            clave = 1
            lambda0 = 0.0_wp
            p = maxloc(abs(x),1)
            x = x/x(p)
            do i = 1, iter
                y = matmul(a,x)
                lambda= y(p)
                p = maxloc(abs(y),1)
                x = y/y(p)
                if ( abs(lambda-lambda0) <= tol*abs(lambda) ) then
                    clave = 0
                    iter = i
                    exit
                end if
                lambda0 = lambda
            end do
        end subroutine power_li
    end module power_method
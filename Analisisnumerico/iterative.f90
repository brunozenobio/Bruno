module iterative

  use iso_fortran_env, only: wp => real64
  implicit none

  private :: matvec_sparse

contains

  subroutine jacobi(a,b,x,tol,iter,clave)

    ! METODO DE JACOBI para la resolución por iteración del
    ! sistema de ecuaciones lineales Ax=b. 
    
    ! Argumentos de la subrutina
    real(wp), intent(in)    :: a(:,:) ! Matriz del sistema
    real(wp), intent(in)    :: b(:)   ! Término independiente
    real(wp), intent(inout) :: x(:)   ! Aproximación inicial/solución
    real(wp), intent(in)    :: tol    ! Tolerancia para el error
    integer,  intent(inout) :: iter   ! Max iteraciones/iter realizadas
    integer,  intent(out)   :: clave  ! Clave de éxito/error
                                      ! clave  = 0, OK.
                                      ! clave /= 0, max iter. alcanzado
    ! Variables locales
    real(wp) :: diag(size(a,1)) ! Arreglo para guardar la diagonal de A
    real(wp) :: x0(size(x))     ! Arreglo para guardar la iteración anterior
    integer  :: k               ! Contador de iteraciones
    integer  :: i               ! Indice

    ! Procedimiento
    clave = 1
    diag  = [ (a(i,i), i=1,size(a,1)) ]

    do k=1,iter
       x0 = x
       x  = x0 + (b - matmul(a,x0))/diag
       if ( maxval(abs(x-x0)) <= maxval(abs(x))*tol ) then
          clave = 0
          iter  = k
          exit
       end if
    end do

  end subroutine jacobi

  subroutine gauss_seidel(a,b,x,tol,iter,clave)

    ! METODO DE GAUSS SEIDEL para la resolución por iteración del
    ! sistema de ecuaciones lineales Ax = b.
    
    ! Argumentos de la subrutina
    real(wp), intent(in)    :: a(:,:) ! Matriz del sistema
    real(wp), intent(in)    :: b(:)   ! Término independiente
    real(wp), intent(inout) :: x(:)   ! Aproximación inicial/solución
    real(wp), intent(in)    :: tol    ! Tolerancia para el error
    integer,  intent(inout) :: iter   ! Max iteraciones/iter realizadas
    integer,  intent(out)   :: clave  ! Clave de éxito/error
                                      ! clave  = 0, OK.
                                      ! clave /= 0, max iter. alcanzado
    ! Variables locales
    integer  :: n         ! Dimensión del problema
    integer  :: k         ! Contador de iteraciones
    integer  :: i         ! Indice
    real(wp) :: xi        ! Componente del vector iterado
    real(wp) :: xnorma    ! Norma del vector iterado
    real(wp) :: difnorma  ! Norma de la diferencia entre dos iteraciones

    ! Procedimiento
    clave = 1
    n     = size(a,1)

    do k=1,iter
       xnorma   = 0.0_wp
       difnorma = 0.0_wp
       do i=1,n
          xi       = (b(i) - dot_product(a(i,1:i-1),x(1:i-1)) &
              & - dot_product(a(i,i+1:n),x(i+1:n)))/a(i,i)
          xnorma   = max(xnorma,abs(xi))
          difnorma = max(difnorma,abs(xi-x(i)))
          x(i)     = xi
       end do
       if (difnorma <= xnorma*tol) then
          iter  = k
          clave = 0
          exit
       end if
    end do

  end subroutine gauss_seidel

  subroutine jacobi_sparse(ia,ja,a,b,x,tol,iter,info)

    ! METODO DE JACOBI para la resolución por iteración del sistema de ecuaciones
    ! lineales Ax = b donde A es una MATRIZ RALA, la cual es dada en la
    ! representación de coordenadas.
    !
    ! Argumentos:
    !
    ! Input, integer, dimension(:), ia,
    ! Input, integer, dimension(:), ja,
    ! Input, real(wp),dimension(:), a:
    !  arreglos de la representación de coordenadas de la matriz rala.
    !
    ! Input, real(wp), dimension(:), b:
    !  término independiente del sistema de ecuaciones.
    !
    ! Input/Output, real(wp), dimension(:), x:
    !   como dato de entrada es una aproximación inicial a la solución,
    !   como dato de salida es la solución numérica.
    !
    ! Input, real(wp), tol:
    !   tolerancia para el criterio de convergencia.
    !
    ! Input/Output, integer, iter:
    !   como dato de entrada es el número máximo de iteraciones,
    !   como dato de salida es el número de iteraciones realizadas.
    !
    ! Output, integer, info:
    !   Código de error:
    !   info  = 0 => el método convergió,
    !   info /= 0 => el método no convergió después de realizar el
    !                número de máximas iteraciones.
    
    ! Argumentos
    integer,  intent(in)    :: ia(:)
    integer,  intent(in)    :: ja(:)
    real(wp), intent(in)    :: a(:)
    real(wp), intent(in)    :: b(:)
    real(wp), intent(inout) :: x(:)
    real(wp), intent(in)    :: tol
    integer,  intent(inout) :: iter
    integer,  intent(out)   :: info

    ! Variables locales
    real(wp) :: diag(size(b))
    real(wp) :: x0(size(b))
    integer  :: nelt
    integer  :: k

    info = 1
    nelt = size(ia)

    ! Obtener la diagonal de A
    do k=1,nelt
       if (ia(k) == ja(k) ) then
          diag(ia(k)) = a(k)
       end if
    end do

    ! Proceder con las iteraciones
    do k=1,iter
       x0 = x
       x = x0 + ( b - matvec_sparse(ia,ja,a,x0) )/diag
       if ( maxval(abs(x-x0)) <= maxval(abs(x))*tol ) then
          info = 0
          iter = k
          exit
       end if
    end do

  end subroutine jacobi_sparse

  function matvec_sparse(ia,ja,a,x) result(y)

    ! matvec_sparse: Computa el vector y = Ax, resultante del producto de
    ! una matriz rala A por un vector x.
    !
    ! Argumentos
    !
    ! Input, integer, dimension(:), ia,
    ! Input, integer, dimension(:), ja,
    ! Input, real(wp),dimension(:), a:
    !  arreglos de la representación de coordenadas de la matriz rala.
    !
    ! Input, real(wp), dimension(:), x,
    !   vector a ser multiplicado por A.
    !
    ! Output, real(wp), dimension(:), y,
    !   vector y = Ax.
    !

    ! Argumentos
    integer,  intent(in) :: ia(:)
    integer,  intent(in) :: ja(:)
    real(wp), intent(in) :: a(:)
    real(wp), intent(in) :: x(:)
    real(wp)             :: y(size(x))

    ! Variables locales
    integer :: k

    ! Procedimiento
    y = 0.0_wp

    do k=1,size(ia)
       y(ia(k)) = y(ia(k))+a(k)*x(ja(k))
    end do

  end function matvec_sparse

end module iterative

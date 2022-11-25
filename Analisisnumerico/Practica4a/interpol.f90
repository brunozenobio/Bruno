module interpol

  use iso_fortran_env, only: wp => real64
  implicit none

  private :: locate

contains

  subroutine polyfit(x, y, c)

    ! Calcula los coeficientes c(1),c(2),...c(n) del polinomio de grado n-1
    ! que interpola los n puntos (x(1),y(1)), (x(2),y(2)),...,(x(n),y(n))
    ! expresado según la fórmula de Newton:
    !
    ! p(x) = c(1) + c(2)*(x-x(1)) + ... + c(n)*(x-x(1))...(x-x(n-1))
    !
    ! Argumentos:
    !
    ! Vector de entrada con los nodos de la interpolación
    real(wp), intent(in)  :: x(:)
    ! Vector de entrada con los valores a interpolar
    real(wp), intent(in)  :: y(:)
    ! Vector de salida con los coeficientes del polinomio interpolante
    real(wp), allocatable, intent(out) :: c(:)

    integer  :: n,i,k

    n = size(x)
    allocate (c(n))

    c(1) = y(1)
    do k=2,n
       c(k) = y(k)
       do i=1,k-1
          c(k) = (c(k)-c(i))/(x(k)-x(i))
       end do
    end do

  end subroutine polyfit

  function polyeval(x, c, z) result(p)

    ! Evalua la forma de Newton del polinomio interpolante en un punto z dado.
    !
    ! Argumentos:
    !
    ! Vector de entrada con los nodos de interpolación
    real(wp), intent(in) :: x(:)
    ! Vector de entrada con los coeficientes del polinomio interpolante
    real(wp), intent(in) :: c(:)
    ! Dato de entrada correspondiente al punto donde evaluar el polinomio
    real(wp), intent(in) :: z
    ! Valor del polinomio interpolante en z
    real(wp) :: p

    integer  :: n,k

    n = size(x)
    p = c(n)
    do k=n-1,1,-1
       p = c(k) + p*(z-x(k))
    end do

  end function polyeval

  subroutine pwlfit(x, y, c)

    ! Determina los coeficientes del polinomio lineal segmentario que interpola
    ! el conjunto de n puntos (x(i),y(i)) donde los n nodos están ordenados en
    ! forma monótonamente creciente:
    !
    !                      x(1) < x(2) < ... < x(n).
    !
    ! Sobre cada uno de los n-1 intervalos [x(i), x(i+1)], i = 1,2,...,n-1,
    ! el interpolador lineal segmentario tiene la forma:
    !
    !         L_i(x) = c(i,1) + c(i,2) (x -x(i))
    !
    ! Argumentos:
    !
    ! Vector de entrada con los nodos ordenados en forma creciente
    real(wp), intent(in)  :: x(:)
    ! Vector de entrada con 
    real(wp), intent(in)  :: y(:)
    ! Matriz de salida con los coeficientes del interpolador lineal segmentario
    real(wp), allocatable, intent(out) :: c(:,:)

    integer :: n

    n = size(x)
    allocate(c(n-1,2))
    
    c(:,1) = y(1:n-1)
    c(:,2) = ( y(2:n) - y(1:n-1) ) / ( x(2:n) - x(1:n-1) ) 
    
  end subroutine pwlfit

  function pwleval(x, c, z) result(p)

    ! Evalua el interpolador lineal segmentario, que interpola un conjunto
    ! de n datos, en un punto z dado.
    !
    ! Argumentos:
    !
    ! Vector de entrada con los nodos ordenados en forma creciente
    real(wp), intent(in) :: x(:)
    ! Matriz de entrada con los coeficientes del interpolador lineal segmentario
    real(wp), intent(in) :: c(:,:)
    ! Dato de entrada correspondiente al punto donde evaluar el polinomio
    real(wp), intent(in) :: z
    ! Valor del interpolador lineal segmentario en z
    real(wp) :: p
    
    integer :: i

    i = locate(x,z)
    p = c(i,1) + c(i,2)*( z - x(i) )
    
  end function pwleval

  function locate(x,z) result(i)
    
    ! Sea x un vector de n componentes monótonamente creciente:
    !
    !                x(1) < x(2) < ... < x(n)
    !
    ! Esto define una partición del intervalo [x(1):x(n)] en n-1 subintervalos:
    !
    ! 1  : [x(1),x(2))
    ! 2  : [x(2),x(3))
    ! ...
    ! i  : [x(i),x(i+1))
    ! ...
    ! n-1: [x(n-1):x(n))
    !
    ! Dado un número z dentro del intervalo [x(1):x(n)] queremos determinar el
    ! índice i entre 1 y n-1 tal que:
    !
    !                      x(i) <= z < x(i+1)
    !
    ! Si z está fuera del rango definido por [x(1):x(n)) se devuelve el intervalo
    ! más próximo a z, es decir i = 1 si z < x(1) ó i = n-1 si z >= x(n) 
    !
    ! Argumentos:
    !
    ! Vector de entrada que define la partición del intervalo
    real(wp), intent(in) :: x(:)
    ! Dato de entrada correspondiente al valor de z
    real(wp), intent(in) :: z
    ! Dato de salida correspondiente al índice del subintervalo buscado
    integer :: i

    integer :: j, k

    ! Búsqueda binaria
    i = 1
    j = size(x)
    do
       k = i + (j-i)/2
       if ( z < x(k) ) then
          j = k
       else
          i = k
       end if
       if ( i + 1 >= j ) exit
    end do

!    integer :: n
!    
!    ! Busqueda lineal
!    n = size(x)
!    i = n - 1 
!    do j = 2, n-1
!       if ( z < x(j) ) then
!          i = j - 1
!          exit
!       end if
!    end do
    
  end function locate

end module interpol

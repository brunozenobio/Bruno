module qr
    use iso_fortran_env, only: wp => real64
    implicit none
contains
    subroutine qr_gramschmidt(a,r)
    ! Determinación de la factorización QR de una matriz A
    ! a través del método (modificado) de ortogonalización
    ! de Gram-Schmidt.
    !
    ! Argumentos:
    !
    ! Arreglo mxn:
    ! * entrada: matriz A a factorizar
    ! * salida: matriz Q de la factorización
    real(wp), intent(inout) :: a(:,:)
    !
    ! Arreglo nxn:
    ! * salida: matriz R triangular superior de
    ! la factorización
    real(wp), intent(out) :: r(:,:)
    integer :: i,j
    r = 0.0_wp
    do j = 1, size(a,2)
        do i=1,j-1
            r(i,j) = dot_product(a(:,i),a(:,j))
            a(:,j) = a(:,j)-r(i,j)*a(:,i)
        end do
        r(j,j) = norm2(a(:,j)) ! sqrt(dot_product(a(:,j),a(:,j)))
        a(:,j) = a(:,j)/r(j,j)
    end do
    end subroutine qr_gramschmidt
end module qr
program name
    use iso_fortran_env, only: wp => real64
    use f95_lapack, only: la_gels
    implicit none
    real(wp),allocatable   :: a(:,:)
    real(wp),allocatable   :: x(:),y(:)
    real(wp)               :: normmin
    character(1)           :: trans
    character(:), allocatable :: filename
    integer                :: info,n,g,m,i,j,kode
    call get_from_cmd(g, filename)
    open(8,file=filename,action='read',status='old',iostat=kode)
    if (kode /= 0) then
        write(*,'(a)') 'El archivo de datos no pudo ser leido'
        stop
    endif  
    read(8,*) m
    allocate(x(m),y(m))
    do i=1,m
        read(8,*) x(i), y(i)
    end do
    close(8)
    n=g+1
    if(n > m) then
        write (*,'(a)') 'Error: número de parámetros mayor que el de datos'
        stop
    endif
    allocate(a(m,n))
    a(:,1) = 1.0_wp
    do j = 2, n
        a(:,j) = x * a(:,j-1)
    end do
    call la_gels(a, y, info=kode)
    if (kode /= 0) then
        write(*,*) 'Error = ', kode
        stop
    endif
    normmin=sqrt(sum(y(n+1:m)**2))
    write(*,*) 'El grado del polinomio es ',g
    write(*,*) 'El numero de puntos es ',m
    write(*,*) 'La cantidad de parametros es ',n
    write(*,*) 'La norma del vector de residuos es ',normmin
    write(*,*) 'Y los parametros del polinomio son:'
    do i=1,n
        write (*,'(i0,2x,g0.5)') i, y(i)
    enddo
contains
    subroutine get_from_cmd(g,string)
         integer, intent(out) :: g
         character(:), allocatable, intent(out) :: string
         integer :: ilen
         character(:), allocatable :: arg
         call get_command_argument(1,length=ilen)
         allocate(character(ilen) :: arg)
         call get_command_argument(1,arg)
         string = arg
         deallocate(arg)
         call get_command_argument(2,length=ilen)
         allocate(character(ilen) :: arg)
         call get_command_argument(2,arg)
         read(arg,*) g
    end subroutine get_from_cmd


end program name
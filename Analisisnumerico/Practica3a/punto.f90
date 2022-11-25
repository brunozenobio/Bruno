 program punto3
 
    !Resuelve el sistema de ecuaciones AX = B donde A es una matriz n x n y
    ! B es una matriz n x m. Los elementos de estas matrices son leídos de
    ! un archivo pasado en la línea de comandos y cuyo formato es:
    !
    ! n m
    ! A(1,1) A(1,2) ... A(1,n) B(1,1) .... B(1,m)
    ! A(2,1) A(2,2) ... A(2,n) B(2,1) .... B(2,m)
    ! ...
    ! A(n,1) A(n,2) ... A(n,n) B(n,1) .... B(n,m)
    !
    ! Cargar los módulos apropiados
    use iso_fortran_env, only : wp => real64
    use f95_lapack, only: la_gesv
    ! Declaración de variables
    implicit none (type, external)
    integer :: n,m ! dimensiones para A y B
    integer :: ok ! clave de error
    integer :: i ! indice
    integer , allocatable :: p(:) ! vector de pivoteos
    real(wp), allocatable :: a(:,:) ! matriz A
    real(wp), allocatable :: b(:,:) ! matriz B
    character(:), allocatable :: filename ! archivo de datos
    ! Obtener el nombre del archivo
    call get_filename(filename)
    ! Abrir archivo en modo solo lectura
    open(8,file=filename,status='old',action='read',iostat=ok)
    if ( ok /= 0 ) then
    write(*,'(2a)') 'No se puede leer el archivo ', filename
    stop
    endif
    ! Leer datos del archivo
    read(8,*) n,m
    allocate(a(n,n),b(n,m),p(n))
    do i=1,n
    read(8,*) a(i,:),b(i,:)
    end do
    close(8)
    ! Eco en la terminal
    write(*,'(a)') 'Arreglo A|B ='
    write(*,*)
    do i=1,n
    write(*,'(*(g0.5,2x))') a(i,:), '|', b(i,:)
    end do
    write(*,*)
    ! Resolver los sistemas
    call la_gesv(a,b,p,ok)
    ! Imprimir resultados
    write(*,'(a)') 'Resultados de la subrutina la_gesv:'
    write(*,*)
    write(*,'(a,i0)') 'Info = ', ok
    write(*,*)
    write(*,'(a)') 'Arreglo A|B ='
    write(*,*)
    do i=1,n
    write(*,'(*(g0.5,2x))') a(i,:), '|', b(i,:)
    end do
    write(*,*)
    write(*,'(a,*(i0,:,", "))') 'Vector p = ', p
    contains
    subroutine get_filename(filename)
    ! Devuelve el nombre del archivo pasado en la línea de comandos en
    ! una variable caracter del tamaño apropiado.
    character(:), allocatable, intent(out) :: filename
    integer :: ilen
    if ( command_argument_count() /= 1 ) then
    write(*,'(a)') 'Uso: programa archivo_datos'
    stop
    endif
    call get_command_argument(1,length=ilen)
    allocate(character(ilen) :: filename)
    call get_command_argument(1,filename)
    end subroutine get_filename
end program punto3
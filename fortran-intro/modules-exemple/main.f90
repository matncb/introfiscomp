program main
    ! Use serve para carregar o módulo
    use area
    implicit none

    real :: r = 2., A

    ! Roda a função do módulo area
    A = find_area(r)

    print *, 'A área é :', A

end program main

! Para compilar, precisamos compilar area.f90 antes e depois main.f90
! gfortran -Wall -Wextra area.f90 main.f90 -o ./build/main

! Também podemos compilar separadamente e linkar depois
! gfortran -Wall -Wextra -c area.f90 -o ./build/area.o
! gfortran -Wall -Wextra -c main.f90 -o ./build/main.o
! gfortran -Wall -Wextra ./build/main.o ./build/area.o -o ./build/main
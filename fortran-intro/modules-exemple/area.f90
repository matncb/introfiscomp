! Módulo para calcular a área de um círculo

module area
    implicit none

    ! Define parâmetro geral pi
    real, parameter :: pi = 4.*atan(1.)

contains

    real function find_area(r)
        implicit none
        
        ! intent(in) diz que r é apenas valor de entrada
        real, intent(in) :: r
        find_area = pi * r**2
    end function find_area

end module area
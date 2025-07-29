program area

    implicit none ! Remove declaração implícita de variáveis

    real :: r = 2.0, A
    real, parameter :: pi = 4*atan(1.0)

    A = area_formula(r)

    print *, 'pi is', pi
    print *, 'Area is', A

contains

    real function area_formula(raio)
        implicit none
        real, intent(in) :: raio
        area_formula = pi*raio**2
    end function area_formula

end program area
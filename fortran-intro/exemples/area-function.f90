program area

    implicit none ! Remove declaração implícita de variáveis

    real :: r = 2.0, A

    ! Usa a função
    A = area_formula(r)
    
    print *, 'Area is', A

! Dentro de contains declaramos funções e subrotinas
contains
    ! Declarando uma função --> retorna um valor
    ! Declarando uma subrotina --> Não há valor retornado

    real function area_formula(radius)
        implicit none
        real, parameter :: pi = 4*atan(1.0)
        real, intent(in) :: radius

        ! intent(in) diz que valor de entrada 

        ! Para retornar o valor, o nome da função recebe o valor
        area_formula = pi*radius**2
    end function area_formula

end program area
program area

    implicit none ! Remove declaração implícita de variáveis

    ! Declara variável real
    real :: r = 2.0, A

    ! parameter diz que é imutável
    real, parameter :: pi = 4.*atan(1.0)

    A = pi*r**2

    print *, 'pi is', pi
    print *, 'Area is', A

end program area
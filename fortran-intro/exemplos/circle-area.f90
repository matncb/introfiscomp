program area

    implicit none ! Remove declaração implícita de variáveis

    real :: r = 2.0, A
    real, parameter :: pi = 4*atan(1.0)

    A = pi*r**2

    print *, 'pi is', pi
    print *, 'Area is', A

end program area
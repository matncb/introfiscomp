program exer1
    implicit none

    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp

    real(p), parameter :: x0 = 0.5
    real(p), parameter :: h_list(14) = (/0.5,0.2,0.1,0.05,0.01, &
                            & 0.005,0.001, 0.0005,0.0001, 0.00005, 0.00001, &
                            & 0.000001, 0.0000001, 0.00000001 /)

    call table(x0, h_list)

contains
    ! Carregar entrada
    ! Funções teóricas
    function f(x) result(res)
        real(p) :: x
        real(p) :: res
        res = log(1 + x**2)
    end function f

    function df(x) result(res)
        real(p) :: x
        real(p) :: res
        res = 2*x/(1 + x**2)
    end function df

    function ddf(x) result(res)
        real(p) :: x
        real(p) :: res
        res = 2/((1 + x**2)**2)
    end function ddf

    function dddf(x) result(res)
        real(p) :: x
        real(p) :: res
        res = -8*x/((1 + x**2)**3)
    end function dddf

    ! Discretização

    function fn(x0,h,n) result(res)
        real(p) :: x0
        real(p) :: h
        integer :: n

        real(p) :: res

        res = f(x0 + h*n)
    end function fn

    ! Derivadas seguindo a ordem do pdf

    function d1(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 1) - fn(x0, h, 0))/h
    end function d1

    function d2(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 0) - fn(x0, h, -1))/h
    end function d2

    function d3(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 1) - fn(x0, h, -1))/(2*h)
    end function d3

    function d4(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (-fn(x0, h, 2) + 8*fn(x0, h, 1) -8*fn(x0, h, -1) + fn(x0, h, -2)) /(12*h)
    end function d4

    function d5(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 1) -2*fn(x0, h, 0) + fn(x0, h, -1) )/(h**2)
    end function d5

    function d6(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (-fn(x0, h, 2) +16*fn(x0, h, 1) - 30*fn(x0, h, 0) &
             &+16*fn(x0,h,-1) - fn(x0,h, -2))/(12*h**2)
    end function d6


    function d7(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 2) -2*fn(x0, h, 1) + 2*fn(x0, h, -1) - fn(x0,h,-2) )/(2*h**3)
    end function d7
    
    ! Elaborar tabela

    subroutine table(x0, h_list)
        real(p):: x0
        real(p):: h_list(14)
        integer :: i

        open(unit=1, file='tab1_out.dat', status='replace') 

        write (1,*) "h ", "derivada simétrica 3 pontos ", "derivada para frente 2 pontos ", &
                &"derivada para trás 2 pontos ", "derivada segunda simétrica 3 pontos ", &
                & "derivada segunda simétrica 5 pontos ", "derivada terceira anti-simétrica 5 pontos"

        do i = 1, 14
            write (1,*) h_list(i), d3(x0,h_list(i)), d1(x0,h_list(i)), d2(x0,h_list(i)),&
             & d5(x0,h_list(i)), d6(x0,h_list(i)), d7(x0,h_list(i))
        end do

        close(unit = 1)
    end subroutine table

end program exer1
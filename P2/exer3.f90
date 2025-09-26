module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão

    private
    public :: sp, dp, p
end module precision


module func
    use precision
    implicit none

    private 
    public :: f, df

contains
    function f(x) result(res)
        real(p) :: x
        real(p) :: res
        res = x**3 - 2*x**2 -x + 3
    end function f

    function df(x) result(res)
        real(p) :: x
        real(p) :: res
        res = 3*x**2 - 4*x -1
    end function df

end module func

module methods
    use precision
    use func
    implicit none

contains
    function NR(x0, N) result(res)
        integer :: i, N
        real(p) :: x0, res

        real(p) :: xi, xi1
        xi = x0
        do i = 0, N-1
            xi1 = xi - f(xi)/df(xi)
            xi = xi1
        end do

        res = xi1
    end function NR

    function sec(x0, x1, N) result(res)
        integer :: i, N
        real(p) :: x0, x1, res

        real(p) :: xi, xi1, xi11
        xi11 = x0
        xi = x1

        do i = 0, N-1
            xi1 = xi - f(xi) * (xi - xi11)/(f(xi) - f(xi11))
            xi11 = xi
            xi = xi1
        end do

        res = xi1
    end function sec

end module methods



program exer3
    implicit none


end program exer3
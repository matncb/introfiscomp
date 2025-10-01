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
        res = x**3 - 3*x**2 -x + 3
    end function f

    function df(x) result(res)
        real(p) :: x
        real(p) :: res
        res = 3*x**2 - 6*x -1
    end function df

end module func

module methods
    use precision
    use func
    implicit none

contains
    function dir(x0, N) result(res)
        integer, intent(in) :: N
        real(p), intent(in) :: x0
        real(p) :: res

        real(p) :: a, b, h, m
        integer :: i, max_steps
        
        h = 0.1_p         
        max_steps = 1000  
        
        a = x0
        b = x0
        do i = 1, max_steps
            b = x0 + i * h
            if (f(a) * f(b) < 0.0_p) exit
            
            a = x0 - i * h
            if (f(b) * f(a) < 0.0_p) then
                m = a
                a = b
                b = m
                exit
            end if
            a = x0 
        end do

        if (f(a) * f(b) < 0.0_p) then
            do i = 1, N
                m = (a + b) / 2.0_p
                if (f(a) * f(m) < 0.0_p) then
                    b = m
                else
                    a = m
                end if
            end do
            res = (a + b) / 2.0_p
        else
            res = -9999.9_p
        end if
        
    end function dir

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

module data_handler
    use precision
    use methods
    use func
    implicit none

    contains

    subroutine gen_out(N, x0_list, x1_list)
        integer, intent(in) :: N
        real(p), intent(in) :: x0_list(3)
        real(p), intent(in) :: x1_list(3)
        
        real(p), allocatable :: matrix_out(:,:)

        integer :: i, j

        character(len = 50) :: label(10)
        label(1) = "iter"
        label(2) = "dir1"
        label(3) = "dir2"
        label(4) = "dir3"
        label(5) = "NR1"
        label(6) = "NR2"
        label(7) = "NR3"
        label(8) = "sec1"
        label(9) = "sec2"
        label(10) = "sec3"
        
        ! Derivada simétrica de 5 pontos não é usada msm?

        allocate(matrix_out(N, 9))

        do i = 1, N
            do j =1, 3
                matrix_out(i,j) = dir(x0_list(j),i)
                matrix_out(i, 3 + j) = NR(x0_list(j),i)
                matrix_out(i, 6 + j) = sec(x0_list(j),x1_list(j),i)
            end do
        end do

        open(unit=1, file="tab3_out.dat", status="replace", action = "write") 

        do i = 1, 10
            write(1, '(A, A)', advance='no') trim(label(i)), '   '
        end do
        write(1, *) 
            
        do i = 1, N
            write(1,"(I5)", advance = "no") i
            write(1, *) matrix_out(i, :)
        end do

        close(1)

        deallocate(matrix_out)
    end subroutine gen_out

end module data_handler

program exer3
    use precision
    use data_handler
    implicit none

    real(p),parameter :: x0_list(3) = (/-1.3,1.3,2.7/)
    real(p), parameter :: x1_list(3) = (/-1.4,1.4,2.8/)

    integer :: N

    read(*,*) N

    call gen_out(N, x0_list, x1_list)
end program exer3
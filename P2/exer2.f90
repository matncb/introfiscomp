! Modulo para o cálculo a partir das funções analíticas

module parametros
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão

    real(p), parameter :: a = 0., b = 1.

    private
    public :: sp, dp, p, a, b
end module parametros

module func
    use parametros
    implicit none

    private
    public :: f, f_primitiva, int_f
contains

    function f(x) result(res)
        real(p) :: x
        real(p) :: res
        res = x**2 * cos(x)
    end function f

    function f_primitiva(x) result(res)
        real(p) :: x
        real(p) :: res
        res = x**2 * sin(x) + 2*x*cos(x) -2*sin(x)
    end function f_primitiva

    function int_f() result(res)
        real(p):: res
        res = f_primitiva(b) - f_primitiva(a)
    end function int_f

end module func

module integrals
    use parametros
    use func
    implicit none

contains
    ! Lida com a discretização
    function fn(x0,h,n) result(res)
        real(p) :: x0
        real(p) :: h
        integer :: n

        real(p) :: res

        res = f(x0 + h*n)
    end function fn

    function find_h(N) result(res)
        integer :: N
        real(p) :: res
        res = (b-a)/N
    end function find_h

    function trapezio(N) result(res)
        real(p) :: res, h
        integer :: N, i
        real(p) :: x0

        res = 0
        h = find_h(N)
        x0 = a + h

        do i = 1, int(N/2)
            res = res + h/2 * (fn(x0,h,1) + 2*fn(x0,h,0) + fn(x0,h,-1))
            x0 = x0 + 2*h
        end do

    end function trapezio

    function simpson(N) result(res)
        real(p) :: res, h
        integer :: N, i
        real(p) :: x0

        res = 0.
        h = find_h(N)
        x0 = a + h

        do i = 1, int(N/2)
            res = res + h/3 * (fn(x0,h,1) + 4*fn(x0,h,0) + fn(x0,h,-1))
            x0 = x0 + 2*h
        end do
    end function simpson
    
    function bode(N) result(res)
        real(p) :: res, h
        integer :: N, i
        real(p) :: x0

        res = 0.
        h = (b-a)/N
        x0 = a

        do i = 1, int(N/4)
            res = res + 2*h/45 * (7*fn(x0,h,0) + 32*fn(x0,h,1) &
              & + 12*fn(x0,h,2) + 32*fn(x0,h,3) + 7*fn(x0,h,4))
            x0 = x0 + 4*h
        end do

    end function bode
end module integrals

module data_handler
    use parametros
    use integrals
    use func
    implicit none

    contains

    subroutine load_tab2_in(N_num, N_array)
        integer, intent(out) :: N_num
        integer, allocatable, intent(out) :: N_array(:)

        open(unit=1, file= "tab2_in.dat", status ="old", action = "read" )
        read(1, *) N_num

        allocate(N_array(N_num))
        read (1, *) N_array

        close(1)
    end subroutine load_tab2_in

    subroutine gen_out(N_num, N_array)
        integer, intent(in) :: N_num
        integer, allocatable, intent(in) :: N_array(:)

        real(p), allocatable :: matrix_out(:,:)
        integer :: i, min_ind
        real(p) :: true_val, min_val
        integer :: min_ind_vec(1)

        character(len = 50) :: label(5)
        label(1) = "N"
        label(2) = "h"
        label(3) = "Regra do trapézio"
        label(4) = "Regra de Simpson"
        label(5) = "Regra de Bode"
        
        ! Derivada simétrica de 5 pontos não é usada msm?

        allocate(matrix_out(N_num, 4))

        do i = 1, N_num
           matrix_out(i, 1) = find_h(N_array(i))
           matrix_out(i, 2) = trapezio(N_array(i))
           matrix_out(i, 3) = simpson(N_array(i))
           matrix_out(i, 4) = bode(N_array(i))
        end do

        true_val = int_f()

        ! Calcular diferença do valor teórico
        matrix_out = transpose(matrix_out) ! h_num x 6 -> 6 x h_num
        do i = 2, 4
            matrix_out(i,:) = abs(matrix_out(i,:) - true_val) 
        end do

        print *, "Melhores valores de N: "
        print *, ""

        do i = 2, 4
            min_val = minval(matrix_out(i, :))
            min_ind_vec = minloc(matrix_out(i, :))
            min_ind = min_ind_vec(1)

            print *, trim(label(i+1)), " :"
            write(*,"(A)",advance = "no") " N = "
            write(*,*) N_array(min_ind) 
            print *, "Diferença de ", min_val
            print *, ""
        end do

        matrix_out = transpose(matrix_out)

        open(unit=1, file="tab2_out.dat", status="replace", action = "write") 

        do i = 1, 5
            write(1, '(A, A)', advance='no') trim(label(i)), '   '
        end do
        write(1, *) 
            
        do i = 1, N_num
            write(1,"(I5)", advance = "no") N_array(i)
            write(1, *) matrix_out(i, :)
        end do

        close(1)

        deallocate(matrix_out)
    end subroutine gen_out

end module data_handler

program exer2
    use parametros
    use data_handler
    implicit none

    integer :: N_num
    integer, allocatable:: N_array(:)

    call load_tab2_in(N_num, N_array)
    call gen_out(N_num, N_array)

    deallocate(N_array)
    
end program exer2
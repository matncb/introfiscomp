! Modulo para o cálculo a partir das funções analíticas

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
    public :: f, df, ddf, dddf
contains

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
        res = 2*(1-x**2)/((1 + x**2)**2)
    end function ddf

    function dddf(x) result(res)
        real(p) :: x
        real(p) :: res
        res = (4*x**3-12*x)/((1 + x**2)**3)
    end function dddf

end module func

module derivatives
    use precision
    use func
    implicit none

    private
    public :: d_frente_2, d_tras_2, d_sim_3, d_sim_5, dd_sim_3, dd_sim_5, ddd_antisim_5

contains

    ! Lida com a discretização
    function fn(x0,h,n) result(res)
        real(p) :: x0
        real(p) :: h
        integer :: n

        real(p) :: res

        res = f(x0 + h*n)
    end function fn

    ! Derivadas pedidas no pdf
    ! Código: d_(frente/tras/sim)_(n_pontos)

    function d_frente_2(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 1) - fn(x0, h, 0))/h
    end function d_frente_2

    function d_tras_2(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 0) - fn(x0, h, -1))/h
    end function d_tras_2

    function d_sim_3(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 1) - fn(x0, h, -1))/(2*h)
    end function d_sim_3

    function d_sim_5(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (-fn(x0, h, 2) + 8*fn(x0, h, 1) -8*fn(x0, h, -1) + fn(x0, h, -2)) /(12*h)
    end function d_sim_5

    function dd_sim_3(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 1) -2*fn(x0, h, 0) + fn(x0, h, -1) )/(h**2)
    end function dd_sim_3

    function dd_sim_5(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (-fn(x0, h, 2) +16*fn(x0, h, 1) - 30*fn(x0, h, 0) &
             &+16*fn(x0,h,-1) - fn(x0,h, -2))/(12*h**2)
    end function dd_sim_5

    function ddd_antisim_5(x0,h) result(res)
        real(p) :: x0
        real(p) :: h

        real(p) :: res

        res = (fn(x0, h, 2) -2*fn(x0, h, 1) + 2*fn(x0, h, -1) - fn(x0,h,-2) )/(2*h**3)
    end function ddd_antisim_5 

end module derivatives

module data_handler
    use precision
    use derivatives
    use func
    implicit none

    contains

    subroutine load_tab1_in(h_num, h_array)
        integer, intent(out) :: h_num
        real(p), allocatable, intent(out) :: h_array(:)

        open(unit=1, file= "tab1_in.dat", status ="old", action = "read" )
        read(1, *) h_num

        allocate(h_array(h_num))
        read (1, *) h_array

        close(1)
    end subroutine load_tab1_in

    subroutine gen_out(x0, h_num, h_array)
        real(p), intent(in) :: x0
        integer, intent(in) :: h_num
        real(p), allocatable, intent(in) :: h_array(:)

        real(p), allocatable :: matrix_out(:,:)
        integer :: i, min_ind
        real(p) :: min_val
        integer :: min_ind_vec(1)

        character(len = 50) :: label(7)
        label(1) = "h"
        label(2) = "Derivada simétrica 3 pontos"
        label(3) = "Derivada para frente 2 pontos"
        label(4) = "Derivada para trás 2 pontos"
        label(5) = "Derivada segunda simétrica 3 pontos"
        label(6) = "Derivada segunda simétrica 5 pontos"
        label(7) = "Derivada terceira anti-simétrica 5 pontos"
        
        ! Derivada simétrica de 5 pontos não é usada msm?

        allocate(matrix_out(h_num, 6))

        do i = 1, h_num
           matrix_out(i, 1) = d_sim_3(x0,h_array(i))
           matrix_out(i, 2) = d_frente_2(x0,h_array(i))
           matrix_out(i, 3) = d_tras_2(x0,h_array(i))
           matrix_out(i, 4) = dd_sim_3(x0,h_array(i))
           matrix_out(i, 5) = dd_sim_5(x0,h_array(i))
           matrix_out(i, 6) = ddd_antisim_5(x0,h_array(i))
        end do

        ! Calcular diferença do valor teórico
        matrix_out = transpose(matrix_out) ! h_num x 6 -> 6 x h_num
        matrix_out(1, :) = abs(matrix_out(1,:) - df(x0))
        matrix_out(2, :) = abs(matrix_out(2,:) - df(x0))
        matrix_out(3, :) = abs(matrix_out(3,:) - df(x0))
        matrix_out(4, :) = abs(matrix_out(4,:) - ddf(x0))
        matrix_out(5, :) = abs(matrix_out(5,:) - ddf(x0))
        matrix_out(6, :) = abs(matrix_out(6,:) - dddf(x0))

        print *, "Melhores valores de h: "
        print *, ""

        do i = 1, 6
            min_val = minval(matrix_out(i, :))
            min_ind_vec = minloc(matrix_out(i, :))
            min_ind = min_ind_vec(1)

            print *, trim(label(i+1)), " :"
            write(*,"(A)",advance = "no") " h = "
            write(*,"(ES8.1)") h_array(min_ind) 
            print *, "Diferença de ", min_val
            print *, ""
        end do

        matrix_out = transpose(matrix_out)

        open(unit=1, file="tab1_out.dat", status="replace", action = "write") 

        do i = 1, 7
            write(1, '(A, A)', advance='no') trim(label(i)), '   '
        end do
        write(1, *) 
            
        do i = 1, h_num
            write(1,"(ES8.1)", advance = "no") h_array(i)
            write(1, *) matrix_out(i, :)
        end do

        close(1)

        deallocate(matrix_out)
    end subroutine gen_out

end module data_handler


program exer1
    use precision
    use data_handler
    implicit none

    real(p), parameter :: x0 = 0.5

    integer :: h_num
    real(p), allocatable:: h_array(:)

    call load_tab1_in(h_num, h_array)
    call gen_out(x0, h_num, h_array)

    deallocate(h_array)
    
end program exer1
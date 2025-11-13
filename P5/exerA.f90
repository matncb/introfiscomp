module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp

    private
    public :: p
end module precision

module parameters
    use precision
    implicit none

    ! Se os resultados estiverem estranhos para o ajuste, provavelmente o problema é o valor de max_iter_fit. Ele varia para cada situação. O enunciado não especifica como lidar com isso.
    integer, parameter :: max_iter_fit = 40  ! Ponto de saturação. Depois disso o fit para de funcionar. A soma continua funcinando. Depende do valor de r testado, é necessário olhar o gráfico
    integer, parameter :: max_iter_soma = 1000
    integer, parameter :: transient = 5  ! Iterações iniciais a descartar

    private
    public :: max_iter_fit, max_iter_soma, transient
end module parameters

module logistic_map
    use precision
    implicit none
    private
    public :: G, G_prime

contains
    function G(r, x) result(x_next)
        real(p), intent(in) :: r, x
        real(p) :: x_next
        x_next = r * x * (1.0_p - x)
    end function G

    function G_prime(r, x) result(g_p)
        real(p), intent(in) :: r, x
        real(p) :: g_p
        g_p = r * (1.0_p - 2.0_p * x)
    end function G_prime

end module logistic_map

program exerA
    use precision
    use parameters
    use logistic_map
    implicit none

    real(p) :: r, x0, epsilon
    
    integer :: i
    real(p) :: x_i, x_i_eps, d_i
    real(p) :: sum_ln_G_prime
    real(p) :: lambda_sum, lambda_fit
    real(p) :: intercept_fit ! Variável para o coeficiente linear
    
    ! Variáveis para regressão linear
    integer :: count_fit, count_sum
    real(p) :: sum_x, sum_y, sum_xx, sum_xy
    real(p) :: x_val, y_val
    
    read(*, *) x0
    read(*, *) r
    read(*, *) epsilon

    open(unit=1, file="distA_out.dat", status='replace', action='write')
    x_i = x0
    x_i_eps = x0 + epsilon

    sum_ln_G_prime = 0.0_p
    count_sum = 0.0_p

    
    ! regressão linear
    count_fit = 0
    sum_x = 0.0_p
    sum_y = 0.0_p
    sum_xx = 0.0_p
    sum_xy = 0.0_p

    do i = 1, max_iter_soma

        d_i = abs(x_i_eps - x_i)
        write(1, *) i, x_i, d_i
        
        if (i < max_iter_fit) then
            if (i > transient .and. d_i > 0.0_p) then
                x_val = real(i, p)
                y_val = log(d_i)
                
                sum_x = sum_x + x_val
                sum_y = sum_y + y_val
                sum_xx = sum_xx + x_val * x_val
                sum_xy = sum_xy + x_val * y_val
                count_fit = count_fit + 1
            end if
        end if
        
        if (i > transient) then
            if (abs(G_prime(r, x_i)) > 0.0_p) then
                sum_ln_G_prime = sum_ln_G_prime + log(abs(G_prime(r, x_i)))
                count_sum = count_sum + 1
            end if
        end if

        x_i = G(r, x_i)
        x_i_eps = G(r, x_i_eps)

    end do

    close(1)

    if (count_fit > 1) then
        lambda_fit = (count_fit * sum_xy - sum_x * sum_y) / &
                     (count_fit * sum_xx - sum_x * sum_x)
        
        intercept_fit = (sum_y / real(count_fit, p)) - &
                        lambda_fit * (sum_x / real(count_fit, p))
    else
        lambda_fit = 0.0_p
        intercept_fit = 0.0_p
    end if

    if (count_sum > 0) then
        lambda_sum = sum_ln_G_prime / real(count_sum, p)
    else
        lambda_sum = 0.0_p
    end if

    ! Saída
    write(*, *) 'Lambda Fit:  ', lambda_fit
    write(*, *) 'Lambda Soma:        ', lambda_sum
    !write(*,*) intercept_fit

end program exerA
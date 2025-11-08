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

    integer, parameter :: max_iter = 1000

    private
    public :: max_iter
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
    real(p) :: lambda_sum
    
    ! Variáveis da Regressão Linear
    integer :: n_discard, n_fit
    real(p) :: i_fit, log_di
    real(p) :: S_x, S_y, S_xx, S_xy
    real(p) :: lambda_fit

    read(*, *) x0
    read(*, *) r
    read(*, *) epsilon

    
    open(unit=1, file="distA_out.dat", status='replace', action='write')

    x_i = x0
    x_i_eps = x0 + epsilon
    sum_ln_G_prime = 0.0_p

    do i = 1, max_iter

        d_i = abs(x_i_eps - x_i)
        write(1, *) i, x_i, d_i
        
        sum_ln_G_prime = sum_ln_G_prime + log(abs(G_prime(r, x_i)))

        x_i = G(r, x_i)
        x_i_eps = G(r, x_i_eps)
        
    end do

    close(1)

    lambda_sum = sum_ln_G_prime / real(max_iter, p)

    ! Vamos descaetar 20% iniciais para o fit
    n_discard = max_iter / 5 
    n_fit = max_iter - n_discard

    ! Reinicializa as somas para a regressão
    S_x = 0.0_p
    S_y = 0.0_p
    S_xx = 0.0_p
    S_xy = 0.0_p

    ! Abre o arquivo novamente para leitura
    open(unit=1, file="distA_out.dat", status='old', action='read')
    
    ! Loop de leitura e cálculo da regressão
    do i = 1, max_iter
        read(1, *) i_fit, x_i, d_i ! i_fit e x_i não são usados aqui
        
        ! Descarta as primeiras iterações [cite: 79]
        if (i > n_discard) then
            log_di = log(d_i)
            
            ! y = log(d_i), x = i
            S_x  = S_x  + real(i, p)
            S_y  = S_y  + log_di
            S_xx = S_xx + real(i, p)**2
            S_xy = S_xy + real(i, p) * log_di
        end if
    end do
    
    close(1)
    
    ! Calcula o coeficiente angular (lambda) da regressão
    ! lambda = (N*Sxy - Sx*Sy) / (N*Sxx - Sx*Sx)
    lambda_fit = (real(n_fit, p) * S_xy - S_x * S_y) / &
                 (real(n_fit, p) * S_xx - S_x**2)
    
    write(*, *) ""
    write(*, *) 'Lambda Decaimento:   ', lambda_fit
    write(*, *) 'Lambda Soma: ', lambda_sum

end program exerA
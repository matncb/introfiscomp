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

    ! Valores aumentados para combater o "abrandamento crítico"
    integer, parameter :: n_transient = 100000  ! Iterações para descartar
    integer, parameter :: n_stable = 2000     ! Iterações para analisar
    
    real(p), parameter :: x0 = 0.2_p      ! Condição inicial
    
    ! Limite de Comparação automático baseado na raiz de epsilon
    real(p), parameter :: tol = sqrt(epsilon(1.0_p)) 

    private
    public :: n_transient, n_stable, x0, tol
end module parameters

module logistic_map
    use precision
    use parameters
    implicit none
    private
    public :: G, get_period, find_r_bifurcation

contains
    function G(r, x) result(x_next)
        real(p), intent(in) :: r, x
        real(p) :: x_next
        x_next = r * x * (1.0_p - x)
    end function G

    function get_period(r) result(period_count)
        real(p), intent(in) :: r
        integer :: period_count
        real(p) :: x
        real(p) :: xi_values(n_stable)
        integer :: i, j
        real(p) :: last_val, temp_val

        x = x0

        ! Fase transiente
        do i = 1, n_transient
            x = G(r, x)
        end do

        ! Fase estável
        do i = 1, n_stable
            x = G(r, x)
            xi_values(i) = x
        end do

        ! Ordena o array (Insertion Sort)
        do i = 2, n_stable
            temp_val = xi_values(i)
            j = i - 1
            do while (j >= 1 .and. xi_values(j) > temp_val)
                xi_values(j+1) = xi_values(j)
                j = j - 1
            end do
            xi_values(j+1) = temp_val
        end do

        ! Conta os pontos únicos
        period_count = 1
        last_val = xi_values(1)
        do i = 2, n_stable
            if (abs(xi_values(i) - last_val) > tol) then
                period_count = period_count + 1
                last_val = xi_values(i)
            end if
        end do

    end function get_period

    ! Busca Binária entre r_guess_low, r_guess_high até encontrar o ponto que o período aumenta de period_low para seu dobro
    function find_r_bifurcation(r_guess_low, r_guess_high, period_low) result(r_bif)
        real(p), intent(in) :: r_guess_low, r_guess_high
        integer, intent(in) :: period_low ! O período em r_guess_low
        real(p) :: r_bif

        real(p) :: r_low, r_high, r_mid
        integer :: period_mid

        r_low = r_guess_low
        r_high = r_guess_high

        ! Iterações de Bisseção 
        do
            r_mid = (r_low + r_high) / 2.0_p

            ! Critério de parada: Atingiu a precisão computacional máxima
            if (r_mid == r_low .or. r_mid == r_high) then
                r_bif = r_high
                exit
            end if

            period_mid = get_period(r_mid)

            if (period_mid == period_low) then
                ! a bifurcação está acima
                r_low = r_mid
            else
                ! a bifurcação está abaixo
                r_high = r_mid
            end if
        end do

    end function find_r_bifurcation

end module logistic_map

program delta_coef
    use precision
    use parameters
    use logistic_map
    implicit none

    real(p) :: r1, r2, r3, r4, r5, r6
    real(p), dimension(4) :: deltas ! Array para armazenar as estimativas
    integer, parameter :: n_deltas = 4
    real(p) :: delta_1, delta_2, delta_3, delta_4
    real(p), parameter :: delta_lit = 4.66920160910299067185320382_p
    real(p) :: delta_avg, delta_std_dev, delta_std_dev_mean
    real(p) :: variance
    logical :: is_compatible
    real(p) :: r_infinito, r_inf_incerteza ! Variáveis para r_infinito

    write(*, '(A)') "=========================================================="
    write(*, '(A)') " Constante de Feigenbaum (delta)"
    write(*, '(A)') "=========================================================="
    write(*, '(A, T30, F20.15)') "Tolerancia (tol) ", tol
    write(*, *) ""

    ! r1: 1->2 ciclo.
    r1 = find_r_bifurcation(r_guess_low=2.98_p, r_guess_high=3.02_p, period_low=1)
    write(*, '(A, T30, F20.15)') "r1 (1->2 ciclo):", r1

    ! r2: 2->4  entre 3.4 e 3.5.
    r2 = find_r_bifurcation(r_guess_low=3.4_p, r_guess_high=3.5_p, period_low=2)
    write(*, '(A, T30, F20.15)') "r2 (2->4 ciclo):", r2

    ! r3: 4->8 3.54 e 3.55.
    r3 = find_r_bifurcation(r_guess_low=3.54_p, r_guess_high=3.55_p, period_low=4)
    write(*, '(A, T30, F20.15)') "r3 (4->8 ciclo):", r3

    ! r4: 8->16 3.56 e 3.57.
    r4 = find_r_bifurcation(r_guess_low=3.56_p, r_guess_high=3.567_p, period_low=8)
    write(*, '(A, T30, F20.15)') "r4 (8->16 ciclo):", r4

    ! r5: 16->32
    r5 = find_r_bifurcation(r_guess_low=3.568_p, r_guess_high=3.569_p, period_low=16)
    write(*, '(A, T30, F20.15)') "r5 (16->32 ciclo):", r5

    ! r6: 32->64
    r6 = find_r_bifurcation(r_guess_low=3.5695_p, r_guess_high=3.5698_p, period_low=32)
    write(*, '(A, T30, F20.15)') "r6 (32->64 ciclo):", r6

    write(*, *) ""
    write(*, '(A)') "Valores de delta"
    write(*, *) ""

    ! Cálculo de delta (Equação 5) 
    delta_1 = (r2 - r1) / (r3 - r2)
    delta_2 = (r3 - r2) / (r4 - r3)
    delta_3 = (r4 - r3) / (r5 - r4) 
    delta_4 = (r5 - r4) / (r6 - r5) 

    deltas = [delta_1, delta_2, delta_3, delta_4]

    write(*, '(A, T30, F20.15)') "delta (estimativa 1):", delta_1
    write(*, '(A, T30, F20.15)') "delta (estimativa 2):", delta_2
    write(*, '(A, T30, F20.15)') "delta (estimativa 3):", delta_3
    write(*, '(A, T30, F20.15)') "delta (estimativa 4):", delta_4
    
    ! Cálculo da média e desvio padrão
    delta_avg = sum(deltas) / real(n_deltas, p)
    
    ! Desvio padrão da amostra (N-1)
    variance = sum( (deltas - delta_avg)**2 ) / real(n_deltas - 1, p)
    delta_std_dev = sqrt(variance)
    
    ! Desvio padrão da MÉDIA (Incerteza)
    delta_std_dev_mean = delta_std_dev / sqrt(real(n_deltas, p))

    write(*, *) ""
    write(*, '(A, T25, F20.15, A, F20.15)') "Resultado (media +/- incerteza):", &
         delta_avg, " +/- ", delta_std_dev_mean
    write(*, '(A, T25, F20.15)') "delta (literatura):", delta_lit
    write(*, *) ""

    ! Teste de compatibilidade
    is_compatible = (abs(delta_avg - delta_lit) <= delta_std_dev_mean)
    write(*, '(A, L1)') "Resultado compativel (dentro da incerteza)? ", is_compatible

    write(*, *) ""

    ! Previsão do ponto de caos (r_infinito)
    r_infinito = r6 + (r6 - r5) / (delta_avg - 1.0_p)
    
    ! Propagação da incerteza de delta_avg para r_infinito
    r_inf_incerteza = ( (r6 - r5) / (delta_avg - 1.0_p)**2 ) * delta_std_dev_mean

    write(*, '(A, T25, F20.15, A, F20.15)') "Previsao (r_infinito):", &
         r_infinito, " +/- ", r_inf_incerteza
    
    write(*, '(A)') "=========================================================="


end program delta_coef
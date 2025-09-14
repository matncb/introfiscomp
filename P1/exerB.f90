program exerB
    ! Preciso formatar o x para um decimal
    
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: max_iter = 100000

    integer, parameter :: x_array_size = 4
    real(sp), parameter :: x_sp_values(x_array_size) = [0.1_sp,0.2_sp,0.3_sp,0.4_sp]
    real(dp), parameter :: x_dp_values(x_array_size) = [0.1_dp,0.2_dp,0.3_dp,0.4_dp]

    ! Não sei o que fazer com a saída para simples ou dupla. Primeiro simples e depois dupla? Intercalado?
    ! Precisão negativa? Por definição do pdf sim, mas deveria tomar o módulo?

    call eval_sp_array_precision()
    call eval_dp_array_precision()
    print *, ""

    print *, "Como resposta geral, nas condições avaliadas, podemos aproximar logarítimos por séries."
    print *, "Entretanto, se x fosse maior que 1, a série não iria convergir."
    print *, "Para valores muito próximos de 1, o número de iterações até chegar na precisão aumenta drasticamente."
    print *, "Quanto menor for o valor de x, melhor o método de série funciona."

contains
    subroutine eval_sp_array_precision()
        integer :: i
        real(sp) :: precision

        print *, "PRECISAO SIMPLES"
        do i = 1, x_array_size
            precision = eval_ln_precision_sp(x_sp_values(i))
            write(*, '(F3.1)', advance='NO') x_sp_values(i)
            write(*, *) ' ', precision
        end do

    end subroutine eval_sp_array_precision

    subroutine eval_dp_array_precision()
        integer :: i
        real(dp) :: precision

        print *, "PRECISAO DUPLA"
        do i = 1, x_array_size
            precision = eval_ln_precision_dp(x_dp_values(i))
            write(*, '(F3.1)', advance='NO') x_dp_values(i)
            write(*, *) ' ', precision
        end do

    end subroutine eval_dp_array_precision

    function eval_ln_precision_sp(x) result(precision)
        real(sp), intent(in) :: x
        real(sp) :: precision
        real(sp) :: epsilon, epsilon_new
        real(sp) :: ln
        
        integer :: i
        i = 1
        ln = 0
        
        do
            ! Verificação para não entrar em loop infinito
            if (i > max_iter) then
                print *, "Exit by max iter"
                exit
            end if

            epsilon_new = (-1)**(i+1) *(x**i)/(i*1._sp)

            if (ln == (ln + epsilon_new)) then
                exit
            else
                ln = ln + epsilon_new
                epsilon = epsilon_new
            end if

            i = i + 1
        end do
        precision = epsilon/ln

    end function eval_ln_precision_sp

    function eval_ln_precision_dp(x) result(precision)
        real(dp), intent(in) :: x
        real(dp) :: precision
        real(dp) :: epsilon, epsilon_new
        real(dp) :: ln
        
        integer :: i
        i = 1
        ln = 0
        
        do
            ! Verificação para não entrar em loop infinito
            if (i > max_iter) then
                print *, "Exit by max iter"
                exit
            end if

            epsilon_new = (-1)**(i+1) *(x**i)/(i*1._dp)

            if (ln == (ln + epsilon_new)) then
                exit
            else
                ln = ln + epsilon_new
                epsilon = epsilon_new
            end if

            i = i + 1
        end do
        
        precision = epsilon/ln

    end function eval_ln_precision_dp

end program exerB
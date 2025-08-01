program float_precision
    implicit none

    ! Obs: atribui na declaração das variáveis nesse código pq era tudo estático, então não teria teria problema
    ! Não esquecer o SAVE implícito!!!

    ! Para usar uma subrotina, escraval call subrotina()
    call float_easy()
    print *, ''
    call float_right()
    print *, ''
    call var()

    ! No fim das contas, a opção mais fácil é usar -fdefault-real-8 no gfortran, que alterará tudo para double sem precisar se preocupar
    ! Isso promove double para 16. Use -fdefault-double-8 par manter ou alterar manualmente

contains
    ! Declarando uma subrotina --> Não há valor retornado

    subroutine float_easy()
        ! Especificar tamanho do float
        real :: r =1.0
        real(kind = 8) :: x = 1.0
        real(8) :: y = 1.0

        print *, 'Jeito Fácil'
        print *, 'Float', r
        print *, 'Double', x
        print *, 'Double', y
        
    end subroutine float_easy

    subroutine float_right()
        ! Dependendo do compilador, float_easy pode não ser correto (usualmente é)
        ! O jeito oficialmente correto é:

        ! P = 10 número de dígitos R = 30 potência de 10
        integer, parameter :: dp = selected_real_kind(10,30)
        integer, parameter :: sp = selected_real_kind(5,15)
        integer, parameter :: dp_alt = kind(0.d0)

        real(dp) :: z = 1.0
        real(dp_alt) :: w = 1.0

        print *, 'Jeito Correto'
        print *, 'sp', sp
        print *, 'dp', dp
        print *, 'dp_alt', dp_alt
        print *, 'Double', z
        print *, 'Double_alt', w

        print *, ''
        print *, 'Observação: '
        print *, 'Notação científica', 1.0e3
        print *, 'Notação científica em double', 1.0d3
        print *, 'Zero em double', 0.d0

    end subroutine float_right

    subroutine var()
        integer, parameter :: dp = selected_real_kind(10,30)
        integer, parameter :: sp = selected_real_kind(5,15)

        real(sp) :: pi_single
        real(dp) :: pi_double

        print *, 'Atenção às variáveis'

        ! Por padrão, teremos números em single
        pi_single = 4.0 * atan(1.0)
        print *, 'Single e single (padrão)', pi_single
        
        ! Podemos forçar o single
        pi_single = 4.0_sp * atan(1.0_sp)
        print *, 'Single e single (forçado)', pi_single

        ! Podemos forçar o double, resultando num truncamento (gera um warning)
        pi_single = 4.0_dp * atan(1.0_dp)
        print *, 'Single e double ---> Truncamento', pi_single

        ! Usando o pi_double
        pi_double = 4.0 * atan(1.0)
        print *, 'Double e single (padrão) ---> Dígitos aleatórios', pi_double

        ! Podemos forçar o single
        pi_double = 4.0_sp * atan(1.0_sp)
        print *, 'Double e single (forçado) ---> Dígitos aleatórios', pi_double

        ! Podemos forçar o double
        pi_double = 4.0_dp * atan(1.0_dp)
        print *, 'Single e double ---> pi em double', pi_double


    end subroutine var

end program float_precision
program variables
    ! Remove declaração implícita
    ! Variáveis implicitamente tipadas de acordo com a letra com a qual começam

    implicit none

    ! Declaração
    call declare()
    print *, ''

    ! Atribuição na declaração (Cuidado !!!)

    call madness()
    call madness()
    call madness()

    call correct()
    call correct()
    call correct()

    print *, ''

    ! Podemos adicionar atributos

    call attributes(1.0)

contains
    subroutine declare()
        ! Para declarar variáveis, use
        ! <variable_type> :: <variable_name>, <variable_name>, ...
        ! Fortran é uma linguagem estaticamente tipada , o que significa que o tipo de cada variável é fixo quando o programa é compilado
        ! Os tipos de variáveis não podem mudar enquanto o programa está em execução.

        ! Cuidado para não nomear essas variáveis com um nome já usado no escopo global

        integer :: amount
        real :: pi, e ! Duas variáveis reais declaradas
        complex :: frequency
        character :: initial
        logical :: isOkay

        ! Depois de declarar uma variável, podemos atribuir e reatribuir valores a ela usando o operador de atribuição =.

        amount = 10
        pi = 3.1415927
        e = 2.71
        frequency = (1.0, -0.5) ! (real, imaginário)
        initial = 'A'
        isOkay = .false. ! ou .true.

        ! Cuidado com diferenças como 1 (inteiro) ou 1.0 (real) ou 1. (real)


        ! Os caracteres são colocados entre aspas simples (') ou duplas (").

        ! Visualizar resultados

        print *, 'The value of amount (integer) is: ', amount
        print *, 'The value of pi (real) is: ', pi
        print *, 'The value of e (real) is: ', e
        print *, 'The value of frequency (complex) is: ', frequency
        print *, 'The value of initial (character) is: ', initial
        print *, 'The value of isOkay (logical) is: ', isOkay

    end subroutine declare

    subroutine madness()
        ! Atribuições na declaração !!!!!

        ! No código principal não há problema, já que não há risco de reinicialização
        ! Ao usar em funções ou subrotinas, pode levar a resultados malucos, porque a variável será salva, e não reinicializada, como esperado

        implicit none
        integer :: x = 0 ! Equivalente a integer, save :: x = 0
        x = x + 1

        print *, 'Observe a loucura', x

    end subroutine madness

    subroutine correct()
        ! Para resolver o problema anterior basta declarar e depois atribuir
        implicit none
        integer :: x
        x = 0
        x = x + 1

        print *, 'Sem loucura', x

    end subroutine correct

    subroutine attributes(x_in)
        implicit none

        ! Valores constantes podem ser declarados com o parâmetro parameter
        real, parameter :: pi = 4.*atan(1.) ! Como é imutável, posso, de forma segura, já atribuir

        ! Use intent(in) para dizer que x_in é lido, um valor de entrada --> É imutável
        ! Tentativa de modificação gerará erro
        real, intent(in) :: x_in

    end subroutine attributes
end program variables
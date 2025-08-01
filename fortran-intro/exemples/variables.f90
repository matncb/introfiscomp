program variables
    ! Remove declaração implícita
    ! Variáveis implicitamente tipadas de acordo com a letra com a qual começam

    implicit none

    ! Declaração
    call declare()
    print *, ''

    ! Atribuição na declaração (Cuidado !!!)
    print *, 'Atenção ao atributo SAVE implícito!!'
    call madness()
    call madness()
    call madness()

    call correct()
    call correct()
    call correct()

    print *, ''

    ! Podemos adicionar atributos

    print *, 'Lidando com atributos'
    
    print *, ''
    call attributes()

contains
    subroutine declare()
        ! Para declarar variáveis, use
        ! <variable_type> :: <variable_name>, <variable_name>, ...
        ! Fortran é uma linguagem estaticamente tipada , o que significa que o tipo de cada variável é fixo quando o programa é compilado
        ! Os tipos de variáveis não podem mudar enquanto o programa está em execução.

        ! Cuidado para não nomear essas variáveis com um nome já usado no escopo global

        integer :: amount ! Inteiro (4 bytes por padrão)
        real :: pi, e ! Duas variáveis reais declaradas ! Ponto flutuante (4 bytes por padrão)
        complex :: frequency ! Complexo (8 bytes: 4+4)
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
        print *, 'Principais Tipos'
        print *, 'Inteiro ', amount
        print *, 'Real/Ponto Flutuante', pi
        print *, 'Real/Ponto Flutuante', e
        print *, 'Complexo', frequency
        print *, 'Caractere', initial
        print *, 'Lógico/Booleano', isOkay

    end subroutine declare

    subroutine madness()
        ! Atribuições na declaração !!!!!

        ! No código principal não há problema, já que não há risco de reinicialização
        ! Ao usar em funções ou subrotinas, pode levar a resultados malucos, porque a variável será salva, e não reinicializada, como esperado

        integer :: x = 0 ! Equivalente a integer, save :: x = 0
        x = x + 1

        print *, 'Observe a loucura', x

    end subroutine madness

    subroutine correct()
        ! Para resolver o problema anterior basta declarar e depois atribuir
        integer :: x
        x = 0
        x = x + 1

        print *, 'Sem loucura', x

    end subroutine correct

    subroutine attributes()
        ! PARAMETER. Valores constantes podem ser declarados com o atributo parameter
        real, parameter :: pi = 4.*atan(1.) ! Como é imutável, posso, de forma segura, já atribuir

        ! SAVE , de preferência declarado explicitamente para evitar confusão, salva a variável e evita reinicialização ao rodar uma subrotina mais de uma vez
        real, save :: cont = 1. ! Funciona como um contador persistente, podendo ser útil se essa é a intenção

        ! TARGET - Permite apontamento
        integer, target :: target_var = 1
        
        ! POINTER - Ponteiros
        integer, pointer :: ptr_var
        ptr_var => target_var ! Atrela as referências (endereços de memória)

        print *, 'Parâmetro', pi
        print *, 'Save explícito', cont
        print *, ''
        print *, 'Ponteiros'
        print *, 'Valor original da variável original:', target_var
        ptr_var = 100 ! Modificar ponteiro
        print *, 'Valor após modificar ponteiro ponteiro:', target_var
        target_var = 200 ! Modificar variável original
        print *, 'Valor da variável', target_var
        print*, 'Valor do ponteiro', ptr_var

        ! Obs: existe o atributo volatile que especifica que a variável pode ser alterada por meios externos ao programa

    end subroutine attributes
end program variables
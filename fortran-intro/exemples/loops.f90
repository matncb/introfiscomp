program loops
    implicit none

    ! Sintaxe do (equivalente ao for):
    ! do var = inicio, fim, incremento
    !    comandos
    ! end do

    ! A variável var tem que ser declarada antes
    ! Se o incremento não for passado, por padrão será 1
    ! [início, fim] formam intervalo fechado
    ! Não alterar contador (var) dentro do loop

    ! É possível escrever de forma implícita
    !(expressão, contador = inicio, fim, passo)

    ! Sintaxe do while (equivalente ao comum while)
    ! do while (condição)
    !   comandos
    ! end do

    ! Loops infinitos podem ser feitos usando apenas do (sem while ou var)
    ! do
    !   comandos
    ! end do

    ! O comando exit (equivalente ao break) encerra qualquer loop
    ! O comando cycle (equivalente ao continue) pula para a próxima iteração de qualquer loop

    ! Loops podem ser aninhados normalmente

    ! É possível usar tags para fazer um loop e nos comandos exit e cycle

    ! Tags [nome]: ! do ...
    !    ...
    !   exit [nome]   ! ou cycle [nome]
    !    ...
    ! end do [nome]

    ! Muito útil para loops aninhados
    ! Sem tags, ao usar exit e cycle, sempre estará implícito que esses comandos atuam sobre o loop mais interno
    ! Para deixar isso claro ou para fazer isso com loops externos, as tags são úteis

    integer :: i, j

    ! Incremento implícito
    do i = 1,3
        print *, i
    end do

    print *, ''

    ! Incremento explícito
    do i = 1,5,2
        print *, i
    end do

    print *, ''

    ! Escrever de forma implícita
    print *, (i, i = 1,3)
    print *, ''

    ! While
    i = 0 ! Necessário atribuir antes
    do while (i < 5)
        print *, i
        i = i + 1
    end do

    print *, ''

    ! Usando um loop infinito
    i = 0 ! Reiniciando a variável
    do 
        if (i >= 5) exit
        print *, i
        i = i + 1
    end do

    print *, ''

    ! Testando exit e cyle
    do i = 1, 100
        ! Para se for 20
        if (i == 20) exit

        ! Se não for 10 ou 30 segue em frente
        if ((i /= 10) .and. (i /= 30)) cycle
        
        print *, i ! Printar os que passam (apenas o 10)
    end do

    print *, ''

    ! Aninhando um loop
    do i = 1, 3
        do j = 1, 2
            print *, "i=", i, "j=", j
        end do
    end do

    print *, ''

    ! Tags
    rows: do i = 1, 4
        cols: do j = 1, 3
            if (j == 2) cycle rows  ! Pula para a próxima linha
            print *, "(", i, ",", j, ")"
        end do cols
    end do rows

end program loops

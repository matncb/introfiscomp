program conditionals
    implicit none

    ! Operadores

    ! == ou .eq. equal
    ! \= ou .ne. not equal (usualmente != na maior parte das linguagens) 
    ! > ou .gt. greather than
    ! < ou .lt. less than
    ! >= ou .ge. greather or equal
    ! <= ou .le. less or equal

    ! .and. TRUE if both left and right operands are TRUE
    ! .or. TRUE if either left or right or both operands are TRUE
    ! .not. TRUE if right operand is FALSE
    ! .eqv. TRUE if left operand has same logical value as right operand
    ! .neqv. TRUE if left operand has the opposite logical value as right operand

    ! Uma dessas operações sempre retorna um valor lógico .true. ou .false.

    ! Duas opções de sintaxe

    ! if (condição) comando

    ! if (condição1) then
    !   Bloco 1
    ! else if (condição2) then
    !   Bloco 2
    ! else
    !   Bloco padrão
    ! end if

    ! Também temos o select case (análogo ao switch case)
    ! Usualmente mais rápido

    ! select case (variável)
    !   case (valor1)
    !       ! Bloco 1
    !   case (valor2:valor3)   ! Faixa de valores
    !       ! Bloco 2
    !   case (valor4, valor5)  ! Múltiplos valores
    !       ! Bloco 3
    !   case default
    !       ! Bloco padrão
    ! end select

    ! Condicionais também podem ser aninhados normalmente

    ! if (condição1)
    !   if (condição2)
    !        comandos
    !   end if
    !   mais comandos
    ! end if

    ! Declarando variáveis 

    integer :: idade, codigo_status
    real :: x, y, nota

    x = 1.
    y = -1.

    ! Operadores retornam .true. ou .false.
    print * , (x>y)
    print *, (x<y)
    print *, ((x>y) .and. (x<y)) ! Sempre .false.
    print *, .not. ((x>y) .and. (x<y)) ! Negação

    ! Condicionais executam seus blocos se .true. e não executam se .false.
    if (.true.) print *, 'Condicional'

    ! Exemplo na outra sintaxe
    if ((x >= 0) .and. (y<=0)) then
        print *, 'x positivo e y negativo'
    end if

    ! if e else
    idade = 19
    if (idade >= 18) then
        print *, "Maior de idade"
    else
        print *, "Menor de idade"
    end if

    ! if, else if e else
    nota = 9.5
    if (nota >= 9.0) then
        print *, "Excelente!"
    else if (nota >= 7.0) then
        print *, "Bom"
    else if (nota >= 5.0) then
        print *, "Regular"
    else
        print *, "Reprovado"
    end if

    ! Select case
    codigo_status = 404
    select case (codigo_status)
        case (200)
            print *, "Sucesso!"
        case (404)
            print *, "Não encontrado"
        case (500:599)
            print *, "Erro do servidor"
        case default
            print *, "Código desconhecido"
    end select

end program conditionals
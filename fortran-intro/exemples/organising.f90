program organising
    implicit none

    ! O código pode ser oranizado usando subroutine ou function
    ! Uma subrotina não retorna nenhum valor
    ! Uma função sempre retorna um valor

    ! Funções devem ser usadas apenas para retornar um valor (uso em expressões)
    ! Subrotinas devem ser usadas para modificar variáveis e não em expressões

    ! Funções recursivas precisam ser sinalizadas com recursive antes

    ! Para chamar uma função use apenas function()
    ! Para chamar uma subrotina use call subroutine()

    ! As variáveis passadas são sempre referências, exceto ao passar números, expressões ou slicing de arrays (Porque não existe uma referência única para isso)
    ! Modificar a variável passada irá modificar a variável original !!!!

    ! Para fazer uma cópia, no fortran moderno, podemos usar o atributo value

    ! Sintaxe
    ! function f() result(var)
    ! function f() equivale a function f() result(f)

    real :: x = 1, y = 2
    real :: x1 = 1, x2 =1., x3 = 1.

    print *, 'Antes da troca', x, y
    call swap (x, y) ! Vai modificar x e y originais
    print *, 'Depois da troca da troca', x, y
    
    print *, ''
    print *, 'Antes do incremento', y
    call safe_increment(y) ! Não vai modificar porque usei atributo value
    print *, 'Depois do incremento (com atributo value)', y

    print *, ''
    call inout_attributes(x1, x2, x3)

    print *, ''
    print *, 'Área de um círculo de raio 2', circle_area(2.)

    print *, ''
    print *, 'Fibonnaci de 10', fibonacci(10)

contains ! Em baixo de contains, coloque subrotinas e funções
    subroutine swap(a, b)
        implicit none
        real, intent(inout) :: a, b ! Leitura e escrita permitidas
        real :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap

    subroutine safe_increment(a)
        implicit none
        real, value :: a  ! Cópia local
        a = a + 1  ! Não afeta a original
    end subroutine

    subroutine inout_attributes(x_in, x_out, x_inout)
        implicit none
        ! Por padrão, se não usarmos intent, teremos, implicitamente intent(inout)

        ! Use intent(in) para dizer que x_in é lido, um valor de entrada --> É imutável
        ! Tentativa de modificação gerará erro
        real, intent(in) :: x_in

        ! Use intent(out) para dizer que x_out deve ser modificado, um valor de saída --> É mutável
        ! Tentativa de leitura gerará resultados incorretos
        real, intent(out) :: x_out

        ! Use intent(inout) para dizer que x_inout é valor de entrada e saída
        ! O valor pode ser lido e modificado
        real, intent(inout) :: x_inout

        ! Posso printar
        print *, 'Leitura dos valores de entrada'
        print *, x_in
        print *, x_inout
        print *, x_out ! Não gera erro mas é incorreto !!!

        ! Posso atribuir a x_out e x_inout

        x_out = 2
        x_inout  = 2

        ! Posso printar
        print *, ''
        print *, 'Leitura dos valores de saída'
        print *, x_in
        print *, x_inout
        print *, x_out ! Agora que já atribui, posso printar sem problemas

    end subroutine inout_attributes

    function circle_area(radius) result(area) ! Podemos omitir result(nome), de forma que fique implícito que o nome é o mesmo nome da função
        real, intent(in) :: radius
        real :: area
        real, parameter :: PI = 3.14159
        area = PI * radius**2
    end function circle_area

    recursive function fibonacci(n) result(res) ! Recursive para sinalizar função recursiva
        integer, intent(in) :: n
        integer :: res
        if (n <= 1) then
            res = n
        else
            res = fibonacci(n-1) + fibonacci(n-2)
        end if
    end function fibonacci

end program organising
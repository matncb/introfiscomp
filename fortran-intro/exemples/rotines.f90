program rotines
    implicit none

    ! O código pode ser organizado usando subroutine ou function
    ! Uma subrotina não retorna nenhum valor
    ! Uma função sempre retorna um valor

    ! Funções devem ser usadas para retornar um valor (uso em expressões), mas podem ter efeitos colaterais (não é boa prática)
    ! Subrotinas devem ser usadas para modificar variáveis e não em expressões

    ! Funções recursivas precisam ser sinalizadas com recursive antes
    ! Funções puras precisam ser sinalizadas com pure antes

    ! PURE --> só aceitam intent(in) sem inout ou out, não alteram variáveis globais, não pode usar atributo save, só chamam funções puras
    ! Útil para otimização e paralelização

    ! Para chamar uma função use apenas function(argumentos)
    ! Para chamar uma subrotina use call subroutine(argumentos)

    ! Sempre passe argumentos da seguinte forma function(obrigatorio, opcional1, opcional2, ...)
    ! Argumentos opcionais são sinalizados com o atributo optional
    ! A função present(variavel) verifica se o argumento funcional foi passado ou não
    ! Pode omitir argumentos opicionais intermediários usando os nomes dos argumentos
    ! Argumentos opcionais não são permitidos em funções intrínsecas do Fortran
    ! Argumentos opcionais tem que ter intent explícito e não podem ser pointer ou target

    ! As variáveis passadas são sempre referências, exceto ao passar números, expressões, slicing de arrays, componentes de tipos derivados ou strings com concatenação (Porque não existe uma referência única para isso)
    ! Modificar a variável passada irá modificar a variável original !!!!

    ! Para fazer uma cópia, no fortran moderno, podemos usar o atributo value

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
    call arg(1, c = 3)

    print *, ''
    print *, 'Área de um círculo de raio 2', circle_area(2.)
    print *, 'Área de um círculo de raio 2', circle_area2(2.)
    print *, 'Área de um círculo de raio 2', circle_area3(2.)
    print *, 'Área de um círculo de raio 2', circle_area_pure(2.)

    print *, ''
    print *, 'Fatorial de 10', factorial(10)

contains ! Em baixo de contains, coloque subrotinas e funções
    ! Sequência de exemplos com as mais variadas sintaxes possíveis

    ! Sintaxe do end:
    ! No fortran antigo era apenas aceitável escrever end no final, sem mais nada
    ! Atualmente, é considerado boa prática escrever end subroutine nome

    subroutine swap(a, b)
        real, intent(inout) :: a, b ! Leitura e escrita permitidas
        real :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap

    subroutine safe_increment(a)
        real, value :: a  ! Cópia local
        a = a + 1  ! Não afeta a original
    end subroutine

    subroutine inout_attributes(x_in, x_out, x_inout)
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

    subroutine arg(a,b,c)
        integer, intent(in) :: a ! Obrigatórios
        integer, intent(in), optional :: b, c ! Opicionais

        print *, 'Argumento obrigatório', a
        
        if (present(b)) then ! Verificar se o argumento foi passado
            print * , 'Argumento opcional', b
        end if

        if (present(c)) then  ! Verificar se o argumento foi passado
            print * , 'Argumento opcional', c
        end if

    end subroutine arg

    function circle_area(radius) result(area) ! Podemos omitir result(nome), de forma que fique implícito que o nome é o mesmo nome da função
        real, intent(in) :: radius
        real :: area
        real, parameter :: PI = 3.14159
        area = PI * radius**2
    end function circle_area

    function circle_area2(radius)  ! Forma alternativa que omite o result, assumindo result(nome da função) implicitamente
        real, intent(in) :: radius
        real :: circle_area2
        real, parameter :: PI = 3.14159
        circle_area2 = PI * radius**2
    end function circle_area2

    real function circle_area3(radius) ! Forma alternativa omitindo result ! Usar real, integer, etc antes da função só funciona se o result for omitido
        real, intent(in) :: radius     
        ! real :: circle_area3 implícito devido ao real na frente
        real, parameter :: PI = 3.14159
        circle_area3 = PI * radius**2
    end function circle_area3

    pure function circle_area_pure(radius) result(area) ! Pure na frente para declarar função pura
        real, intent(in) :: radius                      ! É permitido omitir o result
        real :: area
        real, parameter :: PI = 3.1415926535
        area = PI * radius**2
    end function circle_area_pure

    recursive function factorial(n) result(res)   ! Recursive para sinalizar função recursiva
        integer, intent(in) :: n                  ! Não posso omitir result (no Fortran 90 --> Versões mais atuais aceitam)
        integer :: res  ! Variável de resultado
        
        if (n <= 1) then
            res = 1
        else
            res = n * factorial(n-1)
        end if
    end function factorial

end program rotines
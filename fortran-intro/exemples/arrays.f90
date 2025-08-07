program arrays
    implicit none

    ! Para declarar arrays estáticos, existem 3 opções
    ! Usar o atributo dimention(n, m, ...)
    ! tipo :: nome(n,m, ...) --> análogo a C
    ! tipo :: nome(a:b, c:d, ...) --> limites de índices customizáveis (intervalos fechados)

    ! Antes de inicializar os valores, o array contém apenas lixo, números aleatórios
    ! Por padrão, a não ser que especificado, um array em fortran começa do 1, não do zero
    ! Atribuir para um índice que não existe gera apenas um warning, e não um erro

    integer, dimension(2) :: arr1       ! Índices 1 a 2
    integer :: arr2(2) ! Outra forma de declarar
    integer :: arr3(0:1) ! Índices customizáveis --> começar do zero
    integer :: teste(5)
    integer :: teste2(5) = [1,2,3,4,5] ! Cuidado com o SAVE implícito !!!
    real :: teste_float(5) = [1.,1.,1.,1.,1.]
    integer :: i

    ! Vamos usar para multiplicação matricial
    real :: A(3,2), B(2,3), C(3,3)

    ! Vamos usar para slicing e operações
    real :: arr(10) = [(i, i=1,10)]
    real :: matrix(4,4)
    integer :: indices(3) = [2,5,9]
    logical :: mask(10)

    ! Caracteres
    character(len=5):: string = 'Texto' ! len é usado para declarar máximo tamanho de uma string
    character(len=6) :: string_array(2) = ['Texto ','Textos'] ! Todo item deve ter o mesmo tamanho --> Preencher com espaços

    ! Para o arr1
    print *, 'Valores aleatórios'
    print *, arr1
    arr1(1) = 1
    arr1(2) = 2
    print *, 'Atribuindo'
    print *, arr1

    print*, ''

    ! Vamos testar dos outros jeitos
    arr2(1) = 1
    arr2(2) = 2
    print *, arr2

    arr3(0) = 1
    arr3(1) = 2
    print *, arr3

    print *, ''

    ! Além de atribuir item a item, podemos fazer atribuição direta
    ! Posso também usar um loop implícito

    teste = [1,2,3,4,5] ! Tudo de uma vez
    print *, teste
    
    teste = [(i, i=1,5)] ! De forma implícita
    print *, teste

    print *, ''

    ! Caracteres
    print *, string
    print *, string_array
    print *, ''

    !! OPERAÇÕES
    
    ! Elemento a elemento --> 4 operações (+,-,*,/) realizadas um a um
    teste = teste + teste2
    print *, teste
    teste = teste/teste
    print *, teste

    ! Por padrão, funções e operações sempre vão ser aplicadas elemento a elemento
    teste_float = sin(teste_float)
    print *, teste_float

    print *, ''

    ! Multiplicação matricial
    A = reshape([1,2,3,4,5,6], [3,2]) ! Coloca o primeiro array no formato do segundo
    B = reshape([1,2,3,4,5,6], [2,3]) 
    C = matmul(A, B)  ! Multiplicação matricial
    print *, C

    print *, ''

    ! Fatias básicas
    print *, arr(3:7)     ! Elementos 3 a 7 (intervalo fechado)
    print *, arr(1:10:2)  ! Ímpares: 1,3,5,7,9

    ! Com passo negativo
    print *, arr(10:1:-1) ! Inverte o array

    ! Em múltiplas dimensões
    matrix(:,2) = 0.0     ! Zera segunda coluna
    matrix(2:3,3:4) = 1.0 ! Submatriz 2x2

    ! Indexação com vetor
    print *, arr(indices) ! Elementos 2,5,9

    ! Máscaras lógicas
    mask = arr > 5.0
    print *, pack(arr, mask) ! Valores > 5

end program arrays
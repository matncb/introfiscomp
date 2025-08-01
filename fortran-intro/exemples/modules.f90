! Declare um módulo da mesma forma que um programa, substituindo program por module

module math_utils
    implicit none ! É necessário dizer implicit none no escopo do módulo também

    ! Depois disso, podemos dizer o que é público ou privado
    ! Omitir public e private resulta em tudo público
    ! É considerado boa prática dizer explicitamente

    ! Posso escrever tanto public qnt private antes, não tem ordem de preferência
    private           ! Tudo privado por padrão
    public :: PI, add, factorial, print_internal_var  ! Itens públicos

    real, parameter :: PI = 3.1415926535 ! Variável pública
    real, private :: internal_var = 1.5  ! Variável privada --> Pode ser usada dentro do módulo

    ! Posso ter acesso à variáveis privadas no código principal se, por exemplo, eu retornar ela em uma função

    ! Não é permitido ter nenhum comando, nem mesmo atribuições dentro de um módulo
    ! TODA VARIÁVEL SEMPRE TERA O ATRIBUTO SAVE !!!!!!
    ! É recomendado colocar apenas parâmetros nesse escopo
    ! Caso contrátrio, explicite o save para não gerar efeitos malucos sem perceber

contains ! Declarar as funções em baixo
    function add(a, b) result(res)
        real, intent(in) :: a, b
        real :: res
        res = a + b
    end function add
  
    recursive function factorial(n) result(res)
        integer, intent(in) :: n
        integer :: res
        if (n <= 1) then
            res = 1
        else
            res = n * factorial(n-1)
        end if
    end function factorial

    subroutine print_internal_var()
        print *, 'Variável Interna', internal_var
    end subroutine print_internal_var

end module math_utils

module counter
  implicit none
  private
  public :: increment, get_count
  
  integer, save :: count = 0  ! save mantém o valor entre chamadas

  ! Atenção ao SAVE !!!!

  ! Se eu carregar o módulo em diferentes outros módulos e usar separadamente, o valor de count será o mesmo em todo lugar
  ! Isso quer dizer que count será uma variável totalmente global, maior que o próprio módulo n
  
contains
  subroutine increment()
    count = count + 1
  end subroutine
  
  integer function get_count()
    get_count = count
  end function

end module counter


program modules
    ! Para usar o módulo basta usar o comando use
    ! Se only for omitido o programa vai carregar tudo
    ! É considerado boa prática deixar explícito

    use math_utils, only: PI, add, print_internal_var

    implicit none ! Depois de carregar módulos
    real :: result

    ! Posso usar todos os itens normalmente agora

    result = add(1.5, 2.)
    
    call print_internal_var()
    print *, "Resultado:", result
    print *, "Valor de PI:", PI

end program modules
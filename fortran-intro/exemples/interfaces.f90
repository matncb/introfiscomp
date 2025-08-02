! SOBRECARGA: É a capacidade de ter várias funções/subrotinas com o mesmo nome, mas com parâmetros diferentes
! O compilador escolhe a versão correta baseada nos tipos dos argumentos --> Devem ser diferentes!!

module smart_printer
  implicit none
  private
  public :: print_value

  ! Define interface
  interface print_value
    ! Sintaxe: module procedure nome da função/subrotina
    ! Como cada função vai tem certo tipo de argumento, o compilador escolhe qual usar com base nisso
    module procedure print_logical
    module procedure print_integer
    module procedure print_real
    module procedure print_complex
  end interface

! A interface é definida antes do contains, podendo estar tanto em módulos quando no programa principal

contains ! Declarar todas as funções/subrotinas usadas na interface

  subroutine print_logical(val)
    logical, intent(in) :: val
    print *, "Valor lógico: ", val
  end subroutine

  subroutine print_integer(val)
    integer, intent(in) :: val
    print *, "Valor inteiro: ", val
  end subroutine

  subroutine print_real(val)
    real, intent(in) :: val
    print *, "Valor real: ", val
  end subroutine

  subroutine print_complex(val)
    complex, intent(in) :: val
    print *, "Valor complexo: (", real(val), ",", aimag(val), ")"
  end subroutine
end module

program demo
  use smart_printer, only: print_value
  implicit none
  
  call print_value(.true.)       ! lógico
  call print_value(42)           ! inteiro
  call print_value(3.14)         ! real
  call print_value((1.0, -2.0))  ! complexo
end program
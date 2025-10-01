module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão

    private
    public :: sp, dp, p
end module precision


module euler
    use precision
    implicit none
    
contains

end module euler



program exer1
    implicit none

end program exer1
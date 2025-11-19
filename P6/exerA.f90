module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp

    private
    public :: p
end module precision

module parameters
    use precision
    implicit none

    private
    public :: 
end module parameters

module solver
    use precision
    implicit none
contains

end module solver

program exerA
    use precision
    use parameters
    use solver
    implicit none


end program exerA
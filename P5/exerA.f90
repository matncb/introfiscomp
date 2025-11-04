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

    integer, parameter :: max_iter = 1000

    private
    public :: max_iter
end module parameters

module logistic_map
    use precision
    implicit none

contains


end module logistic_map

program exerA
    use precision
    use parameters
    use logistic_map


end program exerA
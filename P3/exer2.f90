module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão
    integer, parameter :: max_iter = 10000
    private
    public :: sp, dp, p, max_iter
end module precision

module parameters
    use precision
    implicit none

    real(p), parameter :: delta_t = 0.1 
    real(p),parameter :: vi = 500
    real(p), parameter :: g = 9.8
    private
    
    public :: delta_t, vi,g 

end module parameters


module euler
    use precision
    use parameters
    implicit none
    
contains
    subroutine solve(theta)
        real(p) :: theta
        real(p) :: xi, xi1, yi, yi1, vxi, vxi1, vyi, vyi1
        integer :: i

        vxi = vi * cos(theta)
        vyi = vi * sin(theta)

        xi = 0.
        yi = 0.

        open(unit=1, file="output.dat", status="replace", action = "write") 
        
        i = 0 
        do
            xi1 = xi + vxi * delta_t
            vxi1 = vxi
            yi1 = yi + vyi * delta_t
            vyi1 = vyi - g* delta_t



            i = i + 1
            if (i >= max_iter) then
                print *, "Exit by max iter"
            end if
        end do 

        close(1)
     

    end subroutine solve

end module euler

program exer2
    use precision
    use euler
    implicit none

    real(p) :: theta
    read(*,*) theta

    theta = atan(1.)/45 * theta
    call solve(theta)

end program exer2
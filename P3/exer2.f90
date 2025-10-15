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

        open(unit=1, file="output2.txt", status="replace", action = "write") 
        
        i = 0 
        do
            if (yi < 0) exit  ! Inverter ou não a ordem do exit e write?
            write(1,*) xi, yi ! Printo o menor que zero ou não?
              
            xi1 = xi + vxi * delta_t
            vxi1 = vxi
            yi1 = yi + vyi * delta_t
            vyi1 = vyi - g* delta_t

            xi = xi1
            vxi = vxi1
            yi = yi1
            vyi = vyi1

            i = i + 1
            if (i >= max_iter) then
                print *, "Exit by max iter"
            end if

        end do 

        close(1)
    end subroutine solve

    subroutine solve_exact(theta)
        real(p) :: theta
        real(p) :: xi, yi, vxi, vyi
        integer :: i

        vxi = vi * cos(theta)
        vyi = vi * sin(theta)

        xi = 0.
        yi = 0.

        open(unit=2, file="trajexata.txt", status="replace", action = "write") 

        i = 0 
        do
            if (yi < 0) exit  ! Inverter ou não a ordem do exit e write?
            write(2,*) xi, yi ! Printo o menor que zero ou não?
            
            xi = vxi * delta_t * (i+1)
            yi = vyi * delta_t * (i+1) - g* (delta_t*(i+1))**2/2

            i = i + 1
            if (i >= max_iter) then
                print *, "Exit by max iter"
            end if

        end do 

        close(2)

    end subroutine solve_exact

end module euler

program exer2
    use precision
    use euler
    implicit none

    real(p) :: theta, theta_exact
    read(*,*) theta

    theta = atan(1.)/45. * theta
    theta_exact = atan(1.)/45. * 45 ! Exata é para 55 ou para o de maior alcance?

    call solve(theta)
    !call solve_exact(theta_exact)

end program exer2
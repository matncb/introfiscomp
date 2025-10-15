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

    real(p), parameter :: g = 9.8
    real(p), parameter :: pi = 4.*atan(1.)
    private
    
    public :: g, pi 

end module parameters

module solvers
    use precision
    use parameters
    implicit none
    
contains
    subroutine solve(m, l, theta0, delta_t, T)
        real(p) :: m, l, theta0, delta_t, T
        real(p) :: omegai, omegai1, thetai, thetai1, time
        integer :: i

        thetai = theta0
        omegai = 0.
        time = 0.

        open(unit=1, file="exer3B_out.dat", status="replace", action = "write") 
        
        i = 0 
        do
            if (time > T) exit 
            write(1,*) time, thetai
              
            omegai1 = omegai - g/l * thetai *delta_t
            thetai1 = thetai + omegai1 * delta_t

            thetai = thetai1
            omegai = omegai1

            if (thetai > pi) then
                thetai = thetai - 2*pi                
            else if (thetai < -pi) then
                thetai = thetai + 2*pi
            end if

            i = i + 1
            time = delta_t * i
            if (i >= max_iter) then
                print *, "Exit by max iter"
            end if

        end do 

        close(1)
    end subroutine solve

    subroutine solve_complete(m, l, theta0, delta_t, T)
        real(p) :: m, l, theta0, delta_t, T
        real(p) :: omegai, omegai1, thetai, thetai1, time, E
        integer :: i

        thetai = theta0
        omegai = 0.
        time = 0.

        E = m*g*l * (1- cos(thetai)) 

        open(unit=2, file="exer3B_out_complete.dat", status="replace", action = "write") 
        
        i = 0 
        do
            if (time > T) exit 
            write(2,*) time, thetai, omegai, E
              
            omegai1 = omegai - g/l * thetai *delta_t
            thetai1 = thetai + omegai1 * delta_t

            thetai = thetai1
            omegai = omegai1

            if (thetai > pi) then
                thetai = thetai - 2*pi                
            else if (thetai < -pi) then
                thetai = thetai + 2*pi
            end if

            E =  m*g*l * (1- cos(thetai)) + m * l**2 * omegai**2/2

            i = i + 1
            time = delta_t * i
            if (i >= max_iter) then
                print *, "Exit by max iter"
            end if

        end do 

        close(2)

    end subroutine solve_complete

    subroutine solve_exact(m, l, theta0, delta_t, T)
        real(p) :: m, l, theta0, delta_t, T
        real(p) :: thetai, time
        integer :: i

        thetai = theta0
        time = 0.

        open(unit=3, file="exer3B_out_exact.dat", status="replace", action = "write") 
        
        i = 0 
        do
            if (time > T) exit 
            write(3,*) time, thetai

            thetai = theta0 * cos(sqrt(g/l) *(i+1)*delta_t)

            if (thetai > pi) then
                thetai = thetai - 2*pi                
            else if (thetai < -pi) then
                thetai = thetai + 2*pi
            end if

            i = i + 1
            time = delta_t * i
            if (i >= max_iter) then
                print *, "Exit by max iter"
            end if

        end do 

        close(3)

    end subroutine solve_exact

end module solvers

program exer3B
    use precision
    use solvers
    implicit none

    real(p) :: m, l, theta0, delta_t, T

    read(*,*) m
    read(*,*) l
    read(*,*) theta0
    read(*,*) delta_t
    read(*,*) T

    theta0 = theta0 * pi/180.

    call solve(m, l, theta0, delta_t, T)
    !call solve_complete(m, l, theta0, delta_t, T)
    !call solve_exact(m, l, theta0, delta_t, T) 


end program exer3B
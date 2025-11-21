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

    integer, parameter :: max_iter = 1000000
    real(p), parameter :: pi = 4._p* atan(1._p)

end module parameters

module solver
    use precision
    use parameters
    implicit none

contains
    subroutine verlet(x0, y0, vx0, vy0, delta_t)
        real(p), intent(in) :: x0, y0, vx0, vy0, delta_t
        real(p) :: xi2, xi1, xi, yi2, yi1, yi, aux, tol
        integer :: i

        xi = x0
        xi1 = x0 + vx0*delta_t

        yi = y0
        yi1 = y0 + vy0*delta_t

        open(unit=1, file="trajA1_out.dat", status='replace', action='write')

        write(1, *) 0._p, xi, yi
        write(1, *) delta_t, xi1, yi1

        i = 0
        do
            if (i > max_iter) then
                write(*,*) "Exit by max iter"
                exit
            end if
            
            xi2 = 2*xi1 - xi - (4.0_p*pi**2) * delta_t**2 * (xi1/((xi1**2 + yi1**2)**(1.5_p)))
            yi2 = 2*yi1 - yi - (4.0_p*pi**2) * delta_t**2 * (yi1/((xi1**2 + yi1**2)**(1.5_p)))

            tol = ((xi2 - xi1)**2 + (yi2-yi1)**2)/2
            if (((xi2 - x0)**2 + (yi2 - y0)**2) < tol) exit
            write(1, *) delta_t*(i+2.0_p), xi2, yi2

            aux = xi1
            xi1 = xi2
            xi = aux

            aux = yi1
            yi1 = yi2
            yi = aux

            i = i + 1
        end do
        close(1)
    end subroutine verlet

    !! PARA A TABELA

    subroutine verlet_T(x0, y0, vx0, vy0, delta_t, T)
        real(p), intent(out) :: T
        real(p), intent(in) :: x0, y0, vx0, vy0, delta_t
        real(p) :: xi2, xi1, xi, yi2, yi1, yi, aux, tol
        integer :: i

        xi = x0
        xi1 = x0 + vx0*delta_t

        yi = y0
        yi1 = y0 + vy0*delta_t

        i = 0
        do
            if (i > max_iter) then
                write(*,*) "Exit by max iter"
                exit
            end if
            
            xi2 = 2*xi1 - xi - (4.0_p*pi**2) * delta_t**2 * (xi1/((xi1**2 + yi1**2)**(1.5_p)))
            yi2 = 2*yi1 - yi - (4.0_p*pi**2) * delta_t**2 * (yi1/((xi1**2 + yi1**2)**(1.5_p)))

            tol = ((xi2 - xi1)**2 + (yi2-yi1)**2)/2
            if (((xi2 - x0)**2 + (yi2 - y0)**2) < tol) exit

            aux = xi1
            xi1 = xi2
            xi = aux

            aux = yi1
            yi1 = yi2
            yi = aux

            i = i + 1
        end do

        T = delta_t*(i+2.0_p)

    end subroutine verlet_T

    subroutine generate_kepler_table(delta_t)

        real(p), intent(in) :: delta_t

        character(len=20) :: planet_names(9)
        
        real(p), parameter :: R_arr(9) = [ &
            0.39_p, 0.72_p, 1.00_p, 1.52_p, 5.20_p, &
            9.24_p, 19.19_p, 30.06_p, 39.53_p &
        ]
        
        real(p) :: R, T, v0, T2_R3
        integer :: i
        
        open(unit=2, file="tabA1_out.dat", status='replace', action='write')

        ! Planetas

        planet_names(1) = "Mercúrio"
        planet_names(2) = "Vênus"
        planet_names(3) = "Terra"
        planet_names(4) = "Marte"
        planet_names(5) = "Júpiter"
        planet_names(6) = "Saturno"
        planet_names(7) = "Urano"
        planet_names(8) = "Netuno"
        planet_names(9) = "Plutão"

        !Cabeçalho
        write(2, *) 'Planeta  ', 'V0 (UA/ano)  ', 'T^2/R^3'
        
        ! Cada planeta
        do i = 1, 9
            R = R_arr(i)
            v0 = (2.0_p * pi) / sqrt(R)
            call verlet_T(R, 0.0_p, 0.0_p, v0, delta_t, T)
            T2_R3 = T**2 / R**3

            write(2, '(a)', advance="no") planet_names(i)
            write(2, *) V0, T2_R3

        end do
        
        close(2)
    
    end subroutine generate_kepler_table

end module solver

program exerA
    use precision
    use parameters
    use solver
    implicit none

    real(p) :: r, v0, delta_t

    read(*,*) r
    read(*,*) v0
    read(*,*) delta_t

    call verlet(r, 0.0_p, 0.0_p, v0, delta_t)
    !call generate_kepler_table(0.001_p)


end program exerA
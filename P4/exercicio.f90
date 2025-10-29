module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão

    private
    public :: p
end module precision

module parameters
    use precision
    implicit none

    real(p), parameter :: mb = 1.0_p
    real(p), parameter :: g = 9.8_p
    real(p), parameter :: delta_t = 0.01_p

    real(p), parameter :: a1 = 0.0039_p
    real(p), parameter :: a2 = 0.0058_P
    real(p), parameter :: vd = 35.0_p
    real(p), parameter :: delta = 5.0_p

    real(p), parameter :: goal_coord1(3) = (/40._p, 4._p, 2.5_p/)
    real(p), parameter :: goal_coord2(3) = (/40._p, 10._p, 2.5_p/)
    
    real(p) :: v0_norm = 100.0_p !km/h
    real(p) :: omega = 39.0_p !rot/s

    integer, parameter :: max_iter = 10000

contains

    subroutine load_si()
        v0_norm = v0_norm/3.6_p
        omega = omega*8.0_p*atan(1.0_p)
    end subroutine load_si
    
end module parameters

module solver
    use precision
    use parameters
    implicit none

contains
    subroutine solve(beta, theta_0, phi_0)

        real(p) :: beta, gamma
        real(p) :: theta_0
        real(p) :: phi_0

        real(p) :: v0x, v0y, v0z
        real(p) :: vxi, vyi, vzi, vxi1, vyi1, vzi1, vi
        real(p) :: xi, yi, zi, xi1, yi1, zi1
        real(p) :: ti, ti1

        integer :: i

        v0x = v0_norm * sin(theta_0)*cos(phi_0)
        v0y = v0_norm * sin(theta_0)*sin(phi_0)
        v0z = v0_norm * cos(theta_0)

        open(unit=1, file="chute_out.dat", status="replace", action = "write") 

        i = 0

        vxi = v0x
        vyi = v0y
        vzi = v0z

        xi = 0.0_p
        yi = 0.0_p
        zi = 0.0_p

        ti = 0.0_p

        do
            write(1,*) xi, yi
            if ((xi >= 40.0_p) .or. (zi < 0.0_p)) exit

            vi = sqrt(vxi**2 + vyi**2 + vzi**2)
            gamma = a1 + a2/(1.0_p + exp((vi - vd)/delta))

            vxi1 = vxi - delta_t*(gamma * vi*vxi + beta*omega*vyi)
            vyi1 = vyi - delta_t*(gamma * vi*vyi - beta*omega*vxi)
            vzi1 = vzi - delta_t*(g + gamma*vi*vzi)

            xi1 = xi + vxi*delta_t
            yi1 = yi + vyi*delta_t
            zi1 = zi + vzi*delta_t

            ti1 = (i+1)*delta_t

            i = i + 1

            vxi = vxi1
            vyi = vyi1
            vzi = vzi1

            xi = xi1
            yi = yi1
            zi = zi1

            ti = ti1
            
            if (i >= max_iter) then
                print *, "Exit by max iter"
                exit
            end if

        end do

        if ((yi >= goal_coord1(2)) .and. (yi <= goal_coord2(2)) .and. (zi <= goal_coord1(3)) .and. (zi >= 0.0_p)) then
            print *, "sim"
        else
            print *, "nao"
        end if

        close(1)

    end subroutine solve

    subroutine solve_complete(beta, theta_0, phi_0, tag)

        real(p) :: beta, gamma
        real(p) :: theta_0
        real(p) :: phi_0

        integer :: tag

        real(p) :: v0x, v0y, v0z
        real(p) :: vxi, vyi, vzi, vxi1, vyi1, vzi1, vi
        real(p) :: xi, yi, zi, xi1, yi1, zi1
        real(p) :: ti, ti1

        integer :: i

        character(len=10) :: beta_str, theta_0_str, phi_0_str, tag_str
        write(beta_str, '(F6.4)') beta
        write(theta_0_str, '(F4.2)') theta_0
        write(phi_0_str, '(F5.2)') phi_0
        write(tag_str, '(I1)') tag

        v0x = v0_norm * sin(theta_0)*cos(phi_0)
        v0y = v0_norm * sin(theta_0)*sin(phi_0)
        v0z = v0_norm * cos(theta_0)

        open(unit=2, file="./graphics_out/" // trim(beta_str) // "_" // trim(theta_0_str) // "_" &
            & // trim(phi_0_str) // "-tag-" // trim(tag_str) // ".dat", status="replace", action = "write") 

        i = 0

        vxi = v0x
        vyi = v0y
        vzi = v0z

        xi = 0.0_p
        yi = 0.0_p
        zi = 0.0_p

        ti = 0.0_p

        do
            write(2,*) ti, xi, yi, zi
            if ((xi >= 40.0_p) .or. (zi < 0.0_p)) exit

            vi = sqrt(vxi**2 + vyi**2 + vzi**2)
            gamma = a1 + a2/(1.0_p + exp((vi - vd)/delta))

            vxi1 = vxi - delta_t*(gamma * vi*vxi + beta*omega*vyi)
            vyi1 = vyi - delta_t*(gamma * vi*vyi - beta*omega*vxi)
            vzi1 = vzi - delta_t*(g + gamma*vi*vzi)

            xi1 = xi + vxi*delta_t
            yi1 = yi + vyi*delta_t
            zi1 = zi + vzi*delta_t

            ti1 = (i+1)*delta_t

            i = i + 1

            vxi = vxi1
            vyi = vyi1
            vzi = vzi1

            xi = xi1
            yi = yi1
            zi = zi1

            ti = ti1
            
            if (i >= max_iter) then
                print *, "Exit by max iter"
                exit
            end if

        end do

        close(2)

    end subroutine solve_complete


    subroutine generete_out_graphics()
        ! Gráfico A
        call solve_complete(0.0005_p, 1.15_p, 0.10_p, 1)
        call solve_complete(0.0005_p, 1.45_p, 0.05_p, 1)
        call solve_complete(0.0005_p, 1.15_p, -0.10_p, 1)

        ! Gráfico B
        call solve_complete(0.0005_p, 1.15_p, 0.10_p, 2)
        call solve_complete(0.0010_p, 1.15_p, 0.10_p, 2)
        call solve_complete(0.0001_p, 1.15_p, 0.10_p, 2)
        
        ! Gráfico C e animação
        call solve_complete(0.0005_p, 1.15_p, 0.10_p, 3)

    end subroutine generete_out_graphics

end module solver

program exercicio
    use precision
    use parameters
    use solver
    implicit none

    real(p) :: beta
    real(p) :: theta_0
    real(p) :: phi_0

    call load_si()

    read(*,*) beta
    read(*,*) theta_0
    read(*,*) phi_0

    call solve(beta, theta_0, phi_0)
    call generete_out_graphics()

end program exercicio
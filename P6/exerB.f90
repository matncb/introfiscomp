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

    integer, parameter :: max_iter = 100000
    real(p), parameter :: pi = 4._p * atan(1._p)

    real(p), parameter :: GMS = 4._p * pi**2 ! Sol
    real(p), parameter :: GMT = GMS * 3.0e-6_p ! Terra
    real(p), parameter :: GMJ = GMS * 1.9_p/2.0_p *1e-3_p ! Júpiter

    ! Vamos assumir como condição inicial órbitas circulares se os planetas estivessem sozinhos

    ! Terra 
    real(p), parameter :: RT = 1.0_p
    real(p), parameter :: VT = 2.0_p * pi / sqrt(RT)
    
    ! Júpiter 
    real(p), parameter :: RJ = 5.20_p
    real(p), parameter :: VJ = 2.0_p * pi / sqrt(RJ)

end module parameters

module solver
    use precision
    use parameters
    implicit none

contains

subroutine verlet(id, multJ, delta_t)
    real(p), intent(in) :: multJ, delta_t
    real(p) :: xTi2, xTi1, xTi, yTi2, yTi1, yTi
    real(p) :: xJi2, xJi1, xJi, yJi2, yJi1, yJi
    real(p) :: RTS_el, RTJ_el, RSJ_el, GMJ_eff
    real(p) :: aux
    integer :: i

    integer, intent(in) :: id 
    character(len = 5) :: id_str
    write(id_str, '(I0)') id

    GMJ_eff = GMJ * multJ
    ! Condições iniciais

    ! Terra
    xTi = RT
    xTi1 = xTi
    yTi = 0.0_p
    yTi1 = yTi + VT*delta_t

    ! Júpiter
    xJi = RJ
    xJi1 = xJi
    yJi = 0.0_p
    yJi1 = yJi + VJ*delta_t


    open(unit=id, file="trajB" // trim(adjustl(id_str)) // "_out.dat", status='replace', action='write')

    write(id, *) 0._p, xTi, yTi
    write(id, *) delta_t, xTi1, yTi1

    i = 0
    do
        if (i > max_iter) then
            exit
        end if
        
        RTS_el = ((xTi1)**2 + (yTi1)**2)**(1.5_p)
        RTJ_el = ((xTi1 - xJi1)**2 + (yTi1 - yJi1)**2)**(1.5_p)
        RSJ_el = ((xJi1)**2 + (yJi1)**2)**(1.5_p)

        xTi2 = 2*xTi1 - xTi - delta_t**2 * (GMS/(RTS_el)*xTi1 + GMJ_eff/(RTJ_el) *(xTi1 - xJi1))
        yTi2 = 2*yTi1 - yTi - delta_t**2 * (GMS/(RTS_el)*yTi1 + GMJ_eff/(RTJ_el) *(yTi1 - yJi1))

        xJi2 = 2*xJi1 - xJi - delta_t**2 * (GMS/(RSJ_el)*xJi1 + GMT/(RTJ_el) *(xJi1 - xTi1))
        yJi2 = 2*yJi1 - yJi - delta_t**2 * (GMS/(RSJ_el)*yJi1 + GMT/(RTJ_el) *(yJi1 - yTi1))

        write(id, *) delta_t*(i+2.0_p), xTi2, yTi2

        aux = xTi1
        xTi1 = xTi2
        xTi = aux

        aux = yTi1
        yTi1 = yTi2
        yTi = aux

        aux = xJi1
        xJi1 = xJi2
        xJi = aux

        aux = yJi1
        yJi1 = yJi2
        yJi = aux

        i = i + 1
    end do
    close(id)
    end subroutine verlet

end module solver

program exerB
    use precision
    use parameters
    use solver
    implicit none

    real(p), parameter :: delta_t = 0.0001_p

    call verlet(1, 1.0_p, delta_t)
    call verlet(100, 100.0_p, delta_t)
    call verlet(10000, 1000.0_p, delta_t)

end program exerB
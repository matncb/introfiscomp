program exerD
    ! Pensar na precisão dos inteiros e do ponto flutuante
    implicit none
    integer(8) :: M
    read(*,*) M

     print '(F6.4)', pi_monte_carlo(M)

contains
    function pi_monte_carlo(N) result(pi)
        integer(8), intent(in) :: N
        real :: pi
        
        integer(8) :: i
        integer(8) :: cont
        real :: coord(2)

        cont = 0
        do i = 1, N
            call random_number(coord)
            if (((coord(1)-0.5)**2 + (coord(2)-0.5)**2) < 0.25) cont = cont + 1
        end do

        pi = 4. * (real(cont)/real(N)) 
    end function pi_monte_carlo
end program exerD
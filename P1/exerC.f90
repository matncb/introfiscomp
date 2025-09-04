program exerC
    ! M incluso ou não? Vou assumir que sim
    integer :: M
    read(*,*) M

    call gen_prime_num_out(M)
contains
    subroutine gen_prime_num_out(M)
        integer, intent(in) :: M

        ! Algoritmo: Crivo de Eratóstenes

        ! Números dados pelo índice
        ! Mascara diz se é primo ou não
        
        logical :: mask(2:M)
        integer :: i, j
        do i = 2, M
            mask(i) = .true.
        end do 

        do i = 2,ceiling(sqrt(real(M))) 
            if (mask(i)) then
                do j = i*i, M, i
                    mask(j) = .false.
                end do
            end if
        end do

        open(unit=1, file='primos_out.dat', status='replace')

        do i = 2, M
            if (mask(i)) write(1, *) i
        end do 
        
        close(unit = 1)

    end subroutine gen_prime_num_out

end program exerC
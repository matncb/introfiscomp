module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão

    private
    public :: sp, dp, p
end module precision

module parameters
    use precision
    implicit none

    real(p), parameter :: m = 80._p 
    real(p),parameter :: power = 400._p

    private
    public :: m, power

end module parameters


module euler
    use precision
    use parameters
    implicit none
    
contains
    subroutine find_N(T, delta_t, N)
        real(p), intent(in) :: T, delta_t
        integer, intent(out) :: N
        N = int(T/delta_t) + 1
    end subroutine find_N

    subroutine solve(v0, delta_t, N, t_array, v_array)
        real(p), intent(in) :: v0, delta_t
        integer, intent(in) :: N
        integer :: i
        
        real(p) :: v_array(N), t_array(N)

        v_array(1) = v0
        t_array(1) = 0._p

        do i = 1, N-1
            v_array(i+1) = v_array(i) + delta_t * power/(m*v_array(i))
            t_array(i+1) = i*delta_t
        end do

    end subroutine solve

end module euler

module data_handler
    use precision
    use euler
    implicit none

contains
    subroutine gen_out(v0, T, delta_t)
        real(p), intent(in) :: v0, T, delta_t
        integer :: i,N

        real(p), allocatable:: v_array(:), t_array(:)

        call find_N(T, delta_t, N)

        allocate(v_array(N))
        allocate(t_array(N))

        call solve(v0,delta_t, N, t_array, v_array)

        open(unit=1, file="vel1_out.dat", status="replace", action = "write") 
            
        do i = 1, N
            write(1, *) t_array(i), v_array(i)
        end do

        close(1)

        deallocate(t_array, v_array)
       
    end subroutine gen_out

end module data_handler

program exer1
    use precision
    use data_handler

    implicit none

    real(p) :: v0, T, delta_t

    read(*,*) T, delta_t, v0
    call gen_out(v0, T, delta_t)

end program exer1
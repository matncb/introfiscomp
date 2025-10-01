module precision
    implicit none
    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: p = dp ! Define precisão padrão

    private
    public :: sp, dp, p
end module precision

module param
    use precision
    implicit none

    real(p), parameter :: m = 80._p 
    real(p),parameter :: P = 400._p

    private
    public :: m, P

end module param


module euler
    use precision
    use param
    implicit none
    
contains
    subroutine solve(v0, T, delta_t, N, t_array, v_array)
        real(p), intent(in) :: v0, T, delta_t
        integer, intent(out) :: N
        integer :: i
        
        real(p), allocatable, intent(out) :: v_array(:), t_array(:)
    
        N = int(T/delta_t) + 1

        allocate(v_array(N))
        allocate(t_array(N))

        v_array(0) = v0
        t_array(0) = 0._p

        do i = 0, N-2
            v_array(i+1) = v_array(i) + delta_t * P/(m*v_array(i))
            t_array(i+1) = (i+1)*delta_t
        end do
    end subroutine solve

end module euler

module data_handler
    use precision
    use euler
    implicit none

contains

    subroutine gen_out()
       
    end subroutine gen_out

end module data_handler



program exer1
    implicit none

end program exer1
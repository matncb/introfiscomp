program exerA
    implicit none

    integer, parameter :: sp = kind(0.e0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: qp = kind(0._16)

    integer, parameter :: max_iter = 10000

    real(sp) :: precision_sp
    real(dp) :: precision_dp
    real(qp) :: precision_qp
    integer :: bits_sp, bits_dp, bits_qp

    call test_sp(precision_sp, bits_sp)
    call test_dp(precision_dp, bits_dp)
    call test_qp(precision_qp, bits_qp)

    print *, bits_sp, precision_sp
    print *, bits_dp, precision_dp
    print *, bits_qp, precision_qp

contains
    subroutine test_sp(precision, bits)
        real(sp) :: a, sum, one

        integer :: iter

        real(sp), intent(out) :: precision
        integer, intent(out) :: bits

        print *, "PRECISAO SIMPLES"

        one = 1._sp
        a = one
        iter = 0

        do
            iter = iter + 1
            sum = a + one

            print *, a, sum

            if (sum == one) exit

            if ((iter > max_iter)) then
                print *, "Exit by max iter"
                exit
            end if

            a = a/2._sp
        end do

        precision = a
        bits = iter -1
       
    end subroutine test_sp

    subroutine test_dp(precision, bits)
        real(dp) :: a, sum, one

        integer :: iter

        real(dp), intent(out) :: precision
        integer, intent(out) :: bits

        print *, "PRECISAO DUPLA"

        one = 1._dp
        a = one
        iter = 0

        do
            iter = iter + 1
            sum = a + one

            print *, a, sum

            if (sum == one) exit

            if ((iter > max_iter)) then
                print *, "Exit by max iter"
                exit
            end if

            a = a/2._dp
        end do

        precision = a
        bits = iter -1
       
    end subroutine test_dp

    subroutine test_qp(precision, bits)
        real(qp) :: a, sum, one

        integer :: iter

        real(qp), intent(out) :: precision
        integer, intent(out) :: bits

        print *, "PRECISAO QUADRUPLA"

        one = 1._qp
        a = one
        iter = 0

        do
            iter = iter + 1
            sum = a + one

            print *, a, sum

            if (sum == one) exit

            if ((iter > max_iter)) then
                print *, "Exit by max iter"
                exit
            end if

            a = a/2._qp
        end do

        precision = a
        bits = iter -1
       
    end subroutine test_qp

end program exerA


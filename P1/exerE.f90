program exerE
    implicit none

    real, allocatable :: M(:, :)

    real :: epsilon
    integer :: n
    integer :: max_iter = 10000

    read(*,*) epsilon
    read(*,*) n

    allocate(M(n,n))
    call M_fill()

    call M_lambda()

    deallocate(M)

contains
    subroutine M_fill()
        integer :: i, j

        do i= 1,n
            read(*, *) (M(i, j), j= 1,n) 
        end do
    end subroutine M_fill

    subroutine normalize(v)
        real :: v(n)
        v = v/sqrt(dot_product(v, v))
    end subroutine normalize

    subroutine M_lambda()
        integer :: k, i
        real :: x_k(n), x_k_1(n)
        real :: lambda_k, lambda_k_1
        real :: precision

        k = 0
        lambda_k = 0

        do i = 1,n
            x_k(i) = 1.
        end do

        x_k_1 = matmul(M, x_k)

        call normalize(x_k_1)
        call normalize(x_k)
        
        do
            lambda_k_1 = dot_product(x_k, x_k_1)/dot_product(x_k_1, x_k_1)
            precision = abs(lambda_k_1 - lambda_k)

            if (precision < epsilon) exit

            if (k > max_iter) then
                print *, "Exit by max iter"
            end if 

            k = k + 1
            x_k = x_k_1
            x_k_1 = matmul(M, x_k_1)

            call normalize(x_k_1)

            lambda_k = lambda_k_1
            
        end do

        print *, lambda_k_1

        do i = 1, n
            print *, x_k_1(i)
        end do

    end subroutine M_lambda

end program exerE
program exer0
    implicit none

    ! Variáveis

    real(4) :: V(4, 3) ! Armazena cada vetor de dimensão 3 em uma matriz
    integer :: i, j

    ! Leitura dos dados
    do i = 1, 4
        read(*,*) (V(i,j), j = 1, 3)
    end do

    ! Saída
    !open(1, file = 'tetra_out.dat', status='replace')
    !write(1, *) volume
    !write(1, *) soma_areas

    close(1)

contains
    function cross_product(v1, v2) result(cross)
        real(4), dimension(3) :: v1, v2
        real(4) :: cross(3)

        cross(1) = v1(2)*v2(3) - v1(3)*v2(2)
        cross(2) = -(v1(1)*v2(3) - v1(3)*v2(1))
        cross(3) = v1(1)*v2(2) -v1(2)*v2(1)

    end function cross_product

    function face_area(v1,v2,v3) result(area)
        real(4) :: area
        real(4), dimension(3) :: v1,v2,v3
        real (4), dimension(3) :: r1, r2, cross

        r1 = v2 -v1
        r2 = v3 - v1

        cross = cross_product(r1,r2)
        area = sqrt(dot_product(cross,cross))


    end function face_area
         
end program exer0
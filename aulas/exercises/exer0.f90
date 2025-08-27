program exer0
    implicit none

    ! Variáveis a serem lidas
    real(4) :: V(4,3) ! Armazena cada vetor de dimensão 3 em uma matriz
    integer :: i, j

    ! Resultados
    real(4) :: vol_res, sum_res
    real(4), dimension(4) :: areas

    ! Leitura dos dados
    do i = 1, 4
        read(*,*) (V(i,j), j = 1, 3)
    end do

    vol_res = volume(V(1,:), V(2,:) ,V(3,:) ,V(4,:))
    call fill_areas(V(1,:), V(2,:) ,V(3,:) ,V(4,:), areas) 
    call sort_areas(areas)
    sum_res = sum_areas(areas)
    call write_out(vol_res,sum_res, areas)

contains
    function cross_product(v1, v2) result(cross)
        real(4), dimension(3) :: v1, v2
        real(4) :: cross(3)

        cross(1) = v1(2)*v2(3) - v1(3)*v2(2)
        cross(2) = -(v1(1)*v2(3) - v1(3)*v2(1))
        cross(3) = v1(1)*v2(2) -v1(2)*v2(1)

    end function cross_product

    function volume(v1,v2,v3,v4) result(vol)
        real(4), dimension(3) :: v1, v2, v3, v4
        real(4), dimension(3) :: r1, r2, r3
        real(4) :: vol

        r1 = v2 - v1
        r2 = v3 - v1
        r3 = v4 - v1

        vol = (1./6.) * dot_product(r1, cross_product(r2,r3))

    end function volume

    function face_area(v1,v2,v3) result(area)
        real(4) :: area
        real(4), dimension(3) :: v1,v2,v3
        real (4), dimension(3) :: r1, r2, cross

        r1 = v2 -v1
        r2 = v3 - v1

        cross = cross_product(r1,r2)
        area = (0.5)*sqrt(dot_product(cross,cross))

    end function face_area

    subroutine fill_areas(v1,v2,v3,v4, areas_arr)
        real(4), dimension(4) :: areas_arr
        real(4), dimension(3) :: v1, v2, v3, v4

        areas_arr(1) = face_area(v1,v2,v3)
        areas_arr(2) = face_area(v1,v2,v4)
        areas_arr(3) = face_area(v1,v3,v4)
        areas_arr(4) = face_area(v2,v3,v4)

    end subroutine fill_areas

    subroutine sort_areas(areas_arr)
        real(4), dimension(4) :: areas_arr
        integer :: l
        real(4) :: a, b

        do l = 1,3
            a = areas_arr(l)
            b = areas_arr(l+1)
            if (a > b) then
                areas_arr(l+1) = a
                areas_arr(l) = b
            end if
        end do
    end subroutine sort_areas

    function sum_areas(areas_arr) result(sum)
        real(4), dimension(4) :: areas_arr
        integer :: l
        real(4) :: sum

        sum = 0.
        do l = 1,4
            sum = sum + areas_arr(l)
        end do

    end function sum_areas

    subroutine write_out(volume, sum, areas_arr)
        real(4), dimension(4) :: areas_arr
        real(4) :: sum, volume

        integer :: l
        real(4) :: temp

        open(1, file = 'tetra_out.dat', status='replace')

        write(1,*) volume
        write(1,*) sum

        temp = -1.
        do l = 1,4
            if (areas_arr(l) /= temp) then     
                write(1,*) areas_arr(l)
                temp = areas_arr(l)
            end if
        end do

        close(1)

    end subroutine write_out


end program exer0
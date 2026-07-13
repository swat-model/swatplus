subroutine soils_test_adjust(isol, mlyr)
    ! Adjust the input soil values based input soil test values.
        
    use soil_module
    use soil_data_module
    
    implicit none  

    integer, intent(in) :: isol
    integer, intent(in) :: mlyr     !none |max number of soil layers
    real :: soil_layer_thickness    !     |thickness of soil layer being processed 
    real :: prev_depth = 0.         !mm   |previous custom depth in millimeters
    real :: sum_bd, sum_cbn, sum_sand, sum_silt, sum_clay !     |temporary sums for weighted averages
    integer :: tot_soil_depth = 0.  !mm   |total soil depth for the soil being processed
    integer :: test                 !     |soil test index
    integer :: i                    !     |index to sol array
    integer :: j                    !     |index to soildb array, sol_mm_db array
    logical :: first_lr             !     |flag if first layer is being processed.

    type (soil_database), dimension(:), allocatable :: sol_mm_db

    first_lr = .true.
    do test = 1, nmbr_soil_test_layers
        if (sol_test(test)%snam == sol(isol)%s%snam) then

            ! insert soil layer test values into temporary data structure

            if (first_lr) then
                prev_depth = 0.
                first_lr = .false.

                ! Allocate a temporary data structure to do weighted averages from.
                tot_soil_depth = soildb(isol)%ly(soildb(isol)%s%nly)%z
                allocate (sol_mm_db(1))
                ! allocate (sol_mm_db(1)%phys(tot_soil_depth))
                allocate (sol_mm_db(1)%ly(tot_soil_depth))

                ! Populate temporary data structure to do weighted averages from.
                prev_depth = 0
                do j = 1, soildb(isol)%s%nly
                    do i = prev_depth + 1, tot_soil_depth
                        if (i <= soildb(isol)%ly(j)%z .and. i > prev_depth ) then
                            sol_mm_db(1)%ly(i)%z = i
                            sol_mm_db(1)%ly(i)%bd = soildb(isol)%ly(j)%bd
                            sol_mm_db(1)%ly(i)%awc = soildb(isol)%ly(j)%awc
                            sol_mm_db(1)%ly(i)%k = soildb(isol)%ly(j)%k
                            sol_mm_db(1)%ly(i)%cbn = soildb(isol)%ly(j)%cbn
                            sol_mm_db(1)%ly(i)%clay = soildb(isol)%ly(j)%clay
                            sol_mm_db(1)%ly(i)%silt = soildb(isol)%ly(j)%silt
                            sol_mm_db(1)%ly(i)%sand = soildb(isol)%ly(j)%sand
                            sol_mm_db(1)%ly(i)%rock = soildb(isol)%ly(j)%rock
                            sol_mm_db(1)%ly(i)%alb = soildb(isol)%ly(j)%alb
                            sol_mm_db(1)%ly(i)%usle_k = soildb(isol)%ly(j)%usle_k
                            sol_mm_db(1)%ly(i)%ec = soildb(isol)%ly(j)%ec
                            sol_mm_db(1)%ly(i)%cal = soildb(isol)%ly(j)%cal
                            sol_mm_db(1)%ly(i)%ph = soildb(isol)%ly(j)%ph
                        else 
                            exit
                        end if
                    end do
                    prev_depth = soildb(isol)%ly(j)%z               
                end do
                prev_depth = 0
            else
                prev_depth =  sol_test(test-1)%d
            endif

            do i = 1, tot_soil_depth
                if (i > prev_depth .and. i <= sol_test(test)%d ) then
                    if (sol_test(test)%bd /= -1.0 ) sol_mm_db(1)%ly(i)%bd = sol_test(test)%bd
                    if (sol_test(test)%cbn /= -1.0 ) sol_mm_db(1)%ly(i)%cbn = sol_test(test)%cbn
                    if (sol_test(test)%sand /= -1.0 ) sol_mm_db(1)%ly(i)%sand = sol_test(test)%sand
                    if (sol_test(test)%silt /= -1.0 ) sol_mm_db(1)%ly(i)%silt = sol_test(test)%silt
                    if (sol_test(test)%clay /= -1.0 ) sol_mm_db(1)%ly(i)%clay = sol_test(test)%clay
                else
                    cycle
                endif 
            end do
        endif
    end do

    ! Adjust soil profile values by doing weighted averages
    do test = 1, nmbr_soil_test_layers
        if (sol_test(test)%snam == sol(isol)%s%snam) then
            prev_depth = 0.
            do i = 1, mlyr
                soil_layer_thickness = sol(isol)%phys(i)%d - prev_depth

                if (sol(isol)%phys(i)%d > prev_depth) then
                    ! calculate weighted averages
                    sum_bd = 0.; sum_cbn = 0.; sum_sand = 0.; sum_silt = 0.; sum_clay = 0.
                    do j = prev_depth + 1, sol(isol)%phys(i)%d
                        sum_bd   = sum_bd   + sol_mm_db(1)%ly(j)%bd
                        sum_cbn  = sum_cbn  + sol_mm_db(1)%ly(j)%cbn
                        sum_sand = sum_sand + sol_mm_db(1)%ly(j)%sand
                        sum_silt = sum_silt + sol_mm_db(1)%ly(j)%silt
                        sum_clay = sum_clay + sol_mm_db(1)%ly(j)%clay
                    end do
                    sol(isol)%phys(i)%bd   = sum_bd   / soil_layer_thickness
                    sol(isol)%phys(i)%cbn  = sum_cbn  / soil_layer_thickness
                    sol(isol)%phys(i)%sand = sum_sand / soil_layer_thickness
                    sol(isol)%phys(i)%silt = sum_silt / soil_layer_thickness
                    sol(isol)%phys(i)%clay = sum_clay / soil_layer_thickness
                    prev_depth = sol(isol)%phys(i)%d
                endif
            enddo
            exit
        endif
    end do

    if (allocated(sol_mm_db)) then
        deallocate (sol_mm_db(1)%ly)
        deallocate (sol_mm_db)
    endif

end subroutine soils_test_adjust
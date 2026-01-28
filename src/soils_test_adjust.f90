subroutine soils_test_adjust(isol, mlyr)
    ! Adjust the input soil values based input soil test values.
        
    use soil_module
    
    implicit none  

    integer, intent(in) :: isol
    integer, intent(in) :: mlyr     !none |max number of soil layers
    real :: test_layer_sum = 0.     !     |temporary variable to store weighted soil test layer fraction
    real :: non_test_layer_sum = 0. !     |temporary variable to store weighted non soil test layer fraction 
    real :: sum  = 0.               !     |sum of wighted fractions
    real :: soil_layer_thickness    !     |thickness of soil layer being processed 
    real :: prev_depth = 0          !mm   |previous custom depth in millimeters
    integer :: test                 !     |soil test index
    integer :: i                    !     |index to sol array
    logical :: first_lr             !     |flag if first layer is being processed.

    first_lr = .true.

    ! Adjust bulk density based on soil test
    do test = 1, nmbr_soil_tests
        if (sol_test(test)%snam == sol(isol)%s%snam) then
            if (first_lr) then
                prev_depth = 0.
                first_lr = .false.
            else
                prev_depth =  sol_test(test-1)%d
            endif
            ! adjust bulk density first if it is provided in soil test data because 
            ! the other soil test values are weighted by bulk density and layer thickness.
            if (sol_test(test)%bd /= -1.0 ) then
                do i = 1, mlyr
                    if (sol_test(test)%d > prev_depth) then
                        if (sol(isol)%phys(i)%d > prev_depth) then
                            if (sol_test(test)%d >= sol(isol)%phys(i)%d) then
                                if (sol_test(test)%bd /= -1.0 ) sol(isol)%phys(i)%bd = sol_test(test)%bd 
                                if (sol_test(test)%cbn /= -1.0 ) sol(isol)%phys(i)%cbn = sol_test(test)%cbn 
                                if (sol_test(test)%sand /= -1.0 ) sol(isol)%phys(i)%sand = sol_test(test)%sand 
                                if (sol_test(test)%silt /= -1.0 ) sol(isol)%phys(i)%silt = sol_test(test)%silt 
                                if (sol_test(test)%clay /= -1.0 ) sol(isol)%phys(i)%silt = sol_test(test)%clay
                                prev_depth = sol(isol)%phys(i)%d
                            else
                                soil_layer_thickness = sol(isol)%phys(i)%d - prev_depth
                                if (sol_test(test)%bd /= -1.0 ) then
                                    test_layer_sum = (sol_test(test)%d - prev_depth) * sol_test(test)%bd
                                    non_test_layer_sum = (sol(isol)%phys(i)%d - sol_test(test)%d) * sol(isol)%phys(i)%bd
                                    sum = test_layer_sum + non_test_layer_sum
                                    sol(isol)%phys(i)%bd = sum / soil_layer_thickness
                                endif
                                if (sol_test(test)%cbn /= -1.0 ) then
                                    test_layer_sum = (sol_test(test)%d - prev_depth) * sol_test(test)%cbn
                                    non_test_layer_sum = (sol(isol)%phys(i)%d - sol_test(test)%d) * sol(isol)%phys(i)%cbn
                                    sum = test_layer_sum + non_test_layer_sum
                                    sol(isol)%phys(i)%cbn = sum / soil_layer_thickness
                                endif
                                if (sol_test(test)%sand /= -1.0 ) then
                                    test_layer_sum = (sol_test(test)%d - prev_depth) * sol_test(test)%sand
                                    non_test_layer_sum = (sol(isol)%phys(i)%d - sol_test(test)%d) * sol(isol)%phys(i)%sand
                                    sum = test_layer_sum + non_test_layer_sum
                                    sol(isol)%phys(i)%sand = sum / soil_layer_thickness
                                endif
                                if (sol_test(test)%silt /= -1.0 ) then
                                    test_layer_sum = (sol_test(test)%d - prev_depth) * sol_test(test)%silt
                                    non_test_layer_sum = (sol(isol)%phys(i)%d - sol_test(test)%d) * sol(isol)%phys(i)%silt
                                    sum = test_layer_sum + non_test_layer_sum
                                    sol(isol)%phys(i)%silt = sum / soil_layer_thickness
                                endif
                                if (sol_test(test)%clay /= -1.0 ) then
                                    test_layer_sum = (sol_test(test)%d - prev_depth) * sol_test(test)%clay
                                    non_test_layer_sum = (sol(isol)%phys(i)%d - sol_test(test)%d) * sol(isol)%phys(i)%clay
                                    sum = test_layer_sum + non_test_layer_sum
                                    sol(isol)%phys(i)%clay = sum / soil_layer_thickness
                                endif
                                exit
                            endif
                        endif
                    else 
                        exit
                    endif
                enddo
            endif
        endif
    end do
end subroutine soils_test_adjust
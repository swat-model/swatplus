subroutine soils_test_adjust(isol, mlyr)
    ! Adjust the input soil values based input soil test values.
    ! A soil test value is a weighted average based on bulk density and layer thickness 
    ! down to the test layer depth.  The changes to soil data must result in the same weighted
    ! average as the soil test value down to the soil test depth.  The relative differences in 
    ! original data in the layers is maintained.
        
    use soil_module
    
    implicit none  

    integer, intent(in) :: isol
    integer, intent(in) :: mlyr         !none          |max number of soil layers
    real :: sum_bd = 0.         !              |temporary sum to do weighted average with 
    real :: sum_cbn = 0.        !              |temporary sum to do weighted average with 
    real :: sum_thick = 0.      !              |temporary sum to do weighted average with 
    real :: sum_clay = 0.       !              |temporary sum to do weighted average with 
    real :: sum_sand = 0.       !              |temporary sum to do weighted average with 
    real :: ltxbd = 0.          !              |Layer thickness time bulk density of that layer 
    real :: ltxbd_sum = 0.      !              |Sum of layer thickness times bulk density
    real :: wavg_bd = 0.        !              |weighted average of soil carbon
    real :: wavg_cbn = 0.       !              |weighted average of soil carbon
    real :: wavg_clay = 0.      !              |weighted average of soil carbon
    real :: wavg_sand = 0.      !              |weighted average of soil carbon
    real :: adjust_frac_bd = 0. !              |computed  weigted average adjustment factor for soil carbon
    real :: adjust_frac_cbn = 0.!              |computed  weigted average adjustment factor for soil carbon
    real :: adjust_frac_clay = 0. !            |computed  weigted average adjustment factor for soil carbon
    real :: adjust_frac_sand = 0. !            |computed  weigted average adjustment factor for soil carbon
    integer :: i                !              |index to array
    integer :: prev_depth = 0   !mm            |previous custom depth in millimeters
    integer :: soil_lyr_thickness !            |temporary variable to store layer thickness
    integer :: test             !              |soil test index


    do test = 1, nmbr_soil_tests
        if (sol_test(test)%snam == sol(isol)%s%snam) then
            ! adjust bulk density first if it is provided in soil test data because 
            ! the other soil test values are weighted by bulk density and layer thickness.
            if (sol_test(test)%bd > 0.00001) then
                prev_depth = 0
                sum_bd = 0.0 
                sum_thick = 0.0 
                do i = 1, mlyr
                    if (sol_test(test)%d > prev_depth) then
                        soil_lyr_thickness = sol(isol)%phys(i)%d - prev_depth 
                        ltxbd = soil_lyr_thickness * sol(isol)%phys(i)%bd
                        sum_bd = sum_bd + ltxbd
                        sum_thick = sum_thick + soil_lyr_thickness
                        prev_depth = sol(isol)%phys(i)%d
                    else 
                        exit
                    endif
                enddo
                wavg_bd = sum_bd/sum_thick
                adjust_frac_bd = sol_test(test)%bd / wavg_bd 
                prev_depth = 0.0
                do i = 1, mlyr
                    if (sol_test(test)%d > prev_depth) then
                        sol(isol)%phys(i)%bd = adjust_frac_bd * sol(isol)%phys(i)%bd 
                        prev_depth = sol(isol)%phys(i)%d
                    else 
                        exit
                    endif
                enddo
            endif
        endif
    end do

    ! Adjust soil carbon based on soil test
    do test = 1, nmbr_soil_tests
        if (sol_test(test)%snam == sol(isol)%s%snam) then
            prev_depth = 0
            sum_cbn = 0.    
            sum_sand = 0.    
            sum_clay = 0.    
            ltxbd_sum = 0.0 
            do i = 1, mlyr
                if (sol_test(test)%d > prev_depth) then
                    soil_lyr_thickness = sol(isol)%phys(i)%d - prev_depth 
                    ltxbd = soil_lyr_thickness * sol(isol)%phys(i)%bd
                    ltxbd_sum = ltxbd_sum + ltxbd
                    sum_cbn = sum_sand + ltxbd * sol(isol)%phys(i)%sand
                    sum_sand = sum_sand + ltxbd * sol(isol)%phys(i)%sand
                    sum_clay = sum_clay + ltxbd * sol(isol)%phys(i)%clay
                    prev_depth = sol(isol)%phys(i)%d
                else 
                    exit
                endif
            enddo

            if (sol_test(test)%cbn > 0.00001) then
                wavg_cbn = sum_cbn/ltxbd_sum
                adjust_frac_cbn = sol_test(test)%cbn / wavg_cbn 
                prev_depth = 0.0
                do i = 1, mlyr
                    if (sol_test(test)%d > prev_depth) then
                        sol(isol)%phys(i)%cbn = adjust_frac_cbn * sol(isol)%phys(i)%cbn 
                        prev_depth = sol(isol)%phys(i)%d
                    else 
                        exit
                    endif
                enddo
            endif

            ! Adjust sand, silt, and clay based on soil test. 
            if (sol_test(test)%sand > 0.00001 .and. &
                sol_test(test)%silt > 0.00001 .and. &
                sol_test(test)%clay > 0.00001 ) then
                wavg_sand = sum_sand/ltxbd_sum
                adjust_frac_sand = sol_test(test)%sand / wavg_sand 
                wavg_clay = sum_clay/ltxbd_sum
                adjust_frac_clay = sol_test(test)%clay / wavg_clay 
                prev_depth = 0.0
                do i = 1, mlyr
                    if (sol_test(test)%d > prev_depth) then
                        sol(isol)%phys(i)%sand = adjust_frac_sand * sol(isol)%phys(i)%sand 
                        sol(isol)%phys(i)%clay = adjust_frac_clay * sol(isol)%phys(i)%clay 
                        sol(isol)%phys(i)%silt = 100.0 - (sol(isol)%phys(i)%sand + sol(isol)%phys(i)%clay)
                        prev_depth = sol(isol)%phys(i)%d
                    else 
                        exit
                    endif
                enddo
            endif
        endif
    enddo

end subroutine soils_test_adjust
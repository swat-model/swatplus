      subroutine pesticide_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

        use hru_module, only : hru, sol_plt_ini
        use soil_module
        use organic_mineral_mass_module
        use constituent_mass_module
        use output_ls_pesticide_module
        use hydrograph_module, only : sp_ob, icmd
        use plant_module
        use pesticide_data_module
      
        implicit none 
        
        integer :: ihru            !none          !counter       
        integer :: npmx            !none          |total number of pesticides     
        integer :: ly              !none          |counter
        integer :: ipest           !none          |counter
        integer :: ipest_db        !              | 
        integer :: isp_ini         !              |
        real :: wt1                !              |
        real :: solpst             !              |
        
      !! allocate hru pesticides
      do ihru = 1, sp_ob%hru
        npmx = cs_db%num_pests
        if (npmx > 0) then
          do ly = 1, soil(ihru)%nly
            allocate (cs_soil(ihru)%ly(ly)%pest(npmx))
          end do
          allocate (cs_pl(ihru)%pest(npmx))
          allocate (cs_irr(ihru)%pest(npmx))
        end if

        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipest_db = sol_plt_ini(isp_ini)%pest
        do ipest = 1, npmx
          hpestb_d(ihru)%pest(ipest)%plant = pest_soil_ini(ipest_db)%plt(ipest)
          cs_pl(ihru)%pest(ipest) = pest_soil_ini(ipest_db)%plt(ipest)
          solpst = pest_soil_ini(ipest_db)%soil(ipest)
          do ly = 1, soil(ihru)%nly
            wt1 = soil(ihru)%phys(ly)%bd * soil(ihru)%phys(ly)%thick / 100.      !! mg/kg => kg/ha
            cs_soil(ihru)%ly(ly)%pest(ipest) = solpst * wt1
          end do
        end do

      end do    ! hru loop
                                   
      return
      end subroutine pesticide_init
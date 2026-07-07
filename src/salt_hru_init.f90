      subroutine salt_hru_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

        use hru_module, only : hru, sol_plt_ini
        use soil_module
        use organic_mineral_mass_module
        use constituent_mass_module
        use output_ls_pesticide_module
        use output_ls_salt_module
        use hydrograph_module, only : sp_ob, icmd
        use plant_module
        use pesticide_data_module
      
        implicit none 
        
        integer :: ihru            !none          !counter       
        integer :: npmx            !none          |total number of pesticides     
        integer :: ly              !none          |counter
        integer :: isalt           !none          |counter
        integer :: isalt_db        !              | 
        integer :: isp_ini         !              |
        real :: wt1                !              |

      !! allocate hru pesticides
      npmx = cs_db%num_salts
      do ihru = 1, sp_ob%hru
        if (npmx > 0) then
          do ly = 1, soil(ihru)%nly
            allocate (cs_soil(ihru)%ly(ly)%salt(npmx))
            allocate (cs_soil(ihru)%ly(ly)%salt_min(5))
          end do
          allocate (cs_pl(ihru)%salt(npmx))
          allocate (cs_irr(ihru)%salt(npmx))
        end if

        isp_ini = hru(ihru)%dbs%soil_plant_init
        isalt_db = sol_plt_ini(isp_ini)%salt
        ! loop for salt ions
        do isalt = 1, npmx
          hsaltb_d(ihru)%salt(isalt)%plant = salt_soil_ini(isalt_db)%plt(isalt)
          cs_pl(ihru)%salt(isalt) = salt_soil_ini(isalt_db)%plt(isalt)
          do ly = 1, soil(ihru)%nly
            wt1 = soil(ihru)%phys(ly)%bd * soil(ihru)%phys(ly)%thick / 100.      !! mg/kg => kg/ha
            cs_soil(ihru)%ly(ly)%salt(isalt) = salt_soil_ini(isalt_db)%soil(isalt) * wt1
            hsaltb_d(ihru)%salt(isalt)%soil = cs_soil(ihru)%ly(ly)%salt(isalt)
          end do
        end do
        
        ! loop for salt mineral fractions
        do isalt = 1, 5
          do ly = 1, soil(ihru)%nly
            cs_soil(ihru)%ly(ly)%salt_min(isalt) = salt_soil_ini(isalt_db)%soil(npmx+isalt)
          end do
        end do

      end do    ! hru loop
                                   
      return
      end subroutine salt_hru_init
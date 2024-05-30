      subroutine salt_hru_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

      use hru_module, only : hru, sol_plt_ini_cs
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use hydrograph_module, only : sp_ob, icmd
      use plant_module
      use pesticide_data_module
      use salt_module
      
      implicit none 
        
      integer :: ihru            !none          !counter       
      integer :: npmx            !none          |total number of pesticides     
      integer :: ly              !none          |counter
      integer :: isalt           !none          |counter
      integer :: isalt_db        !              | 
      integer :: isp_ini         !              |
      real :: wt1                !              |
      real :: hru_area_m2,water_volume
        
        
      !! allocate hru salts
      npmx = cs_db%num_salts
      do ihru = 1, sp_ob%hru
        if (npmx > 0) then
          do ly = 1, soil(ihru)%nly
            allocate (cs_soil(ihru)%ly(ly)%salt(npmx))
            allocate (cs_soil(ihru)%ly(ly)%salt_min(5))
            allocate (cs_soil(ihru)%ly(ly)%saltc(npmx))
          end do
          !allocate (cs_pl(ihru)%salt(npmx))
          allocate (cs_irr(ihru)%saltc(npmx))
        end if

        isp_ini = hru(ihru)%dbs%soil_plant_init
        isalt_db = sol_plt_ini_cs(isp_ini)%salt
        
        !prepare for g/m3 --> kg/ha conversion
        hru_area_m2 = hru(ihru)%area_ha * 10000.
        
        !loop through the salt ions
        do isalt=1,npmx
          !cs_pl(ihru)%salt(isalt) = salt_soil_ini(isalt_db)%plt(isalt)
          do ly = 1, soil(ihru)%nly
            !soil water salt ion concentration (mg/L)
            cs_soil(ihru)%ly(ly)%saltc(isalt) = salt_soil_ini(isalt_db)%soil(isalt) !g/m3 concentration
            !soil water salt mass (kg/ha)
            water_volume = (soil(ihru)%phys(ly)%st/1000.) * hru_area_m2
            cs_soil(ihru)%ly(ly)%salt(isalt) = (salt_soil_ini(isalt_db)%soil(isalt)/1000.) * water_volume / hru(ihru)%area_ha !g/m3 --> kg/ha
          end do
          cs_irr(ihru)%saltc(isalt) = salt_water_irr(isalt_db)%water(isalt) !g/m3 concentration
        end do
        
        ! loop for salt mineral fractions
        do isalt = 1,5
          do ly = 1,soil(ihru)%nly
            cs_soil(ihru)%ly(ly)%salt_min(isalt) = salt_soil_ini(isalt_db)%soil(npmx+isalt)
          end do
        end do

      end do !hru loop
                                   
      return
      end subroutine salt_hru_init
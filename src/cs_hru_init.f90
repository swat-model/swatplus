      !populate initial constituent data for HRU soils      
      subroutine cs_hru_init !rtb cs

      use hru_module, only : hru, sol_plt_ini
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use hydrograph_module, only : sp_ob, icmd
      use plant_module
      use pesticide_data_module
      use cs_module
      
      implicit none 
        
      integer :: ihru            !none          !counter       
      integer :: npmx            !none          |total number of pesticides     
      integer :: ly              !none          |counter
      integer :: ics             !none          |counter
      integer :: ics_db          !              | 
      integer :: isp_ini         !              |
      integer :: ipl
      real :: wt1                !              |
      real :: hru_area_m2,water_volume,soil_volume,soil_mass,mass_sorbed
        
        
      !! allocate hru cs
      npmx = cs_db%num_cs
      do ihru = 1, sp_ob%hru
        if (npmx > 0) then
          do ly = 1, soil(ihru)%nly
            allocate (cs_soil(ihru)%ly(ly)%cs(npmx))
            allocate (cs_soil(ihru)%ly(ly)%csc(npmx))
            allocate (cs_soil(ihru)%ly(ly)%cs_sorb(npmx))
            allocate (cs_soil(ihru)%ly(ly)%csc_sorb(npmx))
          end do
          allocate (cs_irr(ihru)%csc(npmx))
        end if

        isp_ini = hru(ihru)%dbs%soil_plant_init
        ics_db = sol_plt_ini(isp_ini)%cs
        
        !prepare for g/m3 --> kg/ha conversion
        hru_area_m2 = hru(ihru)%area_ha * 10000.
        
        !loop through the constituents
        do ics = 1, npmx
          !plant mass
          !cs_pl(ihru)%cs(ics) = cs_soil_ini(ics_db)%plt(ics)
          !soil water constituent concentration and mass
          do ly = 1, soil(ihru)%nly
            !soil water constituent ion concentration (mg/L)
            cs_soil(ihru)%ly(ly)%csc(ics) = cs_soil_ini(ics_db)%soil(ics) !g/m3 concentration
            !soil water constituent mass (kg/ha)
            water_volume = (soil(ihru)%phys(ly)%st/1000.) * hru_area_m2
            cs_soil(ihru)%ly(ly)%cs(ics) = (cs_soil_ini(ics_db)%soil(ics)/1000.) * water_volume / hru(ihru)%area_ha !g/m3 --> kg/ha
            !sorbed mass concentration (onto soil) (mg/kg)
            cs_soil(ihru)%ly(ly)%csc_sorb(ics) = cs_soil_ini(ics_db)%soil(ics+cs_db%num_cs)
            !sorbed mass (onto soil) (kg/ha)
            soil_volume = hru_area_m2 * (soil(ihru)%phys(ly)%thick/1000.) !m3 of soil
            soil_mass = soil_volume * (soil(ihru)%phys(ly)%bd*1000.) !kg of soil
            mass_sorbed = (cs_soil_ini(ics_db)%soil(ics+cs_db%num_cs)*soil_mass) / 1.e6 !kg of sorbed cs mass
            cs_soil(ihru)%ly(ly)%cs_sorb(ics) = mass_sorbed / hru(ihru)%area_ha !kg/ha of sorbed cs mass
          end do
          !concentration in irrigation water
          cs_irr(ihru)%csc(ics) = cs_water_irr(ics_db)%water(ics) !g/m3 concentration
        end do

      end do !hru loop
                                   
      return
      end subroutine cs_hru_init
      subroutine pl_grow
      
      use plant_data_module
      use basin_module
      use hru_module, only : ihru, ipl
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      use time_module
      use output_landscape_module
      
      implicit none 
      
      integer :: j = 0          !none               |HRU number
      integer :: idp = 0        !none               |plant number from plants.plt
 
      j = ihru
        
      call pl_nut_demand

      hpw_d(j)%bm_grow = 0.0
      hpw_d(j)%c_gro = 0.0

      do ipl = 1, pcom(j)%npl
        !! zero biomass increase and nutrient uptake
        pl_mass_up = plt_mass_z
        
        !! check for start and end of dormancy of temp-based growth plant
        if (pcom(j)%plcur(ipl)%gro == "y") then
            idp = pcom(j)%plcur(ipl)%idplt
            if (pldb(idp)%trig == "temp_gro") then
              call pl_dormant
            end if
        end if
       
        
        !! plant will not undergo stress if dormant
        if (pcom(j)%plcur(ipl)%idorm == "n" .and. pcom(j)%plcur(ipl)%gro == "y") then

          call pl_biomass_gro

          call pl_root_gro(j)

          call pl_leaf_gro
          
          call pl_leaf_senes         

          call pl_seed_gro(j)
          
          call pl_partition(j, 0)

        end if
        
        if (time%end_yr == 1) call pl_mortality

      end do    ! loop for number of plants
      
      return
      end subroutine pl_grow
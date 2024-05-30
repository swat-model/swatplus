      subroutine hru_allo
    
      use hru_module
      use hydrograph_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use reservoir_module
      use reservoir_data_module
      use carbon_module
      use plant_module
      use soil_module
      use water_body_module
      use channel_velocity_module
      use res_salt_module !rtb
      use res_cs_module !rtb
      
      implicit none

      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: ii                   !none       |hru counter

      imax = sp_ob%hru
      if (imax == 0) then
        allocate (hru(0:0))
        allocate (grwway_vel(0:0))
        allocate (soil(0:0))
        allocate (soil1(0:0))
        allocate (soil1_init(0:0))
        allocate (pl_mass(0:0))
        allocate (pcom(0:0))
        allocate (rsd1(0:0))
        allocate (cs_soil(0:0))
        allocate (cs_pl(0:0))
        allocate (cs_irr(0:0))
        allocate (irrig(0:0))
      else 
        allocate (hru(0:imax))
        allocate (grwway_vel(0:imax))
        allocate (soil(0:imax))
        allocate (soil1(0:imax))
        allocate (soil1_init(0:imax))
        allocate (pcom(0:imax))
        allocate (pl_mass(0:imax))
        allocate (cs_soil(0:imax))
        allocate (cs_pl(0:imax))
        allocate (cs_irr(0:imax))
        allocate (irrig(0:imax))
        
        allocate (wet(0:imax))
        allocate (wet_prm(0:imax))
        allocate (wet_hyd(0:imax))
        allocate (wet_om_init(0:imax))
        allocate (wet_ob(imax))
        allocate (wet_in_d(imax))
        allocate (wet_in_m(imax))
        allocate (wet_in_y(imax))
        allocate (wet_in_a(imax))
        allocate (wet_out_d(imax))
        allocate (wet_out_m(imax))
        allocate (wet_out_y(imax))
        allocate (wet_out_a(imax))
        allocate (wet_wat_d(imax))
        allocate (wet_wat_m(imax))
        allocate (wet_wat_y(imax))
        allocate (wet_wat_a(imax))
        allocate (rsd1(0:imax))
        allocate (wet_seep_day(imax))
        allocate (wet_water(imax))
        
        !rtb salt - allocate wetland arrays
        if(cs_db%num_salts > 0) then
          allocate(wetsalt_d(imax))
          allocate(wetsalt_m(imax))
          allocate(wetsalt_y(imax))
          allocate(wetsalt_a(imax))
          do ii=1,imax
            allocate(wetsalt_d(ii)%salt(cs_db%num_salts))
            allocate(wetsalt_m(ii)%salt(cs_db%num_salts))
            allocate(wetsalt_y(ii)%salt(cs_db%num_salts))
            allocate(wetsalt_a(ii)%salt(cs_db%num_salts))  
            allocate(wet_water(ii)%salt(cs_db%num_salts))
            allocate(wet_water(ii)%saltc(cs_db%num_salts))
					enddo
        endif
        
        !rtb cs - allocate wetland arrays
        if(cs_db%num_cs > 0) then
          allocate(wetcs_d(imax))
          allocate(wetcs_m(imax))
          allocate(wetcs_y(imax))
          allocate(wetcs_a(imax))
          do ii=1,imax
            allocate (wetcs_d(ii)%cs(cs_db%num_cs))
            allocate (wetcs_m(ii)%cs(cs_db%num_cs))
            allocate (wetcs_y(ii)%cs(cs_db%num_cs))
            allocate (wetcs_a(ii)%cs(cs_db%num_cs))  
            allocate (wet_water(ii)%cs(cs_db%num_cs))
            allocate (wet_water(ii)%csc(cs_db%num_cs))
					enddo
        endif
        
      endif

      return
      end subroutine hru_allo    
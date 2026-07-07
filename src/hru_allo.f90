      subroutine hru_allo
    
      use hru_module
      use hydrograph_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use reservoir_module
      use carbon_module
      use plant_module
      use soil_module
      use water_body_module
      
      implicit none

      integer :: imax                 !none       |determine max number for array (imax) and total number in file

      imax = sp_ob%hru
      if (imax == 0) then
        allocate (hru(0:0))
        allocate (soil(0:0))
        allocate (soil1(0:0))
        allocate (soil1_init(0:0))
        allocate (cbn_loss(0:0))
        allocate (pl_mass(0:0))
        allocate (pcom(0:0))
        allocate (rsd1(0:0))
        allocate (cs_soil(0:0))
        allocate (cs_pl(0:0))
        allocate (cs_irr(0:0))
        allocate (irrig(0:0))
      else 
        allocate (hru(0:imax))
        allocate (soil(0:imax))
        allocate (soil1(0:imax))
        allocate (soil1_init(0:imax))
        allocate (cbn_loss(0:imax))
        allocate (pcom(0:imax))
        allocate (pl_mass(0:imax))
        allocate (cs_soil(0:imax))
        allocate (cs_pl(0:imax))
        allocate (cs_irr(0:imax))
        allocate (irrig(0:imax))
        
        allocate (wet(0:imax))
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
      endif

      return
      end subroutine hru_allo    
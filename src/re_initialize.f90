      subroutine re_initialize
    
      use hru_module, only : hru, hru_init, bss
      use soil_module
      use plant_module
      use organic_mineral_mass_module
      use mgt_operations_module
      use hydrograph_module
      use hru_lte_module
      use sd_channel_module
      use aquifer_module
      
      implicit none

      !! reset basin soil water for next simulation
      pco%sw_init = "n"
      
      !! re-initialize all hru parameters
      if (sp_ob%hru > 0) then
        hru = hru_init
        soil = soil_init
        soil1 = soil1_init
        rsd1 = rsd1_init
        pcom = pcom_init
        pl_mass = pl_mass_init
        wet = wet_om_init
        bss = 0.
      end if
      
      !! re-initialize hru_lte parameters
      if (sp_ob%hru_lte > 0) then
        hlt = hlt_init
      end if
      
      !! re-initialize channel lte storage and dimensions
      if (sp_ob%chandeg > 0) then
        sd_ch = sdch_init
        ch_stor = ch_om_water_init
        fp_stor = fp_om_water_init
      end if
      
      !! re-initialize reservoir storage
      if (sp_ob%res > 0) then
        res = res_om_init
      end if
      
      !! re-initialize aquifer storage
      if (sp_ob%aqu > 0) then
        aqu_om_init = aqu_om_init
      end if

      return
      end subroutine re_initialize
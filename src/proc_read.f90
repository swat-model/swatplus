      subroutine proc_read
     
      use constituent_mass_module
    
      implicit none
             
      call ch_read_temp
      call cli_read_atmodep
      call cli_staread

      call constit_db_read
      call pest_metabolite_read     !! read pesticide metabolites
      call soil_plant_init
      call soil_plant_init_cs
      call solt_db_read
      call pest_hru_aqu_read
      call path_hru_aqu_read
      call hmet_hru_aqu_read
      
      !rtb salt
      if(cs_db%num_salts > 0) then
        call salt_hru_read
        call salt_aqu_read
        call salt_irr_read
        call salt_plant_read
        call cli_read_atmodep_salt
        call salt_roadsalt_read
        call salt_uptake_read
        call salt_urban_read
        call salt_fert_read
      endif

      !rtb cs
      if(cs_db%num_cs > 0) then
        call cs_hru_read
        call cs_aqu_read
        call cli_read_atmodep_cs
        call cs_irr_read
        call cs_plant_read
        call cs_uptake_read
        call cs_reactions_read
        call cs_urban_read
        call cs_fert_read
      endif
      
      call topo_read
      call field_read
      call hydrol_read
			
			call shade_factor_read   
      
      call snowdb_read
      call soil_db_read
      call soil_lte_db_read
      
      return
      
      end subroutine proc_read
      subroutine proc_hru
    
      use hydrograph_module
      use maximum_data_module
      use hru_module
      use soil_module
      use constituent_mass_module
      use landuse_data_module
      use erosion_module
    
      implicit none
      
      integer :: j              !none       |counter
      integer :: ilum           !none       |counter

       !! set the object number for each hru-to point to weather station
      if (sp_ob%hru > 0) then
        call hru_allo
        call hru_read    
        call hrudb_init
        call hru_lum_init_all
        call topohyd_init
        call hru_output_allo
        call carbon_read
        
        !! septic has to be set before soils are initialized (making a soil layer for the septic zone)
        do j = 1, sp_ob%hru
          ilum = hru(j)%land_use_mgt
          if (lum(ilum)%septic /= "null") then
            call structure_set_parms("septic          ", lum_str(ilum)%septic, j)
          end if
        end do
        
        call soils_init
        call structure_init
        call plant_all_init
        call cn2_init_all
        call hydro_init
        if (cs_db%num_pests > 0) call pesticide_init
        if (cs_db%num_paths > 0) call pathogen_init
        if (cs_db%num_salts > 0) call salt_hru_init !rtb salt
        if (cs_db%num_cs > 0) call cs_hru_init !rtb cs
        
      !! allocate erosion output and open file
      allocate (ero_output(sp_ob%hru))
      open (4001,file = "erosion.out",recl=1200)
      write (4001,*) bsn%name, prog
      write (4001,*) ero_hdr
      write (4001,*) ero_hdr_units
            
!!!!! new checker.out file - always prints
      open (4000,file = "checker.out",recl=1200)
      write (4000,*) bsn%name, prog
      write (4000,*) chk_hdr
      write (4000,*) chk_unit
      write (9000,*) "CHK                       checker.out"

      do j = 1, sp_ob%hru
         write (4000,100) soil(j)%snam, soil(j)%hydgrp, soil(j)%zmx, soil(j)%usle_k, soil(j)%sumfc,  &
            soil(j)%sumul, hru(j)%lumv%usle_p, hru(j)%lumv%usle_ls, hru(j)%hyd%esco, hru(j)%hyd%epco,      &
            hru(j)%hyd%cn3_swf, hru(j)%hyd%perco, hru(j)%hyd%latq_co, hru(j)%tiledrain  
      end do
!!!!! new checker.out file - always prints
        
      end if

      call rte_read_nut
      
100   format(2a16,11f12.4,i12)
       
	  return
      
      end subroutine proc_hru
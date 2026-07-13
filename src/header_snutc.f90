     subroutine header_snutc
    
     use hydrograph_module
     use soil_nutcarb_module
     use output_path_module
     
     implicit none 

!!write all organic carbon for the soil profile
     if (sp_ob%hru > 0) then
        call open_output_file(2610, "hru_orgc.txt", 800)
        write (2610,*) bsn%name, prog
        write (9000,*) "HRU_ORGC                  hru_orgc.txt"
        write (2610,*) orgc_hdr
        write (2610,*) orgc_units
        
!! write total carbon for the soil profile, plants, and residue
     !if (sp_ob%hru > 0) then
        call open_output_file(2611, "hru_totc.txt", 800)
        write (2611,*) bsn%name, prog
        write (2611,*) "HRU_TOTC                  hru_totc.txt"
        write (2611,*) totc_hdr
        write (2611,*) totc_units
        
!! write all carbon, organic n and p, and mineral n and p for the soil profile, plants, and residue
     !if (sp_ob%hru > 0) then
        call open_output_file(2613, "basin_totc.txt", 800)
        write (2613,*) bsn%name, prog
        write (2613,*) "BSN_TOTC                basin_totc.txt"
        write (2613,*) totc_hdr
        write (2613,*) totc_units
    end if
     
      return
      end subroutine header_snutc  
      subroutine proc_res
    
      use hydrograph_module
    
      implicit none
      
      external :: res_allo, res_initial, res_objects, res_read, res_read_conds, res_read_csdb, &
                  res_read_hyd, res_read_init, res_read_nut, res_read_salt_cs, res_read_saltdb, &
                  res_read_sed, dtbl_res_read, res_read_weir, wet_all_initial, wet_fp_init, wet_read, &
                  wet_read_hyd, wet_read_salt_cs
                
      !! allocate and initialize reservoir variables
      call res_read_hyd
      call res_read_sed
      call res_read_nut
      !call res_read_weir 
      call res_read_init
      
      !read database reservoir concentrations
      call res_read_saltdb !rtb salt
      call res_read_csdb !rtb cs
      
      call res_read_conds   !! Osvaldo
        
      if (sp_ob%res > 0) then
        call res_allo
        call res_objects
      
        ! read reservoir data
        call res_read
        call res_read_salt_cs
        call res_initial
      end if

      return
      
      end subroutine proc_res
      subroutine proc_res
    
      use hydrograph_module
    
      implicit none
                
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
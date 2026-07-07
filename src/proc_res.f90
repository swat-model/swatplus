      subroutine proc_res
    
      use hydrograph_module
    
      implicit none
                
      !! allocate and initialize reservoir variables
      call res_read_hyd
      call res_read_sed
      call res_read_nut
      call res_read_weir
      call res_read_init
        
      if (sp_ob%res > 0) then
        call res_allo
        call res_objects
      
        ! read reservoir data
        call res_read
        call res_initial
      end if

	  return
      
      end subroutine proc_res
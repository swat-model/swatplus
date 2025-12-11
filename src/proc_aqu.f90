      subroutine proc_aqu
    
      use hydrograph_module
      
      implicit none
      
      external :: aqu_initial, aqu_read, aqu_read_init, aqu_read_init_cs

      call aqu_read
      call aqu_initial
      call aqu_read_init
      call aqu_read_init_cs
      
      return
      
      end subroutine proc_aqu
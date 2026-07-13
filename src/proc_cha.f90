      subroutine proc_cha
    
      use hydrograph_module
         
      implicit none
      
      external :: aqu2d_init, ch_initial, ch_read, ch_read_hyd, ch_read_init, ch_read_init_cs, ch_read_nut, &
                  ch_read_sed, ch_ttcoef, overbank_read, sd_channel_read, sd_channel_surf_link, &
                  sd_hydsed_init, sd_hydsed_read, time_conc_init, cli_lapse
      
      integer :: irch = 0               !              |
      integer :: idat = 0               !              |
      integer :: i = 0                  !none          |counter
         
      call ch_read_init
      call ch_read_init_cs

      call sd_hydsed_read
      call ch_read_hyd
      call ch_read_sed
      call ch_read_nut
      call ch_read
      call sd_channel_read
      call sd_hydsed_init

      !call channel_allo
          
      !! initialize stream-aquifer interactions for geomorphic baseflow
      !! aquifer to channel flow
      call aqu2d_init
      
      do ich = 1, sp_ob%chan
        !! initialize flow routing variables
        call ch_ttcoef (ich)
      end do
         
      do irch = 1, sp_ob%chan
        i = sp_ob1%chan + irch - 1 
        idat = ob(i)%props
        call ch_initial (idat, irch)
      end do
      
      !set parms for sd-channel-landscape linkage
      call overbank_read
      call sd_channel_surf_link
      
      call time_conc_init

      return
      
      end subroutine proc_cha
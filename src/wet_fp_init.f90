      subroutine wet_fp_init
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes computes the intial storage in flood plain wetlands

      use sd_channel_module
      use hydrograph_module
      
      
      implicit none
      
      integer :: ihru       !none       |hru number

      !! total wetland flood plain volume at start of simulation
      do jrch = 1, sp_ob%chandeg
        wet_stor(jrch) = hz
        if (sd_ch(jrch)%fp%hru_tot > 0) then
          do ihru = 1, sd_ch(jrch)%fp%hru_tot
            wet_stor(jrch) = wet_stor(jrch) + wet(ihru)
          end do
        end if
      end do
        
      return
      end subroutine wet_fp_init
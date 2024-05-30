      subroutine cli_lapse
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adjusts precip and temperature for elevation

      use basin_module
      use climate_module
      use hydrograph_module, only : ob, sp_ob
      
      implicit none

      integer :: iob        !       |object number
      integer :: iwst       !       |weather station number
      integer :: iwgn       !       |weather generator station number
      integer :: igage      !       |number of measured precip or temperature gage
      
      !! set precip and temp lapse for each object
      do iob = 1, sp_ob%objs
        iwst = ob(iob)%wst
        !! adjust precip and temperature for elevation using lapse rates
        if (wst(iwst)%wco_c%pgage == "sim") then
          iwgn = wst(iwst)%wco%wgn
          ob(iob)%plaps = bsn_prm%plaps * (ob(iob)%elev - wgn(iwgn)%elev) / 1000.
        else
          igage = wst(iwst)%wco%pgage
          ob(iob)%plaps = bsn_prm%plaps * (ob(iob)%elev - pcp(igage)%elev) / 1000.
        end if
        if (wst(iwst)%wco_c%tgage == "sim") then
          iwgn = wst(iwst)%wco%wgn
          ob(iob)%tlaps = bsn_prm%tlaps * (wgn(iwgn)%elev - ob(iob)%elev) / 1000.
        else
          igage = wst(iwst)%wco%tgage
          ob(iob)%tlaps = bsn_prm%tlaps * (tmp(igage)%elev - ob(iob)%elev) / 1000.
        end if
      end do
      
      return
      end subroutine cli_lapse
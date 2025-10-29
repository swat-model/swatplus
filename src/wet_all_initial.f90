      subroutine wet_all_initial
      
      use hru_module, only : hru
      use hydrograph_module, only : sp_ob, wet, wet_om_init
      
      implicit none
      
      integer :: iihru = 0      !              |hru counter
      integer :: iprop = 0      !none          |pointer to surface storage data 

      do iihru = 1, sp_ob%hru
        !! set initial volumes and convert units
        iprop = hru(iihru)%dbs%surf_stor
        !if (iprop > 0) then
        if (hru(iihru)%dbsc%surf_stor /= "null") then
          call wet_initial (iihru)
          wet_om_init(iihru) = wet(iihru)
        end if
      end do

      return
      end subroutine wet_all_initial
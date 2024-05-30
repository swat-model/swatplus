      subroutine hru_lum_init_all
    
      use hru_module, only : hru
      use hydrograph_module, only : sp_ob
      
      implicit none
 
      integer :: iihru              !           |hru number

      do iihru = 1, sp_ob%hru
        hru(iihru)%land_use_mgt = hru(iihru)%dbs%land_use_mgt
        call hru_lum_init (iihru)
      end do

      return
      end subroutine hru_lum_init_all
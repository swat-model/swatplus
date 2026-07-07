      subroutine hru_lum_init_all
    
      use hru_module, only : hru
      use plant_module, only : pcom
      use landuse_data_module, only : lum, lum_str, lum_grp
      use hydrograph_module, only : ob, sp_ob
      use climate_module, only : wst
      
      implicit none
 
      integer :: iihru              !           |hru number
      integer :: iob                !           |spatial object number
      integer :: ilu                !none       |land use number 
      integer :: ilug               !none       |counter 
      integer :: isched             !           |management schedule number
      integer :: iwst               !           |weather station number
      integer :: iwgn               !           |weather generator number

      do iihru = 1, sp_ob%hru
        hru(iihru)%land_use_mgt = hru(iihru)%dbs%land_use_mgt
        call hru_lum_init (iihru)
      end do

      return
      end subroutine hru_lum_init_all
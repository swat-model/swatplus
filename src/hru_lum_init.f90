      subroutine hru_lum_init (iihru)
    
      use hru_module, only : hru
      use plant_module, only : pcom
      use landuse_data_module, only : lum, lum_str, lum_grp
      use hydrograph_module, only : ob
      use climate_module, only : wst
      
      implicit none

      integer, intent (in)  :: iihru    !none       |hru number
      integer :: iob                    !           |spatial object number
      integer :: ilu                    !none       |land use number 
      integer :: ilug                   !none       |counter 
      integer :: isched                 !           |management schedule number
      integer :: iwst                   !           |weather station number
      integer :: iwgn                   !           |weather generator number

        !!assign land use pointers for the hru
        ilu = hru(iihru)%land_use_mgt
        pcom(iihru)%name = lum(ilu)%plant_cov
        hru(iihru)%plant_cov = lum_str(ilu)%plant_cov
        hru(iihru)%lum_group_c = lum(ilu)%cal_group
        do ilug = 1, lum_grp%num
          if (hru(iihru)%lum_group_c == lum_grp%name(ilu)) then
            hru(iihru)%lum_group =  ilug
          end if
        end do
        iob = hru(iihru)%obj_no
        iwst = ob(iob)%wst
        iwgn = wst(iwst)%wco%wgn
        isched = lum_str(ilu)%mgt_ops
        hru(iihru)%mgt_ops = lum_str(ilu)%mgt_ops
        hru(iihru)%tiledrain = lum_str(ilu)%tiledrain
        hru(iihru)%septic = lum_str(ilu)%septic
        hru(iihru)%fstrip = lum_str(ilu)%fstrip
        hru(iihru)%grassww = lum_str(ilu)%grassww
        hru(iihru)%bmpuser = lum_str(ilu)%bmpuser
        hru(iihru)%luse%cn_lu = lum_str(ilu)%cn_lu
        hru(iihru)%luse%cons_prac = lum_str(ilu)%cons_prac

      return
      end subroutine hru_lum_init
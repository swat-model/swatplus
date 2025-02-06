      subroutine hrudb_init
    
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      use hru_module, only : hru, hru_db
      use landuse_data_module
      use basin_module
      
      implicit none

      integer :: imp = 0              !           |
      integer :: ihru = 0             !none       |counter 
      integer :: iob = 0              !           |
      integer :: ihru_db = 0          !           | 
      integer :: ilu = 0          !           | 


      !!assign database pointers for the hru
      imp = 0
      do ihru = 1, sp_ob%hru
        iob = sp_ob1%hru + ihru - 1
        ihru_db = ob(iob)%props    !points to hru.dat
        hru(ihru)%dbs = hru_db(ihru_db)%dbs
        hru(ihru)%dbsc = hru_db(ihru_db)%dbsc
        hru(ihru)%obj_no = sp_ob1%hru + ihru - 1
        hru(ihru)%area_ha = ob(iob)%area_ha
        hru(ihru)%km = ob(iob)%area_ha / 100.
        hru(ihru)%land_use_mgt_c = hru_db(ihru_db)%dbsc%land_use_mgt
        ilu = hru(ihru)%dbs%land_use_mgt
        hru(ihru)%cal_group = lum(ilu)%cal_group
        hru(ihru)%nut%phoskd = bsn_prm%phoskd
        hru(ihru)%nut%pperco = bsn_prm%pperco
        hru(ihru)%nut%psp = bsn_prm%psp
        hru(ihru)%nut%nperco = bsn_prm%nperco
        hru(ihru)%nut%cmn = bsn_prm%cmn
        hru(ihru)%nut%nperco_lchtile = bsn_prm%nperco_lchtile
      end do

      return
      end subroutine hrudb_init
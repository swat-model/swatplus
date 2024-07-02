      subroutine hrudb_init
    
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      use hru_module, only : hru, hru_db
      
      implicit none

      integer :: imp                  !           |
      integer :: ihru                 !none       |counter 
      integer :: iob                  !           |
      integer :: ihru_db              !           | 

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
      end do

      return
      end subroutine hrudb_init
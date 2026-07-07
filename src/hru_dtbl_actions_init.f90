      subroutine hru_dtbl_actions_init
    
      use conditional_module
      use mgt_operations_module
      use hydrograph_module
      use hru_module, only : hru
      use plant_module, only : pcom, pcom_init
      
      implicit none

      integer :: id                   !none       |counter
      integer :: iauto                !none       |counter
      integer :: ihru                 !none       |counter
      integer :: iihru                !none       |counter
      integer :: isched               !none       |counter
      
      ! set arrays for counters for dtbl actions (ie: only 1 planting; 2 fert application per year, etc)
      do iihru = 1, sp_ob%hru
        ihru = sp_ob1%hru + iihru - 1
        isched = hru(ihru)%mgt_ops
        if (sched(isched)%num_autos > 0) then
          allocate (pcom(ihru)%dtbl(sched(isched)%num_autos))
        
          do iauto = 1, sched(isched)%num_autos
            id = sched(isched)%num_db(iauto)
            allocate (pcom(ihru)%dtbl(iauto)%num_actions(dtbl_lum(id)%acts))
            pcom(ihru)%dtbl(iauto)%num_actions = 1
            allocate (pcom(ihru)%dtbl(iauto)%days_act(dtbl_lum(id)%acts))
            pcom(ihru)%dtbl(iauto)%days_act = 0
          end do
        end if
      end do
      
      return
      end subroutine hru_dtbl_actions_init
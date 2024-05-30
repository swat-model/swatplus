      subroutine proc_cond

      use hru_module, only : hru, ihru
      use mgt_operations_module
      use hydrograph_module
      use maximum_data_module
      use conditional_module

      implicit none
    
      integer :: isched                 !              | 
      integer :: iauto                  !none          |counter
      integer :: ictl                   !none          |counter
          
      !! set cross walk for auto management operations
      do ihru = 1, sp_ob%hru
        isched = hru(ihru)%mgt_ops
        if (sched(isched)%num_autos > 0) then
           sched(isched)%irr = 1
           !! crosswalk with conditional.ctl
           do iauto = 1, sched(isched)%num_autos
             do ictl = 1, db_mx%dtbl_lum
               if (sched(isched)%auto_name(iauto) == dtbl_lum(ictl)%name) then
                 sched(isched)%num_db(iauto) = ictl
               end if
             end do
           end do
         end if
      end do
    
	  return
      
      end subroutine proc_cond
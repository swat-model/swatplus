       subroutine proc_res
      
       use hydrograph_module
       use reservoir_module, only : res_ob
       use reservoir_seepage_module, only : hru_soil_capacity_1m_m3
      
       implicit none

       integer :: ires
       integer :: ireceiver
       integer :: ihru
       real :: capacity_m3
                 
       !! allocate and initialize reservoir variables
       call res_read_hyd
       call res_read_sed
       call res_read_nut
       call res_read_weir
       call res_read_init
         
       if (sp_ob%res > 0) then
         call res_allo
         call res_objects
       
         ! read reservoir data
         call res_read
         call res_read_seep_hru
         call res_initial

         !! Diagnostic: report available soil storage for mapped HRUs.
         do ires = 1, sp_ob%res
           do ireceiver = 1, res_ob(ires)%n_seep_hru
             ihru = res_ob(ires)%seep_hru(ireceiver)%hru_id
             capacity_m3 = hru_soil_capacity_1m_m3(ihru)

             write(*,'(a,i0,a,i0,a,f14.3)') &
               "Reservoir seepage capacity: res=", ires, &
               " hru=", ihru, &
               " capacity_m3=", capacity_m3
           end do
         end do
       end if

       return
      
       end subroutine proc_res

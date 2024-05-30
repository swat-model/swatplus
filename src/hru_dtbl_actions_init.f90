      subroutine hru_dtbl_actions_init
    
      use conditional_module
      use mgt_operations_module
      use hydrograph_module
      use hru_module, only : hru
      use plant_module, only : pcom
      use maximum_data_module
      use fertilizer_data_module
      
      implicit none

      integer :: id                   !none       |counter
      integer :: iauto                !none       |counter
      integer :: ihru                 !none       |counter
      integer :: iihru                !none       |counter
      integer :: isched               !none       |counter
      integer :: num_fut              !none       |counter
      integer :: iac                  !none       |counter
      integer :: idb                  !none       |counter
      integer :: m_autos              !none       |counter
      
      ! set arrays for counters for dtbl actions (ie: only 1 planting; 2 fert application per year, etc)
      do iihru = 1, sp_ob%hru
        ihru = sp_ob1%hru + iihru - 1
        isched = hru(ihru)%mgt_ops
        m_autos = sched(isched)%num_autos
        
        !! add decision table for water allocation irrigation demand
        if (hru(ihru)%irr_dmd_dtbl > 0) then
          m_autos = m_autos + 1
          hru(ihru)%irr_dmd_iauto = m_autos
        end if
        
        !! add decision table for manure allocation demand
        if (hru(ihru)%man_dmd_dtbl > 0) then
          m_autos = m_autos + 1
          hru(ihru)%man_dmd_iauto = m_autos
        end if
        
        if (m_autos > 0) then
          allocate (pcom(ihru)%dtbl(m_autos))
        
          do iauto = 1, m_autos
            if (iauto /= hru(ihru)%irr_dmd_iauto .and. iauto /= hru(ihru)%man_dmd_iauto) then
              id = sched(isched)%num_db(iauto)
            end if
            if (iauto == hru(ihru)%irr_dmd_iauto) then
              !! dtbl from water allocation for irrigation demand
              id = hru(ihru)%irr_dmd_dtbl
            end if
            if (iauto == hru(ihru)%man_dmd_iauto) then
              !! dtbl from water allocation for irrigation demand
              id = hru(ihru)%man_dmd_dtbl
            end if
            allocate (pcom(ihru)%dtbl(iauto)%num_actions(dtbl_lum(id)%acts))
            pcom(ihru)%dtbl(iauto)%num_actions = 1
            allocate (pcom(ihru)%dtbl(iauto)%days_act(dtbl_lum(id)%acts))
            pcom(ihru)%dtbl(iauto)%days_act = 0
          
          !! set variables for future fertilizer operations
          num_fut = 0
          do iac = 1, dtbl_lum(id)%acts
            if (dtbl_lum(id)%act(iac)%typ == "fert_future") num_fut = num_fut + 1
          end do
          if (num_fut > 0) then
            allocate (pcom(ihru)%fert_fut(num_fut))
            pcom(ihru)%fert_fut_num = num_fut
          end if
          
          num_fut = 0
          do iac = 1, dtbl_lum(id)%acts
            if (dtbl_lum(id)%act(iac)%typ == "fert_future") then
              num_fut = num_fut + 1
              pcom(ihru)%fert_fut(num_fut)%num = dtbl_lum(id)%act(iac)%ob_num
              pcom(ihru)%fert_fut(num_fut)%name = dtbl_lum(id)%act(iac)%name
              pcom(ihru)%fert_fut(num_fut)%fertname = dtbl_lum(id)%act(iac)%option
              pcom(ihru)%fert_fut(num_fut)%fert_kg = dtbl_lum(id)%act(iac)%const
              pcom(ihru)%fert_fut(num_fut)%day_fert = 0
              pcom(ihru)%fert_fut(num_fut)%fertop = dtbl_lum(id)%act(iac)%file_pointer
              
              !xwalk fert name with fertilizer data base
              do idb = 1, db_mx%fertparm
                if (dtbl_lum(id)%act(iac)%option == fertdb(idb)%fertnm) then
                  pcom(ihru)%fert_fut(num_fut)%fertnum = idb
                  exit
                endif
              end do
              !xwalk application type with chemical application data base
              do idb = 1, db_mx%chemapp_db
                if (dtbl_lum(id)%act(iac)%file_pointer == chemapp_db(idb)%name) then
                  pcom(ihru)%fert_fut(num_fut)%appnum = idb
                  exit
                endif
              end do
            end if            
          end do    ! iac - actions loop
          
        end do      ! iauto loop
        end if      ! num_autos > 0
      end do        ! hru loop
      
      return
      end subroutine hru_dtbl_actions_init
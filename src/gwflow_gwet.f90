      subroutine gwflow_gwet !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the volume of groundwater that is removed from the
!!    aquifer via ET
      
      use gwflow_module   
      use maximum_data_module, only : db_mx
      use calibration_data_module, only : lsu_out
      use hydrograph_module, only : sp_ob1,ob,sp_ob
      
      implicit none

      integer :: i                          !       |counter
      integer :: j                          !       |counter
      integer :: k                          !       |counter
      integer :: s                          !       |solute counter
      integer :: cell_id                    !       |gwflow cell
      integer :: hru_id                     !       |id of HRU
      integer :: ob_num											!       |object number of HRU
      real :: hru_gwet_volume               !m3     |gwet volume for HRU
      real :: lsu_gwet_volume               !m3     |gwet volume for landscape unit
      real :: max_gwet                      !m3     |potential ET for cell
      real :: et_surface                    !m      |ground surface
      real :: et_bottom                     !m      |lower elevation bound for ET to occur
      real :: gw_head                       !m      |groundwater head of the cell
      real :: gwet                          !mm			|depth of groundwater ET for cell
      real :: gwet_volume                   !m3     |actual ET for cell
      
      
      
      !check LSU-cell connection
      if (lsu_cells_link == 1) then 
      
        !loop through the landscape units
        do k=1,db_mx%lsu_out
          !sum remaining ET (m3) for each LSU (based on collection of HRUs within the LSU)
          lsu_gwet_volume = 0.
          do j=1,lsu_out(k)%num_tot
            hru_id = lsu_out(k)%num(j)
            ob_num = sp_ob1%hru + hru_id - 1
            hru_gwet_volume = (etremain(hru_id)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
            lsu_gwet_volume = lsu_gwet_volume + hru_gwet_volume !m3
          enddo
          !map remaining ET from LSU to grid cells connected to the LSU
          do i=1,lsu_num_cells(k)
            max_gwet = lsu_gwet_volume * lsu_cells_fract(k,i) !m3 for current cell
            cell_id = lsu_cells(k,i)
            et_surface = gw_state(cell_id)%elev !ground surface (m)
            et_bottom = et_surface - gw_state(cell_id)%exdp !lower elevation bound for ET to occur (m)
            gw_head = gw_state(cell_id)%head !m
            gwet_volume = 0.
            if(gw_head < et_bottom) then
              gwet_volume = 0. !below the extinction depth
            elseif(gw_head > et_surface) then
              gwet_volume = max_gwet !m3
            else
              if(gw_state(cell_id)%exdp.ne.0) then
                gwet_volume = max_gwet * (gw_head - et_bottom) / (et_surface - et_bottom) !vary ET linearly (m3)
              else
                gwet_volume = 0.
              endif
            endif
            !check for available groundwater in the cell - can only remove what is there
            if(gw_state(cell_id)%head.gt.gw_state(cell_id)%botm) then
              if(gwet_volume.ge.gw_state(cell_id)%stor) then
                gwet_volume = gw_state(cell_id)%stor
              endif
            else
              gwet_volume = 0.
            endif
            gw_ss(cell_id)%gwet = gw_ss(cell_id)%gwet + (gwet_volume*(-1)) !(negative --> leaving the aquifer)
            gw_ss_sum(cell_id)%gwet = gw_ss_sum(cell_id)%gwet + (gwet_volume*(-1))
            gw_state(cell_id)%stor = gw_state(cell_id)%stor - gwet_volume 
          enddo
        enddo !go to next LSU
        
      else !proceed with HRU-cell connection  
      
      ob_num = sp_ob1%hru  !object number of first HRU
      do k=1,sp_ob%hru
        max_gwet = etremain(k) !maximum ET rate from the water table (mm)
        do i=1,hru_num_cells(k)
					max_gwet = max_gwet * hru_cells_fract(k,i) !mm
          cell_id = hru_cells(k,i)
          et_surface = gw_state(cell_id)%elev !ground surface
          et_bottom = et_surface - gw_state(cell_id)%exdp !lower elevation bound for ET to occur
          gw_head = gw_state(cell_id)%head
          gwet = 0.
          if(gw_head < et_bottom) then
            gwet = 0. !below the extinction depth
          elseif(gw_head > et_surface) then
            gwet = max_gwet
          else
            if(gw_state(cell_id)%exdp.ne.0) then
              gwet = max_gwet * (gw_head - et_bottom) / (et_surface - et_bottom) !vary ET linearly (mm)
            else
              gwet = 0.
            endif
          endif
          gwet_volume = (gwet/1000.) * (ob(ob_num)%area_ha * 10000.) !m3 of groundwater
          !check for available groundwater in the cell - can only remove what is there
          if(gw_state(cell_id)%head.gt.gw_state(cell_id)%botm) then
            if(gwet_volume.ge.gw_state(cell_id)%stor) then
              gwet_volume = gw_state(cell_id)%stor
            endif
          else
            gwet_volume = 0.
          endif
          gw_ss(cell_id)%gwet = gw_ss(cell_id)%gwet + (gwet_volume*(-1)) !(negative --> leaving the aquifer)
          gw_ss_sum(cell_id)%gwet = gw_ss_sum(cell_id)%gwet + (gwet_volume*(-1))
          gw_state(cell_id)%stor = gw_state(cell_id)%stor - gwet_volume 
        enddo
        ob_num = ob_num + 1
      enddo
      
      endif !check for LSU-cell connection
      
      return
      end subroutine gwflow_gwet 
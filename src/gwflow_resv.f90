      subroutine gwflow_resv(res_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between the reservoir and the connected grid cells
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use hydrograph_module, only : res
      use water_body_module, only: res_wat_d
      use constituent_mass_module
      
      implicit none

      integer, intent (in) :: res_id		 !       |reservoir number
      integer :: k                       !       |cell counter (cells connected to the reservoir object)
      integer :: jj                      !       |cell counter (cells connected to the reservoir cell)
      integer :: s                       !       |solute counter
      integer :: res_cell_id             !       |id of cell that is connected to the reservoir object
      integer :: cell_id								 !       |id of cell that is connected to the reservoir cell
      integer :: isalt                   !       |salt ion counter
      integer :: ics                     !       |constituent counter
      integer :: sol_index
      real :: area_res_cell              !m2     |spatial area of the cell that is in connection with the reservoir
      real :: area_cell                  !m2     |spatial area of the cell connected to the reservoir cell
      real :: min_area									 !m2     |minimum of the two cell areas
      real :: head_diff                  !m      |head difference between reservoir and aquifer
      real :: Q                          !m3     |water volume exchange between cell and reservoir
      real :: conn_length                !m      |length of connection between reservoir cell and adjacent cell
      real :: res_volume                 !m3     |water volume in reservoir before groundwater exchange occurs
      real :: resv_csol(100)             !g/m3   |solute concentration in reservoir water
      real :: solmass(100)               !g      |solute mass transferred

      
      
      !only proceed if reservoir-cell exchange is active
      if (gw_res_flag == 1) then
      
        !record starting reservoir volume (m3)
        res_volume = res(res_id)%flo

        !loop through the cells connected to the reservoir
        do k=1,gw_resv_info(res_id)%ncon
          res_cell_id = gw_resv_info(res_id)%cells(k) !id of reservoir cell
        
          !loop through the cells connected to the cell
          do jj=1,gw_state(res_cell_id)%ncon
            
            !id of cell connected to the reservoir cell
            cell_id = cell_con(res_cell_id)%cell_id(jj)
            
            !only proceed if the cell is active; is so, calculate horizontal exchange (m3/day)
            if(gw_state(cell_id)%stat == 1) then
              Q = 0.
              
              !length of connection between cells
              area_res_cell = gw_state(res_cell_id)%area !m2
              area_cell = gw_state(cell_id)%area !m2
              min_area = min(area_res_cell,area_cell)
              conn_length = sqrt(min_area)
              
              !exchange volume (m3/day) using Darcy's Law
              head_diff = gw_resv_info(res_id)%elev(k) - gw_state(cell_id)%head	
              res_K = gw_resv_info(res_id)%hydc(k)
              res_thick = gw_resv_info(res_id)%thck(k)
              Q = res_K * (head_diff / res_thick) * (res_thick * conn_length)	
              
              !check against available storage volumes (m3)
              if(Q > 0) then !reservoir --> aquifer
                if(Q > res(res_id)%flo) then !can only remove what is there
                  Q = res(res_id)%flo
                endif
              elseif(Q < 0) then !aquifer --> reservoir
                !if((Q*-1 == 1).ge.gw_state(cell_id)%stor) then
                if (-Q .ge.gw_state(cell_id)%stor) then
                  !Q = gw_state(cell_id)%stor * (-1)
                  Q = -gw_state(cell_id)%stor	
                  gw_state(cell_id)%stor = gw_state(cell_id)%stor + Q	
                endif	
              endif

              !store for gwflow water balance calculations (in gwflow_simulate)
              gw_ss(cell_id)%resv = gw_ss(cell_id)%resv + Q 
              gw_ss_sum(cell_id)%resv = gw_ss_sum(cell_id)%resv + Q
              
              !store seepage value for reservoir object
              res_wat_d(res_id)%seep = Q
              
              !calculate solute mass (g/day) transported to/from cell
              if (gw_solute_flag == 1) then
                resv_csol = 0.
                solmass = 0.
                if(Q < 0) then !mass leaving the cell (aquifer --> reservoir)
                  do s=1,gw_nsolute !loop through the solutes
                    solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                    if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                      solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                    endif
                    gwsol_ss(cell_id)%solute(s)%resv = solmass(s)
                    gwsol_ss_sum(cell_id)%solute(s)%resv = gwsol_ss_sum(cell_id)%solute(s)%resv + solmass(s)
                  enddo    
                  !add solute to reservoir
                  res(res_id)%no3 = res(res_id)%no3 + (solmass(1)*(-1)/1000.) !kg
                  res(res_id)%solp = res(res_id)%solp + (solmass(2)*(-1)/1000.) !kg
                  sol_index = 2
                  !salts
                  if (gwsol_salt == 1) then
                    do isalt=1,cs_db%num_salts
                      sol_index = sol_index + 1
                      res_water(res_id)%salt(isalt) = res_water(res_id)%salt(isalt) + (solmass(sol_index)*(-1)/1000.) !kg   
                    enddo
                  endif
                  !constituents
                  if (gwsol_cons == 1) then
                    do ics=1,cs_db%num_cs
                      sol_index = sol_index + 1
                      res_water(res_id)%cs(ics) = res_water(res_id)%cs(ics) + (solmass(sol_index)*(-1)/1000.) !kg  
                    enddo
                  endif
                else !mass entering cell (reservoir --> aquifer)
                  !calculate solute concentrations in reservoir water
                  if(res_volume > 10.) then
                    !no3-n
                    if(res(res_id)%no3 > 0.) then
                      resv_csol(1) = (res(res_id)%no3 * 1000.) / res_volume !g/m3 in reservoir  
                    endif
                    !p
                    if(res(res_id)%solp > 0.) then
                      resv_csol(2) = (res(res_id)%solp * 1000.) / res_volume !g/m3 in reservoir 
                    endif
                    sol_index = 2
                    !salts
                    if (gwsol_salt == 1) then
                      do isalt=1,cs_db%num_salts
                        sol_index = sol_index + 1
                        resv_csol(sol_index) = (res_water(res_id)%salt(isalt) * 1000.) / res_volume !g/m3 in channel        
                      enddo
                    endif
                    !constituents
                    if (gwsol_cons == 1) then
                      do ics=1,cs_db%num_cs
                        sol_index = sol_index + 1
                        resv_csol(sol_index) = (res_water(res_id)%cs(ics) * 1000.) / res_volume !g/m3 in channel        
                      enddo
                    endif
                  endif
                  !calculate mass (g)
                  do s=1,gw_nsolute
                    solmass(s) = Q * resv_csol(s)
                  enddo
                  !check against available solute mass in the channel
                  !no3
                  if((solmass(1)/1000.) > res(res_id)%no3) then
                    solmass(1) = res(res_id)%no3 * 1000. !g
                  endif
                  res(res_id)%no3 = res(res_id)%no3 - (solmass(1)/1000.) !kg
                  !p
                  if((solmass(2)/1000.) > res(res_id)%solp) then
                    solmass(2) = res(res_id)%solp * 1000. !g
                  endif
                  res(res_id)%solp = res(res_id)%solp - (solmass(2)/1000.) !kg
                  sol_index = 2
                  !salts
                  if (gwsol_salt == 1) then
                    do isalt=1,cs_db%num_salts
                      sol_index = sol_index + 1
                      if((solmass(sol_index)/1000.) > res_water(res_id)%salt(isalt)) then
                        solmass(sol_index) = res_water(res_id)%salt(isalt) * 1000. !g
                      endif 
                      res_water(res_id)%salt(isalt) = res_water(res_id)%salt(isalt) - (solmass(sol_index)/1000.) !kg
                    enddo
                  endif
                  !constituents
                  if (gwsol_cons == 1) then
                    do ics=1,cs_db%num_cs
                      sol_index = sol_index + 1
                      if((solmass(sol_index)/1000.) > res_water(res_id)%cs(ics)) then
                        solmass(sol_index) = res_water(res_id)%cs(ics) * 1000. !g
                      endif  
                      res_water(res_id)%cs(ics) = res_water(res_id)%cs(ics) - (solmass(sol_index)/1000.) !kg
                    enddo
                  endif
                  !store for mass balance calculations (in gwflow_simulate)
                  do s=1,gw_nsolute !loop through the solutes
                    gwsol_ss(cell_id)%solute(s)%resv = solmass(s)
                    gwsol_ss_sum(cell_id)%solute(s)%resv = gwsol_ss_sum(cell_id)%solute(s)%resv + solmass(s)
                  enddo
                endif

              endif !end solutes
              
            endif !check if cell is active

          enddo !go to next connected cell

        enddo !go to next reservoir cell
        
      endif !if reservoir-cell connection is active in the simulation
      
      return
      end subroutine gwflow_resv         
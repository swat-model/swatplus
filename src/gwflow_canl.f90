      subroutine gwflow_canl(chan_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use hydrograph_module, only : ch_stor
      use time_module
      use constituent_mass_module
      
      implicit none

      integer, intent (in) :: chan_id		 !       |channel number
      integer :: c											 !       |counter for canals connected to the channel
      integer :: k                       !       |counter for cells connected to a canal
      integer :: s                       !       |counter of groundwater solutes
      integer :: canal_id                !       |canal in connection with the channel
      integer :: cell_id                 !       |cell in connection with the canal
      integer :: day_beg                 !       |beginning day (julian) of active canal
      integer :: day_end                 !       |ending day (julian) of active canal
      integer :: isalt                   !       |salt ion counter
      integer :: ics                     !       |constituent counter
      integer :: sol_index 
      integer :: dum
      real :: chan_volume                !m3     |water volume in channel before groundwater exchange occurs
      real :: width                      !m      |canal width
      real :: depth                      !m      |canal depth
      real :: thick                      !m      |canal bed thickness
      real :: length                     !m      |length of canal in the cell
      real :: stage                      !m      |stage of canal in the cell
      real :: bed_K											 !m/day  |hydraulic conductivity of canal bed in the cell
      real :: flow_area                  !m2     |groundwater flow area of water exchange, in cell
      real :: canal_bed                  !m      |canal bed elevation in the cell
      real :: head_diff                  !m      |head difference between canal stage and groundwater head
      real :: Q                          !m3/day |water exchange flow rate, calculated by Darcy's Law
      real :: chan_csol(100)             !g/m3   |solute concentration in channel water
      real :: solmass(100)               !g      |solute mass transferred
      real :: conc_nh3,conc_no2,conc_dox,conc_orgn
      real :: mass_nh3,mass_no2,mass_dox,mass_orgn
      
      
      !only proceed if canal-cell exchange is active
      if (gw_canal_flag == 1) then
      
        !record starting channel volume (m3) and solute concentrations (g/m3)
        chan_csol = 0.
        conc_nh3 = 0.
        conc_no2 = 0.
        conc_dox = 0.
        conc_orgn = 0.
        chan_volume = ch_stor(chan_id)%flo
        if (chan_volume > 10.) then
          chan_csol(1) = (ch_stor(chan_id)%no3 * 1000.) / chan_volume
          chan_csol(2) = (ch_stor(chan_id)%solp * 1000.) / chan_volume
          sol_index = 2
          !salts
          if (gwsol_salt == 1) then
            do isalt=1,cs_db%num_salts
              sol_index = sol_index + 1
              chan_csol(sol_index) = (ch_water(chan_id)%salt(isalt) * 1000.) / chan_volume    
            enddo
          endif
          !constituents
          if (gwsol_cons  == 1) then
            do ics=1,cs_db%num_cs
              sol_index = sol_index + 1
              chan_csol(sol_index) = (ch_water(chan_id)%cs(ics) * 1000.) / chan_volume
            enddo
          endif 
          conc_nh3 = (ch_stor(chan_id)%nh3 * 1000.) / chan_volume
          conc_no2 = (ch_stor(chan_id)%no2 * 1000.) / chan_volume
          conc_dox = (ch_stor(chan_id)%dox * 1000.) / chan_volume
          conc_orgn = (ch_stor(chan_id)%orgn * 1000.) / chan_volume
        endif
        
        !loop through the canals that are connected to the channel
        do c=1,gw_chan_canl_info(chan_id)%ncanal
        
          !attributes of the canal
          canal_id = gw_chan_canl_info(chan_id)%canals(c)
          width = gw_chan_canl_info(chan_id)%wdth(c)
          depth = gw_chan_canl_info(chan_id)%dpth(c)
          thick = gw_chan_canl_info(chan_id)%thck(c)
          day_beg = gw_chan_canl_info(chan_id)%dayb(c)
          day_end = gw_chan_canl_info(chan_id)%daye(c)
        
          !only proceed if canal is "on"
          if (time%day.ge.day_beg .and. time%day.le.day_end) then
            
            !loop through the cells that are connected to the current canal
            do k=1,gw_canl_info(canal_id)%ncon  
              
              !id of the gwflow cell
              cell_id = gw_canl_info(canal_id)%cells(k)
              
              !only proceed if the cell is active
              if (gw_state(cell_id)%stat == 1) then
              
                !attributes of the cell
                length = gw_canl_info(canal_id)%leng(k) 
                stage = gw_canl_info(canal_id)%elev(k) 
                bed_K = gw_canl_info(canal_id)%hydc(k)
                
                !calculate exchange rate Q (m3/day)
                flow_area = length * width !m2 = area of seepage
                canal_bed = stage - depth !m
                Q = 0.
                if(gw_state(cell_id)%head < canal_bed) then
                  head_diff = depth
                  Q =  bed_K * (head_diff / thick) * flow_area !canal seepage (positive Q: entering aquifer)
                elseif (gw_state(cell_id)%head > stage) then
                  head_diff = gw_state(cell_id)%head - stage
                  Q = bed_K * (head_diff / thick) * flow_area * (-1) !gw discharge (negative Q: leaving aquifer)
                elseif (gw_state(cell_id)%head > canal_bed .and. gw_state(cell_id)%head < stage) then
                  head_diff = stage - gw_state(cell_id)%head
                  Q = bed_K * (head_diff / thick) * flow_area !canal seepage (positive Q: entering aquifer)
                endif
                
                !store values in gwflow source/sink arrays
                if(Q < 0) then !groundwater --> canal
                  !if ((Q*-1 == 1).ge.gw_state(cell_id)%stor) then !can only remove what is there
                  if (-Q .ge. gw_state(cell_id)%stor) then !can only remove what is there                  
                    Q = -gw_state(cell_id)%stor 
                  endif
                  gw_ss(cell_id)%canl = gw_ss(cell_id)%canl + Q
                  gw_state(cell_id)%stor = gw_state(cell_id)%stor + Q !update available groundwater in the cell 
                else !canal --> groundwater (seepage)
                  if(Q > 0) then !canal seepage; remove water from channel
                    if(Q > ch_stor(chan_id)%flo) then !can only remove what is there
                      Q = ch_stor(chan_id)%flo
                    endif
                    gw_ss(cell_id)%canl = gw_ss(cell_id)%canl + Q !store for water balance calculations
                    ch_stor(chan_id)%flo = ch_stor(chan_id)%flo - Q !remove water from channel 
                  endif
                endif
                gw_ss_sum(cell_id)%canl = gw_ss_sum(cell_id)%canl + Q !store for annual water 
                
                !calculate solute mass (g/day) transported between cell and channel
                if (gw_solute_flag == 1) then
                  if(Q < 0) then !mass is leaving the cell --> canal
                    do s=1,gw_nsolute !loop through the solutes
                      solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                      if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                        solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                      endif
                    enddo
                  else !mass entering the cell from the canal (i.e., from the channel that provides the canal water)
                    !calculate mass (g)
                    do s=1,gw_nsolute
                      solmass(s) = Q * chan_csol(s)
                    enddo
                    mass_nh3 = Q * conc_nh3
                    mass_no2 = Q * conc_no2
                    mass_dox = Q * conc_dox
                    mass_orgn = Q * conc_orgn
                    !check against available solute mass in the channel
                    !no3
                    if((solmass(1)/1000.) > ch_stor(chan_id)%no3) then
                      solmass(1) = ch_stor(chan_id)%no3 * 1000. !g
                    endif
                    ch_stor(chan_id)%no3 = ch_stor(chan_id)%no3 - (solmass(1)/1000.) !kg
                    !p
                    if((solmass(2)/1000.) > ch_stor(chan_id)%solp) then
                      solmass(2) = ch_stor(chan_id)%solp * 1000. !g
                    endif
                    ch_stor(chan_id)%solp = ch_stor(chan_id)%solp - (solmass(2)/1000.) !kg
                    !nh3
                    if((mass_nh3/1000.) > ch_stor(chan_id)%nh3) then
                      mass_nh3 = ch_stor(chan_id)%nh3 * 1000. !g
                    endif
                    ch_stor(chan_id)%nh3 = ch_stor(chan_id)%nh3 - (mass_nh3/1000.) !kg
                    !no2
                    if((mass_no2/1000.) > ch_stor(chan_id)%no2) then
                      mass_no2 = ch_stor(chan_id)%no2 * 1000. !g
                    endif
                    ch_stor(chan_id)%no2 = ch_stor(chan_id)%no2 - (mass_no2/1000.) !kg
                    !dox
                    if((mass_dox/1000.) > ch_stor(chan_id)%dox) then
                      mass_dox = ch_stor(chan_id)%dox * 1000. !g
                    endif
                    ch_stor(chan_id)%dox = ch_stor(chan_id)%dox - (mass_dox/1000.) !kg
                    !orgn
                    if((mass_orgn/1000.) > ch_stor(chan_id)%orgn) then
                      mass_orgn = ch_stor(chan_id)%orgn * 1000. !g
                    endif
                    ch_stor(chan_id)%orgn = ch_stor(chan_id)%orgn - (mass_orgn/1000.) !kg
                    sol_index = 2
                    !salts
                    if (gwsol_salt == 1) then
                      do isalt=1,cs_db%num_salts
                        sol_index = sol_index + 1
                        if((solmass(sol_index)/1000.) > ch_water(chan_id)%salt(isalt)) then
                          solmass(sol_index) = ch_water(chan_id)%salt(isalt) * 1000. !g
                        endif 
                        ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) - (solmass(sol_index)/1000.) !kg
                      enddo
                    endif
                    !constituents
                    if (gwsol_cons == 1) then
                      do ics=1,cs_db%num_cs
                        sol_index = sol_index + 1
                        if((solmass(sol_index)/1000.) > ch_water(chan_id)%cs(ics)) then
                          solmass(sol_index) = ch_water(chan_id)%cs(ics) * 1000. !g
                        endif  
                        ch_water(chan_id)%cs(ics) = ch_water(chan_id)%cs(ics) - (solmass(sol_index)/1000.) !kg
                      enddo
                    endif
                  endif
                  !store in mass balance arrays
                  do s=1,gw_nsolute !loop through the solutes
                    gwsol_ss(cell_id)%solute(s)%canl = gwsol_ss(cell_id)%solute(s)%canl + solmass(s)
                    gwsol_ss_sum(cell_id)%solute(s)%canl = gwsol_ss_sum(cell_id)%solute(s)%canl + solmass(s)
                  enddo
                endif !end solutes
                
              endif !check if cell is active
              
            enddo !go to next cell connected to the canal

          endif !check if canal is "on"
      
        enddo !go to next canal connected to the channel  
          
      endif !check if canal-cell exchange is active
          
      return
      end subroutine gwflow_canl      
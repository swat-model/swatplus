      subroutine gwflow_chem(cell_id,gw_vol) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates chemical reactions in gwflow cells
      
      use gwflow_module
      use hydrograph_module
      use constituent_mass_module, only : cs_db
      
      implicit none

      integer, intent (in) :: cell_id       !       |id of cell passed in
      real, intent (in) :: gw_vol           !m3     |volume of groundwater in the cell
      integer :: s = 0                      !       |solute counter
      integer :: n = 0                      !       |shale counter
      integer :: k = 0                      !       |counter for connected cells
      integer :: shale_source = 0           !       |flag: cell contains shale
      integer :: cell_conn                  !       |connected cell
      integer :: isalt = 0                  !            |salt ion counter
      integer :: sol_index = 0              !       |index to keep track of number of solutes
      integer :: dum = 0
      real :: rctn_rate = 0.
      real :: cseo4 = 0.
      real :: cseo3 = 0.
      real :: cno3 = 0.
      real :: o2 = 0.
      real :: no3inhib = 0.
      real :: seo4red = 0.
      real :: seo3red = 0.
      real :: o2red = 0.
      real :: no3red = 0.
      real :: yseo4_o2 = 0.
      real :: yseo4_no3 = 0.
      real :: se_prod_o2 = 0.
      real :: se_prod_no3 = 0.
      real :: ko2a = 0.
      real :: kno3a = 0.
      real :: sseratio = 0.

      
      !track solute numbers
      sol_index = 1        
      mass_rct = 0.
      
      !no3
	  rctn_rate = gwsol_rctn(sol_index)
      mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * rctn_rate	!g/day
      
      !p
      sol_index = sol_index + 1
      mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * gwsol_rctn(sol_index)	!g/day
      
      !salt ions
      if(gwsol_salt) then
        !first-order reaction
        do isalt=1,cs_db%num_salts
          sol_index = sol_index + 1
          mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * gwsol_rctn(sol_index)	!g/day
        enddo
        !precipitation-dissolution (if activated, in gwflow.solutes.minerals)
        if(gwsol_minl) then
          call gwflow_minl(cell_id,gw_vol)
        endif
      endif
      
      !constituents
      if(gwsol_cons) then
      
        !retrieve current concentrations (g/m3) in the cell
        cno3 = gwsol_state(cell_id)%solute(1)%conc
        sol_index = sol_index + 1
        cseo4 = gwsol_state(cell_id)%solute(sol_index)%conc
        sol_index = sol_index + 1
        cseo3 = gwsol_state(cell_id)%solute(sol_index)%conc
        o2 = gwsol_chem(cell_id)%oxyg
        
        !rate law for selenate and selenite reduction
        no3inhib = gwsol_chem(cell_id)%ino3 / (gwsol_chem(cell_id)%ino3 + cno3)
				!seo4
				rctn_rate = gwsol_chem(cell_id)%kseo4
				!increase reaction rate if riparian bmp is active, and if cell is in riparian area
				seo4red = rctn_rate * cseo4 * no3inhib !g/m3-d
				!seo3
				rctn_rate = gwsol_chem(cell_id)%kseo3
				!increase reaction rate if riparian bmp is active, and if cell is in riparian area
        seo3red = rctn_rate * cseo3 * no3inhib !g/m3-d
        
				!rate law for oxygen reduction and nitrate reduction (mobilization of selenium)
				yseo4_o2 = 315.84 / 224.0
        yseo4_no3 = 789.6 / 196.0
        no3red = 0.
        se_prod_o2 = 0.
        se_prod_no3 = 0.
				shale_source = 0
				!1. cell contains shale
        do n=1,gwsol_chem(cell_id)%nshale
          !if cell contains shale
          if(gwsol_chem(cell_id)%shale(n) == 1) then
            !reduction of o2
            ko2a = gwsol_chem(cell_id)%shale_o2a(n)
            o2red = ko2a * o2 !g/m3-d
            !reduction of no3
            kno3a = gwsol_chem(cell_id)%shale_no3a(n)
            no3red = no3red + (kno3a * cno3) !g/m3-d
            !total selenium released from the shale (via oxidation)
            sseratio = gwsol_chem(cell_id)%shale_sseratio(n)
            se_prod_o2 = se_prod_o2 + (o2red * yseo4_o2 * (1/sseratio))
            se_prod_no3 = se_prod_no3 + (no3red * yseo4_no3 * (1/sseratio))  
						!if cell has shale, then do not permit shale source from bedrock or adjacent cell
						gwsol_chem(cell_id)%bed_flag = 0
						shale_source = 1
          endif
        enddo
        !2. cell underlain by bedrock shale
				if(gwsol_chem(cell_id)%bed_flag == 1) then
          !reduction of o2
          ko2a = gwsol_chem(cell_id)%bed_o2a
          o2red = ko2a * o2 !g/m3-d
          !reduction of no3
          kno3a = gwsol_chem(cell_id)%bed_no3a
          no3red = no3red + (kno3a * cno3) !g/m3-d
          !total selenium released from the shale (via oxidation)
          sseratio = gwsol_chem(cell_id)%bed_sse
          se_prod_o2 = se_prod_o2 + (o2red * yseo4_o2 * (1/sseratio))
          se_prod_no3 = se_prod_no3 + (no3red * yseo4_no3 * (1/sseratio))  
        endif
				!3. cell is adjacent to a cell that contains shale
				!only proceed if cell does not contain shale
				!loop through connecting cells
				if(shale_source == 0) then
				  do k=1,gw_state(cell_id)%ncon
            cell_conn = cell_con(cell_id)%cell_id(k)
					  cno3 = gwsol_state(cell_conn)%solute(1)%conc
				    do n=1,gwsol_chem(cell_conn)%nshale
              !if cell contains shale
              if(gwsol_chem(cell_conn)%shale(n) == 1) then
						    !reduction of o2
                ko2a = gwsol_chem(cell_conn)%shale_o2a(n)
                o2red = ko2a * o2 !g/m3-d
                !reduction of no3
                kno3a = gwsol_chem(cell_conn)%shale_no3a(n)
                no3red = no3red + (kno3a * cno3) !g/m3-d
                !total selenium released from the shale (via oxidation)
                sseratio = gwsol_chem(cell_conn)%shale_sseratio(n)
                se_prod_o2 = se_prod_o2 + (o2red * yseo4_o2 * (1/sseratio))
                se_prod_no3 = se_prod_no3 + (no3red * yseo4_no3 * (1/sseratio))
						  endif
					  enddo
				  enddo
				endif
        
        !seo4 mass reacted
        sol_index = sol_index - 1
        mass_rct(sol_index) = (se_prod_o2 + se_prod_no3 - seo4red) * gw_vol !g/day
        if(mass_rct(sol_index) > 0) then
          dum = 10
        endif
        
        !seo3 mass reacted
        sol_index = sol_index + 1
        mass_rct(sol_index) = (seo4red - seo3red) * gw_vol !g/day
        
        !no3 mass reacted (updated)
        mass_rct(1) = mass_rct(1) - (no3red * gw_vol) !g/day
        
        !boron
        sol_index = sol_index + 1
        mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * gwsol_rctn(sol_index)	!g/day
        
      endif !if constituents are active
      
      
      end subroutine gwflow_chem
      
      subroutine gwflow_chem(cell_id,gw_vol) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates chemical reactions in gwflow cells
      
      use gwflow_module
      use hydrograph_module
      use constituent_mass_module, only : cs_db
      
      implicit none

      integer, intent (in) :: cell_id		    !       |id of cell passed in
      real, intent (in) :: gw_vol		        !m3     |volume of groundwater in the cell
      integer :: s                          !       |solute counter
      integer :: n                          !       |shale counter
      integer :: isalt                      !				|salt ion counter
      integer :: sol_index                  !       |index to keep track of number of solutes
      integer :: dum
      real :: cseo4,cseo3,cno3,o2, &
              no3inhib,seo4red,seo3red,o2red,no3red, &
              yseo4_o2,yseo4_no3,se_prod_o2,se_prod_no3,ko2a,kno3a,sseratio

      
      !track solute numbers
      sol_index = 1        
      mass_rct = 0.
      
      !no3
      mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * gwsol_rctn(sol_index)	!g/day
      
      !p
      sol_index = sol_index + 1
      mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * gwsol_rctn(sol_index)	!g/day
      
      !salt ions
      if (gwsol_salt == 1) then
        do isalt=1,cs_db%num_salts
          sol_index = sol_index + 1
          mass_rct(sol_index) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol * gwsol_rctn(sol_index)	!g/day
        enddo
      endif
      
      !constituents
      if (gwsol_cons == 1) then
      
        !retrieve current concentrations (g/m3) in the cell
        cno3 = gwsol_state(cell_id)%solute(1)%conc
        sol_index = sol_index + 1
        cseo4 = gwsol_state(cell_id)%solute(sol_index)%conc
        sol_index = sol_index + 1
        cseo3 = gwsol_state(cell_id)%solute(sol_index)%conc
        o2 = gwsol_chem(cell_id)%oxyg
        
        !rate law for selenate and selenite reduction
        no3inhib = gwsol_chem(cell_id)%ino3 / (gwsol_chem(cell_id)%ino3 + cno3)
        seo4red = gwsol_chem(cell_id)%kseo4 * cseo4 * no3inhib !g/m3-d
        seo3red = gwsol_chem(cell_id)%kseo3 * cseo3 * no3inhib !g/m3-d
        
        !rate law for oxygen reduction and nitrate reduction, in the presence of shale
        yseo4_o2 = 315.84 / 224.0
        yseo4_no3 = 789.6 / 196.0
        no3red = 0.
        se_prod_o2 = 0.
        se_prod_no3 = 0.
        do n=1,gwsol_chem(cell_id)%nshale
          !if cell contains shale
          if (gwsol_chem(cell_id)%shale(n) == 1) then
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
          endif
        enddo
        
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
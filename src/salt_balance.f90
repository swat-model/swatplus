      subroutine salt_balance
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates total salt in system and writes out data to perform
!!    a system-wide salt mass balance.

      use hydrograph_module
      use organic_mineral_mass_module
      use output_landscape_module
      use aquifer_module
      use hru_module, only : hru,ihru
      use soil_module
      use time_module
      use salt_module
      use salt_aquifer
      use constituent_mass_module
      use res_salt_module, only : wetsalt_d,ressalt_d
      use ch_salt_module, only : chsalt_d
      use gwflow_module, only : gw_solute_flag,gwsol_ss,ncell,gw_state,gwsol_state

      implicit none
      
      integer :: i,m,ob_ctr,num_days,jj
      real :: saltsum,hru_area_m2,sol_thick,soil_volume,soil_mass, &
              aquifer_thickness,aquifer_volume,aquifer_mass, sub_ha, soil_thick
      real :: sum_conc,avg_conc(cs_db%num_salts),sum_load,avg_load(11)
      real :: salt_basin(28)

      
      !basin-wide salt mass balance -------------------------------------------------------------------------------------------------------
      
      !lateral salt loading to channels
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%latq * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(1) = saltsum

      !groundwater salt loading to channels
      saltsum = 0.
      if(bsn_cc%gwflow == 1) then !gwflow is active; loop through cells
        if (gw_solute_flag == 1) then
          do i=1,ncell
            do m=1,cs_db%num_salts
              saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%gwsw * (-1) / 1000.) !kg  
              saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%swgw * (-1) / 1000.) !kg 
              saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%satx * (-1) / 1000.) !kg 
            enddo
          enddo
        endif    
      else !normal aquifer module
        ob_ctr = sp_ob1%aqu !first aquifer object
        do i=1,sp_ob%aqu
          do m=1,cs_db%num_salts
            saltsum = saltsum + asaltb_d(i)%salt(m)%saltgw !kg
          enddo
          ob_ctr = ob_ctr + 1
        enddo
      endif
      salt_basin(2) = saltsum

      !surface runoff salt loading to channels
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%surq * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(3) = saltsum

      !urban runoff loading to channels
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%urbq * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(4) = saltsum
      
      !wetland runoff loading to channels
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%wetq * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(5) = saltsum
      
      !tile drain salt loading to stream
      saltsum = 0.
      if(bsn_cc%gwflow == 1) then !gwflow is active (add to tile drainage from HRU soils)
        if (gw_solute_flag == 1) then
          do i=1,ncell
            do m=1,cs_db%num_salts
              saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%tile * (-1) / 1000.) !kg  
            enddo
          enddo
        endif    
      endif
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%tile * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(6) = saltsum

      !soil leaching salt loading to groundwater
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%perc * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(7) = saltsum
      
      !groundwater transfer salt loading to soil
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%gwup * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(8) = saltsum

      !wetland seepage to soil profile
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%wtsp * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(9) = saltsum
      
      !irrigation from surface water
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%irsw * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(10) = saltsum

      !irrigation from groundwater
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%irgw * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(11) = saltsum

      !irrigation from outside source
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%irwo * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(12) = saltsum

      !wet deposition (rainfall)
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%rain * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(13) = saltsum
      
      !dry deposition
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%dryd * hru(i)%area_ha) !kg
        enddo
      enddo
      salt_basin(14) = saltsum
      
      !applied road salt
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%road * hru(i)%area_ha) !kg
        enddo
      enddo
      if(saltsum < 1.e-6) saltsum = 0.
      salt_basin(15) = saltsum
      
      !applied fertilizer
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%fert * hru(i)%area_ha) !kg
        enddo
      enddo
      if(saltsum < 1.e-6) saltsum = 0.
      salt_basin(16) = saltsum
      
      !applied soil amendments
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%amnd * hru(i)%area_ha) !kg
        enddo
      enddo
      if(saltsum < 1.e-6) saltsum = 0.
      salt_basin(17) = saltsum
      
      !salt uptake by plants
      saltsum = 0.
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          saltsum = saltsum + (hsaltb_d(i)%salt(m)%uptk * hru(i)%area_ha) !kg
        enddo
      enddo
      if(saltsum < 1.e-6) saltsum = 0.
      salt_basin(18) = saltsum
      
      !point sources (from within watershed, e.g., treatment plant effluent)
      saltsum = 0.
      do i=1,sp_ob%recall
        do m=1,cs_db%num_salts
          saltsum = saltsum + recsaltb_d(i)%salt(m) !kg  
        enddo
      enddo
      salt_basin(19) = saltsum
      
      !point sources (from without watershed, e.g., inflows from upstream watersheds)
      saltsum = 0.
      do i=1,sp_ob%recall
        do m=1,cs_db%num_salts
          saltsum = saltsum + recoutsaltb_d(i)%salt(m) !kg  
        enddo
      enddo
      salt_basin(20) = saltsum
      
      !recharge to aquifer
      saltsum = 0.
      if(bsn_cc%gwflow == 1) then !gwflow is active (add to tile drainage from HRU soils)
        if (gw_solute_flag == 1) then
          do i=1,ncell
            do m=1,cs_db%num_salts
              saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%rech / 1000.) !kg  
            enddo
          enddo
        endif    
      else !normal aquifer module
        ob_ctr = sp_ob1%aqu !first aquifer object
        do i=1,sp_ob%aqu
          do m=1,cs_db%num_salts
            saltsum = saltsum + asaltb_d(i)%salt(m)%rchrg !kg
          enddo
          ob_ctr = ob_ctr + 1
        enddo
      endif
      salt_basin(21) = saltsum
      
      !seepage from aquifer
      saltsum = 0.
      ob_ctr = sp_ob1%aqu !first aquifer object
      do i=1,sp_ob%aqu
        do m=1,cs_db%num_salts
          saltsum = saltsum + asaltb_d(i)%salt(m)%seep !kg
        enddo
        ob_ctr = ob_ctr + 1
      enddo
      salt_basin(22) = saltsum

      !solid --> dissolved (soil)
      saltsum = 0.
      do i=1,sp_ob%hru
        saltsum = saltsum + (hsaltb_d(i)%salt(1)%diss * hru(i)%area_ha) !kg
      enddo
      salt_basin(23) = saltsum

      !solid --> dissolved (aquifer)
      saltsum = 0.
      if(bsn_cc%gwflow == 1) then !gwflow is active
        if(gw_solute_flag == 1) then
          do i=1,ncell
            do m=1,cs_db%num_salts  
              saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%minl / 1000.) !kg 
            enddo
          enddo
        endif 
      else
        ob_ctr = sp_ob1%aqu !first aquifer object
        do i=1,sp_ob%aqu
          saltsum = saltsum + asaltb_d(i)%salt(1)%diss !kg
          ob_ctr = ob_ctr + 1
        enddo
      endif
      salt_basin(24) = saltsum

      !total soil salt (dissolved)
      saltsum = 0.
      do i=1,sp_ob%hru
        do jj=1,soil(i)%nly
          do m=1,cs_db%num_salts
            saltsum = saltsum + (cs_soil(i)%ly(jj)%salt(m) * hru(i)%area_ha) !kg
          enddo
        enddo
      enddo
      salt_basin(25) = saltsum

      !total soil salt (solid)
      saltsum = 0.
      do i=1,sp_ob%hru
        hru_area_m2 = hru(i)%area_ha * 10000. !m2  
        do jj=1,soil(i)%nly
          soil_thick = soil(i)%phys(jj)%thick !soil thickness (mm) 
          soil_volume = hru_area_m2 * (soil_thick/1000.) !m3 of soil
          soil_mass = soil_volume * (soil(i)%phys(jj)%bd*1000.) !kg of soil
          do m=1,5
            saltsum = saltsum + soil_mass*(cs_soil(i)%ly(jj)%salt_min(m)/100.) !kg
          enddo
        enddo
      enddo
      salt_basin(26) = saltsum

      !total groundwater salt (dissolved)
      saltsum = 0.
      if(bsn_cc%gwflow == 1) then !gwflow is active
        if (gw_solute_flag == 1) then
          do i=1,ncell
            if(gw_state(i)%stat > 0) then
              do m=1,cs_db%num_salts
                saltsum = saltsum + (gwsol_state(i)%solute(2+m)%mass / 1000.) !kg
              enddo
            endif
          enddo
        endif    
      else !normal aquifer module
        ob_ctr = sp_ob1%aqu !first aquifer object
        do i=1,sp_ob%aqu
          do m=1,cs_db%num_salts
            saltsum = saltsum + cs_aqu(i)%salt(m) !kg
          enddo
          ob_ctr = ob_ctr + 1
        enddo
      endif
      salt_basin(27) = saltsum
      
      !total groundwater salt (solid)
      saltsum = 0.
      ob_ctr = sp_ob1%aqu !first aquifer object
      do i=1,sp_ob%aqu
        hru_area_m2 = ob(ob_ctr)%area_ha * 10000. !m2
        aquifer_thickness = 15.
        aquifer_volume = hru_area_m2 * aquifer_thickness
        aquifer_mass = aquifer_volume * 2.000 * 1000. !kg
        do m=1,5
          saltsum = saltsum + aquifer_mass * (cs_aqu(i)%salt_min(m)/100.) !kg
        enddo
        ob_ctr = ob_ctr + 1
      enddo
      salt_basin(28) = saltsum
      
      !write out basin-wide salinity fluxes to file
      write(5080,7000) time%yrc,time%mo,time%day,(salt_basin(i),i=1,28)
      
      !save fluxes for monthly, yearly, and ave annual output
      do i=1,28
        salt_basin_mo(i) = salt_basin_mo(i) + salt_basin(i)
        salt_basin_yr(i) = salt_basin_yr(i) + salt_basin(i)
        salt_basin_aa(i) = salt_basin_aa(i) + salt_basin(i)
      enddo
      
      !monthly print
      if (time%end_mo == 1) then
        num_days = float(time%day_mo)
        salt_basin_mo(25) = salt_basin_mo(25) / num_days
        salt_basin_mo(26) = salt_basin_mo(26) / num_days
        salt_basin_mo(27) = salt_basin_mo(27) / num_days
        salt_basin_mo(28) = salt_basin_mo(28) / num_days
        write(5082,7000) time%yrc,time%mo,time%day,(salt_basin_mo(i),i=1,28)
        salt_basin_mo = 0.
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        num_days = float(time%day_end_yr)
        salt_basin_yr(25) = salt_basin_yr(25) / num_days
        salt_basin_yr(26) = salt_basin_yr(26) / num_days
        salt_basin_yr(27) = salt_basin_yr(27) / num_days
        salt_basin_yr(28) = salt_basin_yr(28) / num_days
        write(5084,7000) time%yrc,time%mo,time%day,(salt_basin_yr(i),i=1,28)
        salt_basin_yr = 0.
      endif
      
      !average annual print
      if (time%end_sim == 1) then
        do i=1,23
          salt_basin_aa(i) = salt_basin_aa(i) / time%nbyr
        enddo
        num_days = time%days_prt
        salt_basin_aa(25) = salt_basin_aa(25) / num_days
        salt_basin_aa(26) = salt_basin_aa(26) / num_days
        salt_basin_aa(27) = salt_basin_aa(27) / num_days
        salt_basin_aa(28) = salt_basin_aa(28) / num_days      
        write(5086,7000) time%yrc,time%mo,time%day,(salt_basin_aa(i),i=1,28)
      endif
      
      
      !zero out balance arrays for next day
      !hru
      do i=1,sp_ob%hru
        do m=1,cs_db%num_salts
          !HRUs
          hsaltb_d(i)%salt(m)%soil = 0.
          hsaltb_d(i)%salt(m)%surq = 0.
          hsaltb_d(i)%salt(m)%latq = 0.
          hsaltb_d(i)%salt(m)%urbq = 0.
          hsaltb_d(i)%salt(m)%wetq = 0.
          hsaltb_d(i)%salt(m)%tile = 0.
          hsaltb_d(i)%salt(m)%perc = 0.
          hsaltb_d(i)%salt(m)%gwup = 0.
          hsaltb_d(i)%salt(m)%wtsp = 0.
          hsaltb_d(i)%salt(m)%irsw = 0.
          hsaltb_d(i)%salt(m)%irgw = 0.
          hsaltb_d(i)%salt(m)%irwo = 0.
          hsaltb_d(i)%salt(m)%rain = 0.
          hsaltb_d(i)%salt(m)%dryd = 0.
          hsaltb_d(i)%salt(m)%road = 0.
          hsaltb_d(i)%salt(m)%fert = 0.
          hsaltb_d(i)%salt(m)%amnd = 0.
          hsaltb_d(i)%salt(m)%uptk = 0.
          !wetlands
          wetsalt_d(i)%salt(m)%inflow = 0.
          wetsalt_d(i)%salt(m)%outflow = 0.
          wetsalt_d(i)%salt(m)%seep = 0.
          wetsalt_d(i)%salt(m)%fert = 0.
          wetsalt_d(i)%salt(m)%irrig = 0.
          wetsalt_d(i)%salt(m)%div = 0.
        enddo
        hsaltb_d(i)%salt(1)%diss = 0.
      enddo
      !aquifer
      do i=1,sp_ob%aqu
        do m=1,cs_db%num_salts
          asaltb_d(i)%salt(m)%saltgw = 0.
          asaltb_d(i)%salt(m)%rchrg = 0.
          asaltb_d(i)%salt(m)%seep = 0.
          asaltb_d(i)%salt(m)%irr = 0.
          asaltb_d(i)%salt(m)%div = 0.
        enddo
        asaltb_d(i)%salt(1)%diss = 0.
      enddo
      !point source
      do i=1,sp_ob%recall
        do m=1,cs_db%num_salts
          recsaltb_d(i)%salt(m) = 0.
          recoutsaltb_d(i)%salt(m) = 0.
        enddo
      enddo
      !reservoirs
      do i=1,sp_ob%res
        do m=1,cs_db%num_salts
          ressalt_d(i)%salt(m)%inflow = 0.
          ressalt_d(i)%salt(m)%outflow = 0.
          ressalt_d(i)%salt(m)%seep = 0.
          ressalt_d(i)%salt(m)%fert = 0.
          ressalt_d(i)%salt(m)%irrig = 0.
          ressalt_d(i)%salt(m)%div = 0.
        enddo
      enddo
      !channels
      do i=1,sp_ob%chandeg
        do m=1,cs_db%num_salts
          chsalt_d(i)%salt(m)%irr = 0.
          chsalt_d(i)%salt(m)%div = 0.
          chsalt_d(i)%salt(m)%gw_in = 0.
        enddo
      enddo
      
      !if gwflow active: zero out daily cell values for recharge and chemical reactions (others are zeroed out in gwflow_simulate)
      if(bsn_cc%gwflow == 1) then
        if (gw_solute_flag == 1) then
          do i=1,ncell
            do m=1,cs_db%num_salts
              gwsol_ss(i)%solute(2+m)%rech = 0.
              gwsol_ss(i)%solute(2+m)%rcti = 0.
              gwsol_ss(i)%solute(2+m)%rcto = 0.
              gwsol_ss(i)%solute(2+m)%minl = 0.
					  enddo
          enddo
        endif
      endif
      
      
7000  format(i8,i8,i8,35e16.8)
7001  format(20e16.8)

      return
      end
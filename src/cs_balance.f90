      subroutine cs_balance !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates total constituent mass in system and writes out data to perform
!!    a system-wide constituent mass balance

      use hydrograph_module
      use organic_mineral_mass_module
      use output_landscape_module
      use aquifer_module
      use hru_module, only : hru,ihru
      use soil_module
      use time_module
      use constituent_mass_module
      use cs_module
      use cs_aquifer
      use res_cs_module, only : wetcs_d,rescs_d
      use ch_cs_module, only: chcs_d
      use gwflow_module, only : gw_solute_flag,gwsol_ss,ncell,ncell,gw_state,gwsol_state

      implicit none
      
      integer :: i,m,ob_ctr,num_days,sol_index,jj
      real :: cssum1,cssum2,cssum3,hru_area_m2,sol_thick,soil_volume,soil_mass, &
              aquifer_thickness,aquifer_volume,aquifer_mass, sub_ha
      real :: cs_basin(87)

      !basin-wide constituent mass balance ------------------------------------------------------------------------------------------------

      !soil profile ---------------------------------------------------------------------------------------------------

      !lateral constituent loading to streams
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%latq * hru(i)%area_ha) !seo4 kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%latq * hru(i)%area_ha) !seo3 kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%latq * hru(i)%area_ha) !boron kg
      enddo
      cs_basin(1) = cssum1
      cs_basin(30) = cssum2
      cs_basin(59) = cssum3

      !surface runoff constituent loading to stream
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%surq * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%surq * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%surq * hru(i)%area_ha) !kg
      enddo
      cs_basin(2) = cssum1
      cs_basin(31) = cssum2
      cs_basin(60) = cssum3

      !selenium mass attached to sediment in surface runoff
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%sedm * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%sedm * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%sedm * hru(i)%area_ha) !kg
      enddo
      cs_basin(3) = cssum1
      cs_basin(32) = cssum2
      cs_basin(61) = cssum3
      
      !selenium mass attached to sediment in urban runoff
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%urbq * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%urbq * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%urbq * hru(i)%area_ha) !kg
      enddo
      cs_basin(4) = cssum1
      cs_basin(33) = cssum2
      cs_basin(62) = cssum3
      
      !selenium mass in wetland runoff to stream
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%wetq * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%wetq * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%wetq * hru(i)%area_ha) !kg
      enddo
      cs_basin(5) = cssum1
      cs_basin(34) = cssum2
      cs_basin(63) = cssum3
      
      !tile drain constituent loading to stream
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      if (bsn_cc%gwflow == 1) then !gwflow is active; loop through cells (add to tile flow from HRUs)
        if (gw_solute_flag == 1) then
          do i=1,ncell
            sol_index = 2 + cs_db%num_salts
            cssum1 = cssum1 + (gwsol_ss(i)%solute(sol_index+1)%tile * (-1) / 1000.) !kg seo4  
            cssum2 = cssum2 + (gwsol_ss(i)%solute(sol_index+2)%tile * (-1) / 1000.) !kg seo3
            cssum3 = cssum3 + (gwsol_ss(i)%solute(sol_index+3)%tile * (-1) / 1000.) !kg boron  
          enddo
        endif 
      endif
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%tile * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%tile * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%tile * hru(i)%area_ha) !kg
      enddo
      cs_basin(6) = cssum1
      cs_basin(35) = cssum2
      cs_basin(64) = cssum3

      !soil leaching constituent loading to groundwater
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%perc * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%perc * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%perc * hru(i)%area_ha) !kg
      enddo
      cs_basin(7) = cssum1
      cs_basin(36) = cssum2
      cs_basin(65) = cssum3

      !groundwater transfer to soil
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%gwup * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%gwup * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%gwup * hru(i)%area_ha) !kg
      enddo
      cs_basin(8) = cssum1
      cs_basin(37) = cssum2
      cs_basin(66) = cssum3
      
      !wetland seepage to soil profile
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%wtsp * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%wtsp * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%wtsp * hru(i)%area_ha) !kg
      enddo
      cs_basin(9) = cssum1
      cs_basin(38) = cssum2
      cs_basin(67) = cssum3
      
      !irrigation from surface water
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%irsw * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%irsw * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%irsw * hru(i)%area_ha) !kg
      enddo
      cs_basin(10) = cssum1
      cs_basin(39) = cssum2
      cs_basin(68) = cssum3
      
      !irrigation from groundwater
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%irgw * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%irgw * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%irgw * hru(i)%area_ha) !kg
      enddo
      cs_basin(11) = cssum1
      cs_basin(40) = cssum2
      cs_basin(69) = cssum3

      !irrigation from outside source
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%irwo * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%irwo * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%irwo * hru(i)%area_ha) !kg
      enddo
      cs_basin(12) = cssum1
      cs_basin(41) = cssum2
      cs_basin(70) = cssum3

      !wet deposition (rainfall)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%rain * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%rain * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%rain * hru(i)%area_ha) !kg
      enddo
      cs_basin(13) = cssum1
      cs_basin(42) = cssum2
      cs_basin(71) = cssum3
      
      !dry deposition
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%dryd * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%dryd * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%dryd * hru(i)%area_ha) !kg
      enddo
      cs_basin(14) = cssum1
      cs_basin(43) = cssum2
      cs_basin(72) = cssum3
      
      !fertilizer
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%fert * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%fert * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%fert * hru(i)%area_ha) !kg
      enddo
      cs_basin(15) = cssum1
      cs_basin(44) = cssum2
      cs_basin(73) = cssum3
      
      !constituent uptake by plants
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%uptk * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%uptk * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%uptk * hru(i)%area_ha) !kg
      enddo
      cs_basin(16) = cssum1
      cs_basin(45) = cssum2
      cs_basin(74) = cssum3
      
      !chemical reactions - soil
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%rctn * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%rctn * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%rctn * hru(i)%area_ha) !kg
      enddo
      cs_basin(17) = cssum1
      cs_basin(46) = cssum2
      cs_basin(75) = cssum3
      
      !sorption reaction - soil
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        cssum1 = cssum1 + (hcsb_d(i)%cs(1)%sorb * hru(i)%area_ha) !kg
        cssum2 = cssum2 + (hcsb_d(i)%cs(2)%sorb * hru(i)%area_ha) !kg
        cssum3 = cssum3 + (hcsb_d(i)%cs(3)%sorb * hru(i)%area_ha) !kg
      enddo
      cs_basin(18) = cssum1
      cs_basin(47) = cssum2
      cs_basin(76) = cssum3
      
      !point sources (from within watershed, e.g., treatment plant effluent)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%recall
        cssum1 = cssum1 + reccsb_d(i)%cs(1) !kg  
        cssum2 = cssum2 + reccsb_d(i)%cs(2) !kg  
        cssum3 = cssum3 + reccsb_d(i)%cs(3) !kg  
      enddo
      cs_basin(19) = cssum1
      cs_basin(48) = cssum2
      cs_basin(77) = cssum3
      
      !point sources (from without watershed, e.g., inflows from upstream watersheds)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%recall
        cssum1 = cssum1 + recoutcsb_d(i)%cs(1) !kg  
        cssum2 = cssum2 + recoutcsb_d(i)%cs(2) !kg
        cssum3 = cssum3 + recoutcsb_d(i)%cs(3) !kg
      enddo
      cs_basin(20) = cssum1
      cs_basin(49) = cssum2
      cs_basin(78) = cssum3
      
      !total soil constituent mass (dissolved)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        do jj=1,soil(i)%nly
          cssum1 = cssum1 + (cs_soil(i)%ly(jj)%cs(1) * hru(i)%area_ha) !kg
          cssum2 = cssum2 + (cs_soil(i)%ly(jj)%cs(2) * hru(i)%area_ha) !kg
          cssum3 = cssum3 + (cs_soil(i)%ly(jj)%cs(3) * hru(i)%area_ha) !kg
        enddo
      enddo
      cs_basin(21) = cssum1
      cs_basin(50) = cssum2
      cs_basin(79) = cssum3

      !total soil constituent mass (sorbed)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%hru
        do jj=1,soil(i)%nly
          cssum1 = cssum1 + (cs_soil(i)%ly(jj)%cs_sorb(1) * hru(i)%area_ha) !kg
          cssum2 = cssum2 + (cs_soil(i)%ly(jj)%cs_sorb(2) * hru(i)%area_ha) !kg
          cssum3 = cssum3 + (cs_soil(i)%ly(jj)%cs_sorb(3) * hru(i)%area_ha) !kg
        enddo
      enddo
      cs_basin(22) = cssum1
      cs_basin(51) = cssum2
      cs_basin(80) = cssum3

      
      !aquifer --------------------------------------------------------------------------------------------------------
      
      !groundwater constituent loading to stream
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      if (bsn_cc%gwflow == 1) then !gwflow is active; loop through cells
        if (gw_solute_flag == 1) then
          do i=1,ncell
            sol_index = 2 + cs_db%num_salts
            !seo4
            cssum1 = cssum1 + (gwsol_ss(i)%solute(sol_index+1)%gwsw * (-1) / 1000.) !kg  
            cssum1 = cssum1 + (gwsol_ss(i)%solute(sol_index+1)%swgw * (-1) / 1000.) !kg 
            cssum1 = cssum1 + (gwsol_ss(i)%solute(sol_index+1)%satx * (-1) / 1000.) !kg 
            !seo3
            cssum2 = cssum2 + (gwsol_ss(i)%solute(sol_index+2)%gwsw * (-1) / 1000.) !kg  
            cssum2 = cssum2 + (gwsol_ss(i)%solute(sol_index+2)%swgw * (-1) / 1000.) !kg 
            cssum2 = cssum2 + (gwsol_ss(i)%solute(sol_index+2)%satx * (-1) / 1000.) !kg 
            !boron
            cssum3 = cssum3 + (gwsol_ss(i)%solute(sol_index+3)%gwsw * (-1) / 1000.) !kg  
            cssum3 = cssum3 + (gwsol_ss(i)%solute(sol_index+3)%swgw * (-1) / 1000.) !kg 
            cssum3 = cssum3 + (gwsol_ss(i)%solute(sol_index+3)%satx * (-1) / 1000.) !kg 
          enddo
        endif    
      else !normal aquifer module
        do i=1,sp_ob%aqu
          cssum1 = cssum1 + acsb_d(i)%cs(1)%csgw !kg
          cssum2 = cssum2 + acsb_d(i)%cs(2)%csgw !kg
          cssum3 = cssum3 + acsb_d(i)%cs(3)%csgw !kg
        enddo
      endif
      cs_basin(23) = cssum1
      cs_basin(52) = cssum2
      cs_basin(81) = cssum3
      
      !recharge to aquifer
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      if (bsn_cc%gwflow == 1) then !gwflow is active
        if (gw_solute_flag == 1) then
          do i=1,ncell
            sol_index = 2 + cs_db%num_salts
            cssum1 = cssum1 + (gwsol_ss(i)%solute(sol_index+1)%rech / 1000.) !kg seo4
            cssum2 = cssum2 + (gwsol_ss(i)%solute(sol_index+2)%rech / 1000.) !kg seo3  
            cssum3 = cssum3 + (gwsol_ss(i)%solute(sol_index+3)%rech / 1000.) !kg boron  
          enddo
        endif 
      else
        do i=1,sp_ob%aqu
          cssum1 = cssum1 + acsb_d(i)%cs(1)%rchrg !kg
          cssum2 = cssum2 + acsb_d(i)%cs(2)%rchrg !kg
          cssum3 = cssum3 + acsb_d(i)%cs(3)%rchrg !kg
        enddo
      endif
      cs_basin(24) = cssum1
      cs_basin(53) = cssum2
      cs_basin(82) = cssum3
      
      !seepage from aquifer
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      do i=1,sp_ob%aqu
        cssum1 = cssum1 + acsb_d(i)%cs(1)%seep !kg
        cssum2 = cssum2 + acsb_d(i)%cs(2)%seep !kg
        cssum3 = cssum3 + acsb_d(i)%cs(3)%seep !kg
      enddo
      cs_basin(25) = cssum1
      cs_basin(54) = cssum2
      cs_basin(83) = cssum3

      !chemical reactions - aquifer
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      if (bsn_cc%gwflow == 1) then !gwflow is active
        if (gw_solute_flag == 1) then
          do i=1,ncell
            sol_index = 2 + cs_db%num_salts
            cssum1 = cssum1 + ((gwsol_ss(i)%solute(sol_index+1)%rcti+gwsol_ss(i)%solute(sol_index+1)%rcto) / 1000.) !kg seo4 
            cssum2 = cssum2 + ((gwsol_ss(i)%solute(sol_index+2)%rcti+gwsol_ss(i)%solute(sol_index+2)%rcto) / 1000.) !kg seo3
            cssum3 = cssum3 + ((gwsol_ss(i)%solute(sol_index+3)%rcti+gwsol_ss(i)%solute(sol_index+3)%rcto) / 1000.) !kg boron  
          enddo
        endif 
      else
        do i=1,sp_ob%aqu
          cssum1 = cssum1 + acsb_d(i)%cs(1)%rctn !kg
          cssum2 = cssum2 + acsb_d(i)%cs(2)%rctn !kg
          cssum3 = cssum3 + acsb_d(i)%cs(3)%rctn !kg
        enddo
      endif
      cs_basin(26) = cssum1
      cs_basin(55) = cssum2
      cs_basin(84) = cssum3
      
      !sorption reaction - aquifer
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      if (bsn_cc%gwflow == 1) then !gwflow is active
        if (gw_solute_flag == 1) then
          do i=1,ncell
            sol_index = 2 + cs_db%num_salts
            cssum1 = cssum1 + (gwsol_ss(i)%solute(sol_index+1)%sorb / 1000.) !kg seo4 
            cssum2 = cssum2 + (gwsol_ss(i)%solute(sol_index+2)%sorb / 1000.) !kg seo3
            cssum3 = cssum3 + (gwsol_ss(i)%solute(sol_index+3)%sorb / 1000.) !kg boron  
          enddo
        endif 
      else
        do i=1,sp_ob%aqu
          cssum1 = cssum1 + acsb_d(i)%cs(1)%sorb !kg
          cssum2 = cssum2 + acsb_d(i)%cs(2)%sorb !kg
          cssum3 = cssum3 + acsb_d(i)%cs(3)%sorb !kg
        enddo
      endif
      cs_basin(27) = cssum1
      cs_basin(56) = cssum2
      cs_basin(85) = cssum3

      !total groundwater constituent mass (dissolved)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      if (bsn_cc%gwflow == 1) then !gwflow is active
        if (gw_solute_flag == 1) then
          do i=1,ncell
            if(gw_state(i)%stat > 0) then
              sol_index = 2 + cs_db%num_salts
              cssum1 = cssum1 + (gwsol_state(i)%solute(sol_index+1)%mass / 1000.) !kg
              cssum2 = cssum2 + (gwsol_state(i)%solute(sol_index+2)%mass / 1000.) !kg
              cssum3 = cssum3 + (gwsol_state(i)%solute(sol_index+3)%mass / 1000.) !kg
            endif
          enddo
        endif    
      else !normal aquifer module
        do i=1,sp_ob%aqu
          cssum1 = cssum1 + cs_aqu(i)%cs(1) !kg
          cssum2 = cssum2 + cs_aqu(i)%cs(2) !kg
          cssum3 = cssum3 + cs_aqu(i)%cs(3) !kg
        enddo
      endif
      cs_basin(28) = cssum1
      cs_basin(57) = cssum2
      cs_basin(86) = cssum3
      
      !total groundwater constituent mass (sorbed)
      cssum1 = 0.
      cssum2 = 0.
      cssum3 = 0.
      ob_ctr = sp_ob1%aqu !first aquifer object
      do i=1,sp_ob%aqu
        cssum1 = cssum1 + (cs_aqu(i)%cs_sorb(1) * ob(ob_ctr)%area_ha) !kg
        cssum2 = cssum2 + (cs_aqu(i)%cs_sorb(2) * ob(ob_ctr)%area_ha) !kg
        cssum3 = cssum3 + (cs_aqu(i)%cs_sorb(3) * ob(ob_ctr)%area_ha) !kg
        ob_ctr = ob_ctr + 1
      enddo
      cs_basin(29) = cssum1
      cs_basin(58) = cssum2
      cs_basin(87) = cssum3
      
      
      !write out results; sum for month, year, avg annual -------------------------------------------------------------
      
      !write out basin-wide constituent fluxes to file (kg)
      write(6080,7000) time%yrc,time%mo,time%day,(cs_basin(i),i=1,87)
      
      !save fluxes for monthly, yearly, and ave annual output
      do i=1,87
        cs_basin_mo(i) = cs_basin_mo(i) + cs_basin(i)
        cs_basin_yr(i) = cs_basin_yr(i) + cs_basin(i)
        cs_basin_aa(i) = cs_basin_aa(i) + cs_basin(i)
      enddo
      
      !monthly print
      if (time%end_mo == 1) then
        num_days = float(time%day_mo)
        !if state variables, then divide by the number of days
        cs_basin_mo(21) = cs_basin_mo(21) / num_days
        cs_basin_mo(22) = cs_basin_mo(22) / num_days
        cs_basin_mo(28) = cs_basin_mo(28) / num_days
        cs_basin_mo(29) = cs_basin_mo(29) / num_days
        cs_basin_mo(50) = cs_basin_mo(50) / num_days
        cs_basin_mo(51) = cs_basin_mo(51) / num_days
        cs_basin_mo(57) = cs_basin_mo(57) / num_days
        cs_basin_mo(58) = cs_basin_mo(58) / num_days
        cs_basin_mo(79) = cs_basin_mo(79) / num_days
        cs_basin_mo(80) = cs_basin_mo(80) / num_days
        cs_basin_mo(86) = cs_basin_mo(86) / num_days
        cs_basin_mo(87) = cs_basin_mo(87) / num_days
        write(6082,7000) time%yrc,time%mo,time%day,(cs_basin_mo(i),i=1,87)
        cs_basin_mo = 0.
      endif
      
      !yearly print
      if (time%end_yr == 1) then
        num_days = float(time%day_end_yr)
        !if state variables, then divide by the number of days 
        cs_basin_yr(21) = cs_basin_yr(21) / num_days
        cs_basin_yr(22) = cs_basin_yr(22) / num_days
        cs_basin_yr(28) = cs_basin_yr(28) / num_days
        cs_basin_yr(29) = cs_basin_yr(29) / num_days
        cs_basin_yr(50) = cs_basin_yr(50) / num_days
        cs_basin_yr(51) = cs_basin_yr(51) / num_days
        cs_basin_yr(57) = cs_basin_yr(57) / num_days
        cs_basin_yr(58) = cs_basin_yr(58) / num_days
        cs_basin_yr(79) = cs_basin_yr(79) / num_days
        cs_basin_yr(80) = cs_basin_yr(80) / num_days
        cs_basin_yr(86) = cs_basin_yr(86) / num_days
        cs_basin_yr(87) = cs_basin_yr(87) / num_days
        write(6084,7000) time%yrc,time%mo,time%day,(cs_basin_yr(i),i=1,87)
        cs_basin_yr = 0.
      endif
      
      !average annual print
      if (time%end_sim == 1) then
        do i=1,20
          cs_basin_aa(i) = cs_basin_aa(i) / time%nbyr
        enddo
        do i=23,27
          cs_basin_aa(i) = cs_basin_aa(i) / time%nbyr
        enddo
        do i=30,49
          cs_basin_aa(i) = cs_basin_aa(i) / time%nbyr
        enddo
        do i=52,56
          cs_basin_aa(i) = cs_basin_aa(i) / time%nbyr
        enddo
        do i=59,78
          cs_basin_aa(i) = cs_basin_aa(i) / time%nbyr
        enddo
        do i=81,85
          cs_basin_aa(i) = cs_basin_aa(i) / time%nbyr
        enddo
        num_days = time%days_prt
        cs_basin_aa(20) = cs_basin_aa(20) / num_days
        cs_basin_aa(21) = cs_basin_aa(21) / num_days
        cs_basin_aa(27) = cs_basin_aa(27) / num_days
        cs_basin_aa(28) = cs_basin_aa(28) / num_days
        cs_basin_aa(48) = cs_basin_aa(48) / num_days
        cs_basin_aa(49) = cs_basin_aa(49) / num_days
        cs_basin_aa(55) = cs_basin_aa(55) / num_days
        cs_basin_aa(56) = cs_basin_aa(56) / num_days
        cs_basin_aa(76) = cs_basin_aa(76) / num_days
        cs_basin_aa(77) = cs_basin_aa(77) / num_days
        cs_basin_aa(83) = cs_basin_aa(83) / num_days
        cs_basin_aa(84) = cs_basin_aa(84) / num_days
        write(6086,7000) time%yrc,time%mo,time%day,(cs_basin_aa(i),i=1,87)
      endif
      
      
      !zero out balance arrays for next day ---------------------------------------------------------------------------
      !hru
      do i=1,sp_ob%hru
        do m=1,cs_db%num_cs
          !HRUs
          hcsb_d(i)%cs(m)%soil = 0.
          hcsb_d(i)%cs(m)%surq = 0.
          hcsb_d(i)%cs(m)%sedm = 0.
          hcsb_d(i)%cs(m)%latq = 0.
          hcsb_d(i)%cs(m)%urbq = 0.
          hcsb_d(i)%cs(m)%wetq = 0.
          hcsb_d(i)%cs(m)%tile = 0.
          hcsb_d(i)%cs(m)%perc = 0.
          hcsb_d(i)%cs(m)%wtsp = 0.
          hcsb_d(i)%cs(m)%irsw = 0.
          hcsb_d(i)%cs(m)%irgw = 0.
          hcsb_d(i)%cs(m)%irwo = 0.
          hcsb_d(i)%cs(m)%rain = 0.
          hcsb_d(i)%cs(m)%dryd = 0.
          hcsb_d(i)%cs(m)%fert = 0.
          hcsb_d(i)%cs(m)%uptk = 0.
          hcsb_d(i)%cs(m)%rctn = 0.
          hcsb_d(i)%cs(m)%sorb = 0.
          !wetlands
          wetcs_d(i)%cs(m)%inflow = 0.
          wetcs_d(i)%cs(m)%outflow = 0.
          wetcs_d(i)%cs(m)%seep = 0.
          wetcs_d(i)%cs(m)%settle = 0.
          wetcs_d(i)%cs(m)%rctn = 0.
          wetcs_d(i)%cs(m)%prod = 0.
          wetcs_d(i)%cs(m)%fert = 0.
          wetcs_d(i)%cs(m)%irrig = 0.
          wetcs_d(i)%cs(m)%div = 0.
        enddo
      enddo
      !aquifer
      do i=1,sp_ob%aqu
        do m=1,cs_db%num_cs
          acsb_d(i)%cs(m)%csgw = 0.
          acsb_d(i)%cs(m)%rchrg = 0.
          acsb_d(i)%cs(m)%seep = 0.
          acsb_d(i)%cs(m)%irr = 0.
          acsb_d(i)%cs(m)%div = 0.
          acsb_d(i)%cs(m)%sorb = 0.
          acsb_d(i)%cs(m)%rctn = 0.
        enddo
      enddo
      !point source
      do i=1,sp_ob%recall
        do m=1,cs_db%num_cs
          reccsb_d(i)%cs(m) = 0.
          recoutcsb_d(i)%cs(m) = 0.
        enddo
      enddo
      !reservoirs
      do i=1,sp_ob%res
        do m=1,cs_db%num_cs
          rescs_d(i)%cs(m)%inflow = 0.
          rescs_d(i)%cs(m)%outflow = 0.
          rescs_d(i)%cs(m)%seep = 0.
          rescs_d(i)%cs(m)%fert = 0.
          rescs_d(i)%cs(m)%irrig = 0.
          rescs_d(i)%cs(m)%div = 0.
        enddo
      enddo
      !channels
      do i=1,sp_ob%chandeg
        do m=1,cs_db%num_cs
          chcs_d(i)%cs(m)%irr = 0.
          chcs_d(i)%cs(m)%div = 0.
          chcs_d(i)%cs(m)%gw_in = 0.
        enddo
      enddo
      
      !if gwflow active: zero out daily cell values for recharge, reactions, and sorption (others are zeroed out in gwflow_simulate)
      if (bsn_cc%gwflow == 1) then
        if (gw_solute_flag == 1) then
          sol_index = 2 + cs_db%num_salts + 1
          do m=1,cs_db%num_cs
            do i=1,ncell
              gwsol_ss(i)%solute(sol_index)%rech = 0.
              gwsol_ss(i)%solute(sol_index)%rcti = 0.
              gwsol_ss(i)%solute(sol_index)%rcto = 0.
              gwsol_ss(i)%solute(sol_index)%minl = 0.
              gwsol_ss(i)%solute(sol_index)%sorb = 0.
            enddo
            sol_index = sol_index + 1
					enddo	!go to next constituent
        endif
      endif
      
      
7000  format(i8,i8,i8,100e16.8)
7001  format(20e16.8)
7002  format(i8,50f16.8)

      return
      end
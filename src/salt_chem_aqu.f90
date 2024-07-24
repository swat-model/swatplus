      subroutine salt_chem_aqu
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU
!!    (precipitation-dissolution, complexation, cation exchange)
       
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    sol_csalt     |mg Salt/L     |concentration of salt ions in solution in each soil layer
!!    saltgw_conc   !mg Salt/L     |concentration of salt ions in groundwater
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_csalt     |mg Salt/L     |concentration of salt ions in solution in each soil layer
!!    saltgw_conc   !mg Salt/L     |concentration of salt ions in groundwater
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use aquifer_module
      use basin_module
      use constituent_mass_module
      use salt_data_module
      use soil_module
      use salt_data_module
      use hydrograph_module, only : ob,icmd
      use salt_module
      use salt_aquifer
      
      implicit none

      integer iaq
      integer m,iter_count
      real    ion1,ion2,ion3,ion4,ion5,ion6,ion7,ion8,hru_area_m2,gw_volume
      real    waterC,Sol_CaCO3_p(1000),Sol_MgCO3_p(1000),Sol_CaSO4_p(1000),Sol_MgSO4_p(1000),Sol_NaCl_p(1000)
      real    I_Prep_in,I_diff,SkipedIEX
      real    mass_before,mass_after         
      double precision IonStr,IS_temp,K_ADJ1,K_ADJ2,K_ADJ3,K_ADJ4,K_ADJ5,error1ST,error2ND,error3RD,errorTotal
      
      !aquifer ID
      iaq = ob(icmd)%num
      
      !area of the aquifer in m2
      hru_area_m2 = ob(icmd)%area_ha * 10000.

      mass_before = 0.
      mass_after = 0.

      !water content = saturated water content for saturated zone of aquifer
      waterC = 0.25 !for now, set equal to an assumed value of porosity

      !retrieve the solid salt mineral concentration in the aquifer, and perform conversions
      Sol_CaCO3_p(1) = cs_aqu(iaq)%salt_min(1)
      Sol_MgCO3_p(1) = cs_aqu(iaq)%salt_min(2)
      Sol_CaSO4_p(1) = cs_aqu(iaq)%salt_min(3)
      Sol_MgSO4_p(1) = cs_aqu(iaq)%salt_min(4)
      Sol_NaCl_p(1)  = cs_aqu(iaq)%salt_min(5)
      Sol_CaCO3(1) = (Sol_CaCO3_p(1)/100) *(1.855/(waterC*100.0))*1000.0 
      Sol_MgCO3(1) = (Sol_MgCO3_p(1)/100) *(1.855/(waterC*84.31))*1000.0 
      Sol_CaSO4(1) = (Sol_CaSO4_p(1)/100) *(1.855/(waterC*136.14))*1000.0 
      Sol_MgSO4(1) = (Sol_MgSO4_p(1)/100) *(1.855/(waterC*120.36))*1000.0 
      Sol_NaCl(1)  = (Sol_NaCl_p(1)/100) *(1.855/(waterC*58.44))*1000.0 
        
      !get concentration of salt ions in the aquifer
      gw_volume = (aqu_d(iaq)%stor/1000.)*(ob(icmd)%area_ha*10000.) !m3 of groundwater
      do m=1,cs_db%num_salts
        if(cs_aqu(iaq)%salt(m).lt.0) then
          cs_aqu(iaq)%salt(m) = 0.
		endif
        if(cs_aqu(iaq)%saltc(m).lt.0) then
          cs_aqu(iaq)%saltc(m) = 0.
		endif
        mass_before = mass_before + cs_aqu(iaq)%salt(m) !kg of salt
        if(gw_volume.gt.0) then
          cs_aqu(iaq)%saltc(m) = (cs_aqu(iaq)%salt(m) * 1000.) / gw_volume !g/m3 = mg/L
        else
          cs_aqu(iaq)%saltc(m) = 0.
        endif
      enddo
      
      !retrieve the current (daily) salt ion solution concentrations from groundwater (mg/L) and convert to mol/L
      ion1 = cs_aqu(iaq)%saltc(1) !sulfate
      ion2 = cs_aqu(iaq)%saltc(2) !calcium
      ion3 = cs_aqu(iaq)%saltc(3) !magnesium
      ion4 = cs_aqu(iaq)%saltc(4) !sodium
      ion5 = cs_aqu(iaq)%saltc(5) !potassium
      ion6 = cs_aqu(iaq)%saltc(6) !chloride
      ion7 = cs_aqu(iaq)%saltc(7) !carbonate
      ion8 = cs_aqu(iaq)%saltc(8) !bicarbonate
      Sul_Conc(1) = ion1*((1.0/1000)*(1.0/96.06)) !sulfate
      Cal_Conc(1) = ion2*((1.0/1000)*(1.0/40.078)) !calcium
      Mg_Conc(1) = ion3*((1.0/1000)*(1.0/24.305)) !magnesium
      Sod_Conc(1) = ion4*((1.0/1000)*(1.0/23.0)) !sodium
      Pot_Conc(1) = ion5*((1.0/1000)*(1.0/39.0)) !potassium
      Cl_Conc(1) = ion6*((1.0/1000)*(1.0/35.45)) !chloride
      Car_Conc(1) = ion7*((1.0/1000)*(1.0/60.01)) !carbonate
      BiCar_Conc(1) = ion8*((1.0/1000)*(1.0/61.01)) !bicarbonate

      !define the activity coefficient using Extended Debye-Huckel equation
      call Ionic_strength(IS_temp,Cal_Conc(1),Sul_Conc(1),Car_Conc(1),BiCar_Conc(1),Mg_Conc(1),Sod_Conc(1),&                    
        Pot_Conc(1))
      IonStr = IS_temp
      I_Prep_in = IonStr
      I_diff = 1

      !assign 1 for concentration to able count and store the data in while loop
      c11 = 1
      c22 = 1
      salt_c3 = 1
      salt_c4 = 1
      c5 = 1

      !This while loop compares I from precipitation loop with I from complexation. If they are about the same
      !(I_diff<0.001), it will go to the next row
      !do while (I_diff.ge.1e-2)
        
        call activity_coefficient(I_Prep_in)

        !update the K values 
        K_ADJ1 = LAMDA(1)*LAMDA(3)
        K_ADJ2 = LAMDA(5)*LAMDA(3)
        K_ADJ3 = LAMDA(1)*LAMDA(2)
        K_ADJ4 = LAMDA(5)*LAMDA(2)
        K_ADJ5 = LAMDA(6)*LAMDA(6)

        salt_K1 = Ksp12/K_ADJ1
        salt_K2 = Ksp22/K_ADJ2
        salt_K3 = Ksp32/K_ADJ3
        salt_K4 = Ksp42/K_ADJ4
        salt_K5 = Ksp52/K_ADJ5

        errorTotal = 1

        !Precipitation-Dissolution package ----------------------------------------------------------------
        iter_count = 1
        do while (errorTotal.GE.1e-3)
          call CaCO3
          call MgCO3
          call CaSO4
          call MgSO4
          call NaCl

          !check the errors
          error1ST = Car_Conc(c22+1)-Car_Conc(c22+2)
          error2ND = Cal_Conc(c11+1)-Cal_Conc(c11+2)
          error3RD = Sul_Conc(salt_c4+1)-Sul_Conc(salt_c4+2)
          !errorTotal = ABS(MAX(error1ST,error2ND,error3RD))
          errorTotal = max(abs(error1ST),abs(error2ND),abs(error3RD))
        
          !update the counter for ions concentration
          c11 = c11 + 2
          c22 = c22 + 2
          salt_c3 = salt_c3 + 2
          salt_c4 = salt_c4 + 2
          c5 = c5 + 1

          !update iteration count
          iter_count = iter_count + 1
          if(iter_count.gt.500) then
            goto 20
          endif

        enddo

        !convert mol/liter to ppm
20      upion1 = Sul_Conc(salt_c4)
        upion2 = Cal_Conc(c11)
        upion3 = Mg_Conc(salt_c3)
        upion4 = Sod_Conc(c5)
        upion5 = Pot_Conc(1)
        upion6 = Cl_Conc(c5)
        upion7 = Car_Conc(c22)
        upion8 = BiCar_Conc(1)
        
      !enddo

      !update concentration after precipitation
      upion1 = Sul_Conc(salt_c4)*(96.06*1000.0)
      upion2 = Cal_Conc(c11)*(40.078*1000.0)
      upion3 = Mg_Conc(salt_c3)*(24.305*1000.0)
      upion4 = Sod_Conc(c5)*(23.0*1000.0)
      upion5 = Pot_Conc(1)*(39.0*1000.0)
      upion6 = Cl_Conc(c5)*(35.45*1000.0)
      upion7 = Car_Conc(c22)*(60.01*1000.0)
      upion8= BiCar_Conc(1)*(61.01*1000.0) 

      !Cation Exchange package --------------------------------------------------------------------------
      call cationexchange

      !skipping cation exchange if: 
      if (upion2.le.0 .or. upion3.le.0 .or. upion4.le.0 .or. upion5.le.0) then
        upion2 = Cal_Conc(c11)*(40.078*1000.0)
        upion3 = Mg_Conc(salt_c3)*(24.305*1000.0)
        upion4 = Sod_Conc(c5)*(23.0*1000.0)
        upion5 = Pot_Conc(1)*(39.0*1000.0)
        SkipedIEX = SkipedIEX + 1
      else
        upion2 = upion2
        upion3 = upion3
        upion4 = upion4
        upion5 = upion5
      endif

      !save data (upion = updated groundwater salt ion concentration)
      cs_aqu(iaq)%saltc(1) = upion1
      cs_aqu(iaq)%saltc(2) = upion2
      cs_aqu(iaq)%saltc(3) = upion3
      cs_aqu(iaq)%saltc(4) = upion4
      cs_aqu(iaq)%saltc(5) = upion5
      cs_aqu(iaq)%saltc(6) = upion6
      cs_aqu(iaq)%saltc(7) = upion7
      cs_aqu(iaq)%saltc(8) = upion8

      !convert to kg/ha, for regular SWAT routines
      gw_volume = (aqu_d(iaq)%stor/1000.)*(ob(icmd)%area_ha*10000.) !m3 of groundwater in HRU aquifer
      do m=1,cs_db%num_salts
        cs_aqu(iaq)%salt(m) = (cs_aqu(iaq)%saltc(m)*gw_volume) / 1000. !kg of salt
      enddo

      !check mass
      do m=1,cs_db%num_salts !total salt mass in soil layer
        mass_after = mass_after + cs_aqu(iaq)%salt(m) !kg of salt
      enddo

      !convert solids concentrations to solid percentage, and save
      Sol_CaCO3_p(c5) = (Sol_CaCO3(c5)*100.0)/((1.855/(waterC*100.0))*1000.0) 
      Sol_MgCO3_p(c5) = (Sol_MgCO3(c5)*100.0)/((1.855/(waterC*84.31))*1000.0) 
      Sol_CaSO4_p(c5) = (Sol_CaSO4(c5)*100.0)/((1.855/(waterC*136.14))*1000.0) 
      Sol_MgSO4_p(c5) = (Sol_MgSO4(c5)*100.0)/((1.855/(waterC*120.36))*1000.0) 
      Sol_NaCl_p(c5)  = (Sol_NaCl(c5)*100.0)/((1.855/(waterC*58.44))*1000.0) 
           
      !save solids concentration for the HRU and the layer
      cs_aqu(iaq)%salt_min(1) = Sol_CaCO3_p(c5)
      cs_aqu(iaq)%salt_min(2) = Sol_MgCO3_p(c5)
      cs_aqu(iaq)%salt_min(3) = Sol_CaSO4_p(c5)
      cs_aqu(iaq)%salt_min(4) = Sol_MgSO4_p(c5)
      cs_aqu(iaq)%salt_min(5) = Sol_NaCl_p(c5)      

      !store HRU value for aquifer salt (solid --> dissolved)
      asaltb_d(iaq)%salt(1)%diss = mass_after - mass_before
      
      return
      end
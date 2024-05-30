      subroutine salt_chem_soil_single(hru_num,lay_num,waterC) !rtb salt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates salt ion concentrations based on equilibrium chemical reactions
!!    (precipitation-dissolution, complexation, cation exchange), for a specified HRU, for a specified layer

      use hru_module, only : hru,ihru
      use basin_module
      use constituent_mass_module
      use salt_data_module
      use soil_module
      use salt_data_module
      use salt_module
      use time_module

      implicit none

      integer j,jj,m,n,dum,iter_count,hru_num,lay_num
      real    ion1,ion2,ion3,ion4,ion5,ion6,ion7,ion8
      real    sol_water,sol_thick,waterC,sol_bd,&        
          Sol_CaCO3_p(1000),Sol_MgCO3_p(1000),Sol_CaSO4_p(1000),&        
          Sol_MgSO4_p(1000),Sol_NaCl_p(1000)
      real    I_Prep_in,I_diff,SkipedIEX
      double precision IonStr,IS_temp,&                 
          K_ADJ1,K_ADJ2,K_ADJ3,K_ADJ4,K_ADJ5,&                 
          error1ST,error2ND,error3RD,errorTotal
      
      
      !hru ID (incoming HRU number)
      j = hru_num

      !soil layer
      jj = lay_num
      
      !retrieve the solid salt mineral concentration
      Sol_CaCO3_p(1) = cs_soil(j)%ly(jj)%salt_min(1)
      Sol_MgCO3_p(1) = cs_soil(j)%ly(jj)%salt_min(2)
      Sol_CaSO4_p(1) = cs_soil(j)%ly(jj)%salt_min(3)
      Sol_MgSO4_p(1) = cs_soil(j)%ly(jj)%salt_min(4)
      Sol_NaCl_p(1)  = cs_soil(j)%ly(jj)%salt_min(5)
        
      !perform conversions
      sol_bd = soil(j)%phys(jj)%bd
      Sol_CaCO3(1) = (Sol_CaCO3_p(1)/100) *(sol_bd/(waterC*100.0))*1000.0 
      Sol_MgCO3(1) = (Sol_MgCO3_p(1)/100) *(sol_bd/(waterC*84.31))*1000.0 
      Sol_CaSO4(1) = (Sol_CaSO4_p(1)/100) *(sol_bd/(waterC*136.14))*1000.0 
      Sol_MgSO4(1) = (Sol_MgSO4_p(1)/100) *(sol_bd/(waterC*120.36))*1000.0 
      Sol_NaCl(1)  = (Sol_NaCl_p(1)/100) *(sol_bd/(waterC*58.44))*1000.0 
      
      !retrieve the current (daily) salt ion solution concentrations from soil water (mg/L) and convert to mol/L
      ion1 = soil_salt_conc(1) !sulfate
      ion2 = soil_salt_conc(2) !calcium
      ion3 = soil_salt_conc(3) !magnesium
      ion4 = soil_salt_conc(4) !sodium
      ion5 = soil_salt_conc(5) !potassium
      ion6 = soil_salt_conc(6) !chloride
      ion7 = soil_salt_conc(7) !carbonate
      ion8 = soil_salt_conc(8) !bicarbonate
      Sul_Conc(1) = ion1*((1.0/1000)*(1.0/96.06)) !sulfate
      Cal_Conc(1) = ion2*((1.0/1000)*(1.0/40.078)) !calcium
      Mg_Conc(1) = ion3*((1.0/1000)*(1.0/24.305)) !magnesium
      Sod_Conc(1) = ion4*((1.0/1000)*(1.0/23.0)) !sodium
      Pot_Conc(1) = ion5*((1.0/1000)*(1.0/39.0)) !potassium
      Cl_Conc(1) = ion6*((1.0/1000)*(1.0/35.45)) !chloride
      Car_Conc(1) = ion7*((1.0/1000)*(1.0/60.01)) !carbonate
      BiCar_Conc(1) = ion8*((1.0/1000)*(1.0/61.01)) !bicarbonate

      !define the activity coefficient using Extended Debye-Huckel equation
      call Ionic_strength(IS_temp,Cal_Conc(1),Sul_Conc(1),Car_Conc(1),&                    
          BiCar_Conc(1),Mg_Conc(1),Sod_Conc(1),Pot_Conc(1))
      IonStr = IS_temp
      I_Prep_in = IonStr
      I_diff = 1
        
      !assign 1 for concentration to able count and store the data in while loop
      c11 = 1
      c22 = 1
      salt_c3 = 1
      salt_c4 = 1
      c5 = 1

      call activity_coefficient(I_Prep_in)

      !update the K values 
      K_ADJ1 = LAMDA(1)*LAMDA(3)
      K_ADJ2 = LAMDA(5)*LAMDA(3)
      K_ADJ3 = LAMDA(1)*LAMDA(2)
      K_ADJ4 = LAMDA(5)*LAMDA(2)
      K_ADJ5 = LAMDA(6)*LAMDA(6)

      if(K_ADJ1.gt.0.) then
        salt_K1 = Ksp11/K_ADJ1
	else
        salt_K1 = 0.
	endif
      if(K_ADJ2.gt.0.) then
        salt_K2 = Ksp21/K_ADJ2
	else
        salt_K2 = 0.
	endif
      if(K_ADJ3.gt.0.) then
        salt_K3 = Ksp31/K_ADJ3
	else
        salt_K3 = 0.
	endif
      if(K_ADJ4.gt.0.) then
        salt_K4 = Ksp41/K_ADJ4
	else
        salt_K4 = 0.
	endif
      if(K_ADJ5.gt.0.) then
        salt_K5 = Ksp51/K_ADJ5
	else
        salt_K5 = 0.
	endif
          
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
          goto 10
        endif

      enddo 
      !************************ End Precipitation-Dissolution Package *************************

      !convert mol/liter to ppm
10    upion1 = Sul_Conc(salt_c4)
      upion2 = Cal_Conc(c11)
      upion3 = Mg_Conc(salt_c3)
      upion4 = Sod_Conc(c5)
      upion5 = Pot_Conc(1)
      upion6 = Cl_Conc(c5)
      upion7 = Car_Conc(c22)
      upion8 = BiCar_Conc(1)

      !update concentration after precipitation
      soil_salt_conc(1) = Sul_Conc(salt_c4)*(96.06*1000.0)
      soil_salt_conc(2) = Cal_Conc(c11)*(40.078*1000.0)
      soil_salt_conc(3) = Mg_Conc(salt_c3)*(24.305*1000.0)
      soil_salt_conc(4) = Sod_Conc(c5)*(23.0*1000.0)
      soil_salt_conc(5) = Pot_Conc(1)*(39.0*1000.0)
      soil_salt_conc(6) = Cl_Conc(c5)*(35.45*1000.0)
      soil_salt_conc(7) = Car_Conc(c22)*(60.01*1000.0)
      soil_salt_conc(8) = BiCar_Conc(1)*(61.01*1000.0) 

      return
      end !salt_chem_soil_single
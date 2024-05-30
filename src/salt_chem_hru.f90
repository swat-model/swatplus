      subroutine salt_chem_hru
      
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

      use hru_module, only : hru,ihru
      use basin_module
      use constituent_mass_module
      use salt_data_module
      use soil_module
      use salt_data_module
      use salt_module
      use time_module

      implicit none

      integer j,jj,m,iter_count
      real    ion1,ion2,ion3,ion4,ion5,ion6,ion7,ion8,&        
          hru_area_m2,water_volume
      real    sol_water,sol_thick,waterC,&        
          Sol_CaCO3_p(1000),Sol_MgCO3_p(1000),Sol_CaSO4_p(1000),&        
          Sol_MgSO4_p(1000),Sol_NaCl_p(1000)
      real    I_Prep_in,I_diff,SkipedIEX
      real    soil_volume,mass_before,mass_after,&        
          salt_mass_kg,soil_mass,sol_bd,&        
          mass_before_dis,mass_before_sol,total_before,&        
          mass_after_dis,mass_after_sol,total_after
      double precision IonStr,IS_temp,&                 
          K_ADJ1,K_ADJ2,K_ADJ3,K_ADJ4,K_ADJ5,&                 
          error1ST,error2ND,error3RD,errorTotal
      
      
      !hru ID
      j = ihru

      !area of the HRU in m2
      hru_area_m2 = hru(j)%area_ha * 10000.

      !loop through the soil layers
      mass_before = 0.
      mass_after = 0.

      mass_before_dis = 0.
      mass_before_sol = 0.
      mass_after_dis = 0.
      mass_after_sol = 0.
      total_before = 0.
      total_after = 0.

      do jj = 1,soil(j)%nly
        
        !calculate water content of the soil layer ----------------------------------------------------------
        sol_water = soil(j)%phys(jj)%st
        sol_thick = soil(j)%phys(jj)%thick
        waterC = sol_water / sol_thick

        !retrieve the solid salt mineral concentration
        Sol_CaCO3_p(1) = cs_soil(j)%ly(jj)%salt_min(1)
        Sol_MgCO3_p(1) = cs_soil(j)%ly(jj)%salt_min(2)
        Sol_CaSO4_p(1) = cs_soil(j)%ly(jj)%salt_min(3)
        Sol_MgSO4_p(1) = cs_soil(j)%ly(jj)%salt_min(4)
        Sol_NaCl_p(1)  = cs_soil(j)%ly(jj)%salt_min(5)
        
        !calculate total salt mass from salt minerals
        soil_volume = hru_area_m2 * (sol_thick/1000.) !m3 of soil
        soil_mass = soil_volume * (soil(j)%phys(jj)%bd*1000.) !kg of soil
        do m=1,5
          mass_before_sol = mass_before_sol + &          
              (soil_mass*(cs_soil(j)%ly(jj)%salt_min(m)/100.))
        enddo

        !perform conversions
        sol_bd = soil(j)%phys(jj)%bd
        if(waterC.eq.0.) waterC = 0.05
        Sol_CaCO3(1) = (Sol_CaCO3_p(1)/100) *&                 
            (sol_bd/(waterC*100.0))*1000.0 
        Sol_MgCO3(1) = (Sol_MgCO3_p(1)/100) *&                 
            (sol_bd/(waterC*84.31))*1000.0 
        Sol_CaSO4(1) = (Sol_CaSO4_p(1)/100) *&                 
            (sol_bd/(waterC*136.14))*1000.0 
        Sol_MgSO4(1) = (Sol_MgSO4_p(1)/100) *&                 
            (sol_bd/(waterC*120.36))*1000.0 
        Sol_NaCl(1)  = (Sol_NaCl_p(1)/100) *&                 
            (sol_bd/(waterC*58.44))*1000.0 
        
        !get concentration of salt ions in the layer
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2
        do m=1,cs_db%num_salts
          if(cs_soil(j)%ly(jj)%salt(m).lt.0) then
            cs_soil(j)%ly(jj)%salt(m) = 0.
			endif
          if(cs_soil(j)%ly(jj)%saltc(m).lt.0) then
            cs_soil(j)%ly(jj)%saltc(m) = 0.
			endif
          salt_mass_kg = cs_soil(j)%ly(jj)%salt(m) * hru(j)%area_ha !kg of salt
          mass_before = mass_before + salt_mass_kg
          if(water_volume.gt.0) then
            cs_soil(j)%ly(jj)%saltc(m) = (salt_mass_kg * 1000.) / &                                    
                water_volume                   !g/m3 = mg/L
          else
            cs_soil(j)%ly(jj)%saltc(m) = 0.
          endif
        enddo

        !total dissolved mass
        mass_before_dis = mass_before_dis + mass_before

        !total salt mass in soil layer
        total_before = total_before + (mass_before_dis+mass_before_sol)

        !retrieve the current (daily) salt ion solution concentrations from soil water (mg/L) and convert to mol/L
        ion1 = cs_soil(j)%ly(jj)%saltc(1) !sulfate
        ion2 = cs_soil(j)%ly(jj)%saltc(2) !calcium
        ion3 = cs_soil(j)%ly(jj)%saltc(3) !magnesium
        ion4 = cs_soil(j)%ly(jj)%saltc(4) !sodium
        ion5 = cs_soil(j)%ly(jj)%saltc(5) !potassium
        ion6 = cs_soil(j)%ly(jj)%saltc(6) !chloride
        ion7 = cs_soil(j)%ly(jj)%saltc(7) !carbonate
        ion8 = cs_soil(j)%ly(jj)%saltc(8)!bicarbonate
        
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
            BiCar_Conc(1),Mg_Conc(1),Sod_Conc(1),&                      
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
10        upion1 = Sul_Conc(salt_c4)
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
        if (upion2.le.0 .or. upion3.le.0 .or. upion4.le.0 .or. &      
            upion5.le.0) then
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

        !save data (upion = updated soil water salt ion concentration)
        cs_soil(j)%ly(jj)%saltc(1) = upion1
        cs_soil(j)%ly(jj)%saltc(2) = upion2
        cs_soil(j)%ly(jj)%saltc(3) = upion3
        cs_soil(j)%ly(jj)%saltc(4) = upion4
        cs_soil(j)%ly(jj)%saltc(5) = upion5
        cs_soil(j)%ly(jj)%saltc(6) = upion6
        cs_soil(j)%ly(jj)%saltc(7) = upion7
        cs_soil(j)%ly(jj)%saltc(8) = upion8

        !convert to kg/ha, for regular SWAT+ routines
        hru_area_m2 = hru(j)%area_ha * 10000. !ha --> m2
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2
        do m=1,cs_db%num_salts
          cs_soil(j)%ly(jj)%salt(m) = &                 
              (cs_soil(j)%ly(jj)%saltc(m)/1000.) * water_volume&                 
              / hru(j)%area_ha   
        enddo

        !check mass
        do m=1,cs_db%num_salts !total salt mass in soil layer
          salt_mass_kg = cs_soil(j)%ly(jj)%salt(m) * hru(j)%area_ha !kg of salt
          mass_after = mass_after + salt_mass_kg
        enddo

        !total dissolved mass
        mass_after_dis = mass_after_dis + mass_after

        !convert solids concentrations to solid percentage, and save
        sol_bd = soil(j)%phys(jj)%bd
        Sol_CaCO3_p(c5) = (Sol_CaCO3(c5)*100.0)&                    
            /((sol_bd/(waterC*100.0))*1000.0) 
        Sol_MgCO3_p(c5) = (Sol_MgCO3(c5)*100.0)&                    
            /((sol_bd/(waterC*84.31))*1000.0) 
        Sol_CaSO4_p(c5) = (Sol_CaSO4(c5)*100.0)&                    
            /((sol_bd/(waterC*136.14))*1000.0) 
        Sol_MgSO4_p(c5) = (Sol_MgSO4(c5)*100.0)&                    
            /((sol_bd/(waterC*120.36))*1000.0) 
        Sol_NaCl_p(c5)  = (Sol_NaCl(c5)*100.0)&                    
            /((sol_bd/(waterC*58.44))*1000.0) 
           
        !save solids concentration for the HRU and the layer
        cs_soil(j)%ly(jj)%salt_min(1) = Sol_CaCO3_p(c5)
        cs_soil(j)%ly(jj)%salt_min(2) = Sol_MgCO3_p(c5)
        cs_soil(j)%ly(jj)%salt_min(3) = Sol_CaSO4_p(c5)
        cs_soil(j)%ly(jj)%salt_min(4) = Sol_MgSO4_p(c5)
        cs_soil(j)%ly(jj)%salt_min(5) = Sol_NaCl_p(c5)
        do m=1,5
          if(cs_soil(j)%ly(jj)%salt_min(m) > 100.) then
            cs_soil(j)%ly(jj)%salt_min(m) = 100.
          endif
        enddo
        
        !calculate total salt mass from salt minerals
        do m=1,5
          mass_after_sol = mass_after_sol + &          
              (soil_mass*(cs_soil(j)%ly(jj)%salt_min(m)/100.))
        enddo

        !total salt mass in soil layer
        total_after = total_after + (mass_after_dis + mass_after_sol)

      enddo !go to next soil layer

      !store HRU value for soil salt (solid --> dissolved)
      mass_before = mass_before / hru(j)%area_ha !kg/ha
      mass_after = mass_after / hru(j)%area_ha !kg/ha
      hsaltb_d(j)%salt(1)%diss = mass_after - mass_before

101   format(i8,i8,i8,i8,50(e13.4)) 
      
      return
      end

      


      ! Calculate Ionic Strength of Water *******************************************************************
      subroutine Ionic_Strength(IS_temp,A,B,C,D,E,F,G)
      
      use salt_data_module
      
      implicit none

      double precision IS_temp,A,B,C,D,E,F,G
      real CharBal(7)
      
      DATA CharBal/2.0, -2.0, -2.0, -1.0, 2.0, 1.0, 1.0/     
      
      IS_temp = 0.5*(CharBal(1)**2*A&
          +CharBal(2)**2*B&
          +CharBal(3)**2*C&
          +CharBal(4)**2*D&
          +CharBal(5)**2*E&
          +CharBal(6)**2*F&
          +CharBal(7)**2*G)
     
      return
      end
      
      

      
      
      ! Calculate Activity Coefficient **********************************************************************
      subroutine activity_coefficient(I_Prep_in)
      
      use salt_data_module
      use organic_mineral_mass_module

      real CharBal(7),a_size(7),I_Prep_in
      DATA CharBal/2.0, -2.0, -2.0, -1.0, 2.0, 1.0, 1.0/
      DATA a_size/6.0, 4.0, 4.5, 4.0, 8.0, 4.5, 3.0/
      integer :: ii
      real :: A,B
      A = 0.5 !at 298 K
      B = 0.33 !at 298 K
      
      if (I_Prep_in.LE.1e-1) then
        do ii = 1,7
          LAMDA(ii)= 10.0**(-A*CharBal(ii)**2.0  &       
              *(I_Prep_in**0.5/(1+B*a_size(ii)*I_Prep_in**0.5)))
        enddo
      elseif (I_Prep_in.GE.5) then
        I_Prep_in = 0.5
        do ii = 1,7
          LAMDA(ii)= 10.0**(-A*CharBal(ii)**2.0  &    
              *(I_Prep_in**0.5/(1+I_Prep_in**0.5)-0.3*I_Prep_in))
        enddo
      else
        do ii = 1,7
          LAMDA(ii)= 10.0**(-A*CharBal(ii)**2.0  &    
              *(I_Prep_in**0.5/(1+I_Prep_in**0.5)-0.3*I_Prep_in))
        enddo
      endif
      
      return
      end
           
      



      ! CaSO4 ***********************************************************************************************
      !disp('**************************************************************')
      !disp('The reaction is: CaSO4(s)<--> Ca2+(aq) + SO42-(aq)')
      !disp('The Ksp(Solubility Product Constants) of CaSO4(s) is 4.93*10^-5')
      !disp('**************************************************************') 
      subroutine CaSO4

      use organic_mineral_mass_module
      use salt_data_module
      
      implicit none

      Double Precision M1,M2,M3,Ksp,Solv,Trial_Ksp,& 
          PosSolv,CalSul_Prep,Solid_CaSO4,Dissolved_Solid,& 
          Calcium_Conc,Sulfate_Conc
      
      M1 = Sol_CaSO4(c5)
      M2 = Cal_Conc(c11+1)
      M3 = Sul_Conc(salt_c4)
      Ksp = salt_K3
      
      Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
      Trial_Ksp = M2 * M3

      if (Trial_Ksp.gt.Ksp) then
        !disp('precipitation WILL form')
        ! Defining temporary parameter PosSolve
        PosSolv = abs(Solv)
        CalSul_Prep = PosSolv 
        Solid_CaSO4 = M1+CalSul_Prep
        Dissolved_Solid = 0
        Calcium_Conc = M2-CalSul_Prep
        Sulfate_Conc = M3-CalSul_Prep
      
      elseif (M1.gt.Solv) then
        !disp('Dissolution WILL form')
        !Defining temporary parameter PosSolv
        PosSolv = Solv
      
        !disp('****************************************')
        !disp('Portion of the solid will be dissolved')
        !disp('****************************************')
        Calcium_Conc = (M2+PosSolv)
        Sulfate_Conc = (M3+PosSolv)
        Dissolved_Solid = PosSolv
        Solid_CaSO4 = (M1-PosSolv)
      
      else 
        !disp('****************************************')
        !disp('Solid will be compeletly dissolved')
        !disp('****************************************')
        Calcium_Conc = (M1+M2)
        Sulfate_Conc = (M1+M3)
        Dissolved_Solid = M1
        Solid_CaSO4 = 0
      endif
      
      Sol_CaSO4(c5+1) = Solid_CaSO4
      Cal_Conc(c11+2) = Calcium_Conc
      Sul_Conc(salt_c4+1) = Sulfate_Conc

      return
      end





      ! MgCO3 ***********************************************************************************************
      !disp('**************************************************************')
      !disp('The reaction is: MgCO3(s)<--> Mg2+(aq) + CO32-(aq)')
      !disp('The Ksp(Solubility Product Constants) of MgCO3(s) is 4.7937*10^-6')
      !disp('**************************************************************') 
      subroutine MgCO3

      use organic_mineral_mass_module
      use salt_data_module

      implicit none

      Double Precision M1,M2,M3,Ksp,Solv,Trial_Ksp,& 
          PosSolv,MgCar_Prep,Solid_MgCO3,Dissolved_Solid,& 
          Mag_Conc,Carbonate_Conc
      
      M1 = Sol_MgCO3(c5)
      M2 = Mg_Conc(salt_c3)
      M3 = Car_Conc(c22+1)
      Ksp = salt_K2
      Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
      Trial_Ksp=M2*M3

      if (Trial_Ksp.GT.Ksp) then   
        !disp('precipitation WILL form')
        ! Defining temporary parameter PosSolve
        PosSolv = abs(Solv)
        MgCar_Prep = PosSolv 
        Solid_MgCO3 = M1+MgCar_Prep
        Dissolved_Solid = 0
        Mag_Conc = M2-MgCar_Prep
        Carbonate_Conc = M3-MgCar_Prep
      
      elseif(M1.GT.Solv) then   
        !disp('Dissolution WILL form')
        !Defining temporary parameter PosSolv
        PosSolv = Solv
        ! IF (M1.GT.PosSolv) THEN 
        !disp('****************************************')
        !disp('Portion of the solid will be dissolved')
        !disp('****************************************')
        Mag_Conc = (M2+PosSolv)
        Carbonate_Conc = (M3+PosSolv)
        Dissolved_Solid = PosSolv
        Solid_MgCO3 = (M1-PosSolv)
      
      else 
        !disp('****************************************')
        !disp('Solid will be compeletly dissolved')
        !disp('****************************************')
        Mag_Conc = (M1+M2)
        Carbonate_Conc = (M1+M3)
        Dissolved_Solid = M1
        Solid_MgCO3= 0
      
      endif

      Sol_MgCO3(c5+1) = Solid_MgCO3
      Mg_Conc(salt_c3+1) =   Mag_Conc
      Car_Conc(c22+2) =  Carbonate_Conc  
         
      return
      end





      ! NaCl ************************************************************************************************
      !disp('**************************************************************')
      !disp('The reaction is: NaCl(s)<--> Na+(aq) + Cl-(aq)')
      !disp('The Ksp(Solubility Product Constants) of NaCl(s) is 37.3')
      !disp('**************************************************************')  
      subroutine NaCl

      use organic_mineral_mass_module
      use salt_data_module

      implicit none

      Double Precision M1,M2,M3,Ksp,Solv,Trial_Ksp,& 
          PosSolv,SodiumChloride_Prep,Solid_NaCl,Dissolved_Solid,& 
          Sodium_Conc,Chloride_Conc
      
      M1 = Sol_NaCl(c5)
      M2 = Sod_Conc(c5)
      M3 = Cl_Conc(c5)
      Ksp = salt_K5
      
      Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
      Trial_Ksp=M2*M3

      if (Trial_Ksp.GT.Ksp) then    
        !disp('precipitation WILL form')
        ! Defining temporary parameter PosSolve
        PosSolv = abs(Solv)
        SodiumChloride_Prep = PosSolv 
        Solid_NaCl = M1+SodiumChloride_Prep
        Dissolved_Solid = 0
        Sodium_Conc = M2-SodiumChloride_Prep
        Chloride_Conc  = M3-SodiumChloride_Prep
      
      elseif(M1.GT.Solv) then
        !disp('Dissolution WILL form')
        !Defining temporary parameter PosSolv
        PosSolv = Solv
        !  IF (M1.GT.PosSolv) THEN 
        !disp('****************************************')
        !disp('Portion of the solid will be dissolved')
        !disp('****************************************')
        Sodium_Conc = (M2+PoSSolv)
        Chloride_Conc  = (M3+PosSolv)
        Dissolved_Solid = PosSolv
        Solid_NaCl = (M1-PosSolv)
      
      else
        !disp('****************************************')
        !disp('Solid will be compeletly dissolved')
        !disp('****************************************')
        Sodium_Conc = (M1+M2)
        Chloride_Conc  = (M1+M3)
        Dissolved_Solid = M1
        Solid_NaCl= 0
      
      endif

      Sol_NaCl(c5+1) = Solid_NaCl
      Sod_Conc(c5+1) = Sodium_Conc
      Cl_Conc(c5+1) = Chloride_Conc
      
      return
      end





      ! MgSO4 ***********************************************************************************************
      !disp('**************************************************************')
      !disp('The reaction is: MgSO4(s)<--> Mg2+(aq) + SO42-(aq)')
      !disp('The Ksp(Solubility Product Constants) of MgSO4(s) is ?')
       !disp('**************************************************************')    
      subroutine MgSO4

      use organic_mineral_mass_module
      use salt_data_module

      implicit none

      Double Precision M1,M2,M3,Ksp,Solv,Trial_Ksp,& 
          PosSolv,MgSul_Prep,Solid_MgSO4,Dissolved_Solid,& 
          Mag_Conc,Sulfate_Conc
      
      M1 = Sol_MgSO4(c5)
      M2 = Mg_Conc(salt_c3+1)
      M3 = Sul_Conc(salt_c4+1)
      Ksp = salt_K4
      
      Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
      Trial_Ksp=M2*M3

      if (Trial_Ksp.GT.Ksp) then  
        !disp('precipitation WILL form')
        ! Defining temporary parameter PosSolve
        PosSolv = abs(Solv)
        MgSul_Prep = PosSolv 
        Solid_MgSO4 = M1+MgSul_Prep
        Dissolved_Solid = 0
        Mag_Conc = M2-MgSul_Prep
        Sulfate_Conc = M3-MgSul_Prep

      elseif(M1.GT.Solv) then
        !disp('Dissolution WILL form')
        !Defining temporary parameter PosSolv
        PosSolv = Solv
        !IF (M1.GT.PosSolv) THEN 
        !disp('****************************************')
        !disp('Portion of the solid will be dissolved')
        !disp('****************************************')
        Mag_Conc = (M2+PosSolv)
        Sulfate_Conc = (M3+PosSolv)
        Dissolved_Solid = PosSolv
        Solid_MgSO4 = (M1-PosSolv)

      else
        !disp('****************************************')
        !disp('Solid will be compeletly dissolved')
        !disp('****************************************')
        Mag_Conc = (M1+M2)
        Sulfate_Conc = (M1+M3)
        Dissolved_Solid = M1
        Solid_MgSO4= 0
      
      endif
        
      Sol_MgSO4(c5+1) = Solid_MgSO4
      Mg_Conc(salt_c3+2) = Mag_Conc
      Sul_Conc(salt_c4+2) = Sulfate_Conc
      
      return
      end





      ! CaCO3 ***********************************************************************************************
      !disp('**************************************************************')
      !disp('The reaction is: CaCO3(s)<--> Ca2+(aq) + CO32-(aq)')
      !disp('The Ksp(Solubility Product Constants) of CaCO3(s) is 3.0702*10^-9')
      !disp('**************************************************************')     
      subroutine CaCO3
      
      use organic_mineral_mass_module
      use salt_data_module
       
      Double Precision M1,M2,M3,Ksp,Solv,Trial_Ksp,& 
          PosSolv,CalCar_Prep,Solid_CaCO3,Dissolved_Solid,& 
          Calcium_Conc,Carbonate_Conc
            
      M1 = Sol_CaCO3(c5)
      M2 = Cal_Conc(c11)
      M3 = Car_Conc(c22)
      Ksp = salt_K1
      Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
      Trial_Ksp=M2*M3

      if (Trial_Ksp.GT.Ksp) then
        !disp('precipitation WILL form')
        ! Defining temporary parameter PosSolve
        PosSolv = abs(Solv)
        CalCar_Prep = PosSolv 
        Solid_CaCO3 = M1+CalCar_Prep
        Dissolved_Solid = 0
        Calcium_Conc = M2-CalCar_Prep
        Carbonate_Conc = M3-CalCar_Prep

      elseif(M1.GT.Solv) then
        !disp('Dissolution WILL form')
        !Defining temporary parameter PosSolv
        PosSolv = Solv
        !IF (M1.GT.PosSolv) THEN 
        !disp('****************************************')
        !disp('Portion of the solid will be dissolved')
        !disp('****************************************')
        Calcium_Conc = (M2+Solv)
        Carbonate_Conc = (M3+Solv)
        Dissolved_Solid = Solv
        Solid_CaCO3 = (M1-Solv)

      else
        !disp('****************************************')
        !disp('Solid will be compeletly dissolved')
        !disp('****************************************')
        Calcium_Conc = (M1+M2)
        Carbonate_Conc = (M1+M3)
        Dissolved_Solid = M1
        Solid_CaCO3= 0

      endif

      Sol_CaCO3(c5+1) = Solid_CaCO3 
      Cal_Conc(c11+1) =  Calcium_Conc
      Car_Conc(c22+1) = Carbonate_Conc 
      
      return
      end





      ! Calculate Cation Exchange ***************************************************************************   
      ! Developed by Saman Tavakoli 
      ! At Colorado State University 
      ! October 2016
      ! Includes Ca2+, Mg2+, Na+,K+
      ! CEC = Cation Exchange Capacity in meq/100g soil
      !Assumption for CEC: constant for a given soil, independent of of pH, ion type and concentration
      !Sel_K1 through Sel_K6 stands for selectivity coefficient of exchange reaction
      !XCAINI and others stands for inital ion which attached to the soil particle
      subroutine cationexchange
      
      use organic_mineral_mass_module
      use salt_data_module

      implicit none 
      real CEC,Sel_K1,Sel_K2,Sel_K3,Sel_K4,Sel_K5,Sel_K6,&     
          XCAINI,XMGINI,XNAINI,XKINI,&     
          DeltaX_Ca,DeltaX_Mg,DeltaX_Na,DeltaX_K,&     
          Con_Ca,Con_Mg,Con_Na,Con_K,&     
          X_Ca,X_Mg,X_Na,X_K

      !CEC selected based on soil type; for simplicity, for now used one value based on the sandy-loam soil type
      CEC = 15 ! meq/100g soil 
      
      !Convert the ions concentration in solution from ppm to mmol/liter 
      Con_Ca = upion2/40  ! mmol/liter water 
      Con_Mg = upion3/24  ! mmol/liter water 
      Con_Na = upion4/23  ! mmol/liter water 
      Con_K  = upion5/39  ! mmol/liter water 

      if(Con_Ca.gt.0 .and. Con_Mg.gt.0 .and. &   
          Con_Na.gt.0 .and. Con_K.gt.0) then
        
      !using Gapon Convention 
      Sel_K1 = 0.7
      Sel_K2 = 6
      Sel_K3 = 0.4
      Sel_K4 = 0.2
      Sel_K5 = 4
      Sel_K6 = 16

      !Make sure an appropriate value to include
      XCAINI = 13.6  ! meg/100g soil
      XMGINI = 0.17  !meq/100g soil
      XNAINI = 0.25 !meq/100g soil
      XKINI = 0.18 !meq/100g soil
      X_Ca = CEC/(1+((Sel_K1*(Con_Mg)**0.5)/((Con_Ca)**0.5))+&       
          ((Con_Na)/(Sel_K2*(Con_Ca)**0.5))+&       
          ((Con_K)/(Sel_K3*(Con_Ca)**0.5)))
      X_Mg = CEC/(((Con_Ca)**0.5/(Sel_K1*(Con_Mg)**0.5))+1+&       
          ((Con_Na)/(Sel_K4*(Con_Mg)**0.5))+&       
          ((Con_K)/(Sel_K5*(Con_Mg)**0.5)))
      X_Na = CEC/(((Sel_K2*(Con_Ca)**0.5)/(Con_Na))+&
          ((Sel_K5*(Con_Mg)**0.5)/(Con_Na))+1+((Sel_K6*Con_K)/(Con_Na)))
      X_K = CEC/(((Sel_K3*(Con_Ca)**0.5)/(Con_K))+&
          ((Sel_K4*(Con_Mg)**0.5)/(Con_K))+(Con_Na/(Sel_K6*Con_K))+1)
      
      !Post processing the solution concentration
      
!C     1 meq Ca = 20 mg 
!C     1 meq Na = 23 mg 
!C     1 meq K = 39 mg
      
!C ppm = mg/kg 
!C mg/ 100g soil * 10 = mg/kg 
!C (X meq Ca/100g soil) * (20 mg/mequ)* 10 = 200X mg/kg or ppm
!C (X meq Mg/100g soil) * (12 mg/mequ)* 10 = 120X mg/kg or ppm
!C (X meq Na/100g soil) * (23 mg/mequ)* 10 = 230X mg/kg or ppm
!C (X meq K/100g soil) * (39 mg/mequ)* 10 = 390X mg/kg or ppm
      
      
!C Need Soil Bulk Density, assume : 1650 kg/m3
!C Need water content from RT3D, for simplicity right now, water content = 0.3
      
      DeltaX_Ca = X_Ca - XCAINI
      DeltaX_Mg = X_Mg - XMGINI
      DeltaX_Na = X_Na - XNAINI
      DeltaX_K = X_K - XKINI
      
      upion2 = Con_Ca*40 - DeltaX_Ca*200*1.65/0.3 ! 0.3 = water content 
      upion3 = Con_Mg*24 - DeltaX_Mg*120*1.65/0.3
      upion4 = Con_Na*23 - DeltaX_Na*230*1.65/0.3
      upion5 = Con_K*39 - DeltaX_K*390*1.65/0.3

      if (Con_Ca.LE.0) then
          upion2 = -10
      endif 
      if (Con_Mg.LE.0) then
          upion3 = -10
      endif
      if (Con_Na.LE.0) then
          upion4 = -10
      endif
      if (Con_K.LE.0) then
          upion5 = -10
      endif
      
      endif

      return 
      end ! end subroutine cation exchange 
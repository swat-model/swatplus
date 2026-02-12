      subroutine gwflow_minl(cell_id,gw_vol) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates chemical reactions in gwflow cells
      
      use gwflow_module
      use hydrograph_module
      use constituent_mass_module, only : cs_db
      use salt_data_module
      
      implicit none

      integer, intent (in) :: cell_id		                  !       |id of cell passed in
      real, intent (in) :: gw_vol		                      !m3     |volume of groundwater in the cell
      integer ::  isalt = 0                                   !       |salt ion counter
      integer ::  sol_index = 0                               !       |solute counter
      integer ::  iter_count = 0                              !       |iteration counter for precipitation-dissolution
      real :: mass_before(8) = 0.           
      real :: mass_after(8) = 0.                              !g      |salt mass before and after precipitation-dissolution 
      real :: wc = 0.                                         !-      |water content in cell
      real :: ion1 = 0.
      real :: ion2 = 0.
      real :: ion3 = 0.
      real :: ion4 = 0.
      real :: ion5 = 0.
      real :: ion6 = 0.
      real :: ion7 = 0.
      real :: ion8 = 0.
      real :: Sol_CaCO3_p(1000) = 0.
      real :: Sol_MgCO3_p(1000) = 0.
      real :: Sol_CaSO4_p(1000) = 0.
      real :: Sol_MgSO4_p(1000) = 0.
      real :: Sol_NaCl_p(1000) = 0.
      real :: I_Prep_in = 0.
      real :: I_diff = 0.
      real :: SkipedIEX = 0.
      double precision :: IonStr = 0.
      double precision :: IS_temp = 0.
      double precision :: K_ADJ1 = 0.
      double precision :: K_ADJ2 = 0.
      double precision :: K_ADJ3 = 0.
      double precision :: K_ADJ4 = 0.
      double precision :: K_ADJ5 = 0.
      double precision :: error1ST = 0.
      double precision :: error2ND = 0.
      double precision :: error3RD = 0.
      double precision :: errorTotal = 0.
      
      
      !track salt ion mass
      mass_before = 0.
      mass_after = 0.
      
      !store beginning salt mass (g) in the cell
      sol_index = 2
      do isalt=1,cs_db%num_salts
        sol_index = sol_index + 1 
        mass_before(isalt) = gwsol_state(cell_id)%solute(sol_index)%conc * gw_vol !g
      enddo
      
      !saturated water content (for saturated zone of aquifer) = specific yield
      wc = gw_state(cell_id)%spyd
      
      !retrieve the solid salt mineral concentration in the aquifer, and perform conversions
      Sol_CaCO3_p(1) = gwsol_minl_state(cell_id)%fract(1)
      Sol_MgCO3_p(1) = gwsol_minl_state(cell_id)%fract(2)
      Sol_CaSO4_p(1) = gwsol_minl_state(cell_id)%fract(3)
      Sol_MgSO4_p(1) = gwsol_minl_state(cell_id)%fract(4)
      Sol_NaCl_p(1)  = gwsol_minl_state(cell_id)%fract(5)
      Sol_CaCO3(1) = (Sol_CaCO3_p(1)/100) * (1.855/(wc*100.0))*1000.0 
      Sol_MgCO3(1) = (Sol_MgCO3_p(1)/100) * (1.855/(wc*84.31))*1000.0 
      Sol_CaSO4(1) = (Sol_CaSO4_p(1)/100) * (1.855/(wc*136.14))*1000.0 
      Sol_MgSO4(1) = (Sol_MgSO4_p(1)/100) * (1.855/(wc*120.36))*1000.0 
      Sol_NaCl(1)  = (Sol_NaCl_p(1)/100) * (1.855/(wc*58.44))*1000.0 
      
      !retrieve the current salt ion concentration (g/m3) and convert to mol/L
      ion1 = gwsol_state(cell_id)%solute(3)%conc !sulfate
      ion2 = gwsol_state(cell_id)%solute(4)%conc !calcium
      ion3 = gwsol_state(cell_id)%solute(5)%conc !magnesium
      ion4 = gwsol_state(cell_id)%solute(6)%conc !sodium
      ion5 = gwsol_state(cell_id)%solute(7)%conc !potassium
      ion6 = gwsol_state(cell_id)%solute(8)%conc !chloride
      ion7 = gwsol_state(cell_id)%solute(9)%conc !carbonate
      ion8 = gwsol_state(cell_id)%solute(10)%conc !bicarbonate
      Sul_Conc(1) = ion1*((1.0/1000)*(1.0/96.06)) !sulfate
      Cal_Conc(1) = ion2*((1.0/1000)*(1.0/40.078)) !calcium
      Mg_Conc(1) = ion3*((1.0/1000)*(1.0/24.305)) !magnesium
      Sod_Conc(1) = ion4*((1.0/1000)*(1.0/23.0)) !sodium
      Pot_Conc(1) = ion5*((1.0/1000)*(1.0/39.0)) !potassium
      Cl_Conc(1) = ion6*((1.0/1000)*(1.0/35.45)) !chloride
      Car_Conc(1) = ion7*((1.0/1000)*(1.0/60.01)) !carbonate
      BiCar_Conc(1) = ion8*((1.0/1000)*(1.0/61.01)) !bicarbonate
      
      !define the activity coefficient using Extended Debye-Huckel equation
      call salt_ionic_strength(IS_temp,Cal_Conc(1),Sul_Conc(1),Car_Conc(1),BiCar_Conc(1),Mg_Conc(1),Sod_Conc(1),Pot_Conc(1))
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
        
        call salt_act_coeff(I_Prep_in)

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
        
          call salt_minl_caco3
          call salt_minl_mgco3
          call salt_minl_caso4
          call salt_minl_mgso4
          call salt_minl_nacl

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

      !new concentration after precipitation
      upion1 = Sul_Conc(salt_c4)*(96.06*1000.0)
      upion2 = Cal_Conc(c11)*(40.078*1000.0)
      upion3 = Mg_Conc(salt_c3)*(24.305*1000.0)
      upion4 = Sod_Conc(c5)*(23.0*1000.0)
      upion5 = Pot_Conc(1)*(39.0*1000.0)
      upion6 = Cl_Conc(c5)*(35.45*1000.0)
      upion7 = Car_Conc(c22)*(60.01*1000.0)
      upion8 = BiCar_Conc(1)*(61.01*1000.0) 

      !calculate the salt ion mass after precipitation-dissolution
      mass_after(1) = upion1 * gw_vol !g so4
      mass_after(2) = upion2 * gw_vol !g ca
      mass_after(3) = upion3 * gw_vol !g mg
      mass_after(4) = upion4 * gw_vol !g na
      mass_after(5) = upion5 * gw_vol !g k
      mass_after(6) = upion6 * gw_vol !g cl
      mass_after(7) = upion7 * gw_vol !g co3
      mass_after(8) = upion8 * gw_vol !g hco3
      
      !store the solute mass precipitated or dissolved 
      sol_index = 2
      do isalt=1,cs_db%num_salts
        sol_index = sol_index + 1
        mass_min(sol_index) = mass_after(isalt) - mass_before(isalt) !positive if dissolution occurs (more mass after)  
      enddo
      
      !convert solids concentrations to solid percentage
      Sol_CaCO3_p(c5) = (Sol_CaCO3(c5)*100.0)/((1.855/(wc*100.0))*1000.0) 
      Sol_MgCO3_p(c5) = (Sol_MgCO3(c5)*100.0)/((1.855/(wc*84.31))*1000.0) 
      Sol_CaSO4_p(c5) = (Sol_CaSO4(c5)*100.0)/((1.855/(wc*136.14))*1000.0) 
      Sol_MgSO4_p(c5) = (Sol_MgSO4(c5)*100.0)/((1.855/(wc*120.36))*1000.0) 
      Sol_NaCl_p(c5)  = (Sol_NaCl(c5)*100.0)/((1.855/(wc*58.44))*1000.0) 
           
      !save solids concentration for the grid cell
      gwsol_minl_state(cell_id)%fract(1) = Sol_CaCO3_p(c5)
      gwsol_minl_state(cell_id)%fract(2) = Sol_MgCO3_p(c5)
      gwsol_minl_state(cell_id)%fract(3) = Sol_CaSO4_p(c5)
      gwsol_minl_state(cell_id)%fract(4) = Sol_MgSO4_p(c5)
      gwsol_minl_state(cell_id)%fract(5) = Sol_NaCl_p(c5)      
      
      
      end subroutine gwflow_minl
      
           
      
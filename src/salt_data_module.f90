      module salt_data_module
    
      implicit none
    
      integer :: salt_tol_sim                                             !      |flag to simulate salt effect on plant growth
      integer :: salt_soil_type                                           !      |soil type (1 = CaSO4 soils; 2 = NaCl soils)
      integer :: salt_effect                                              !      |1 = applied after other stresses; 2 = included with other stresses (min)
      real :: salt_tds_ec                                                 !      |total dissolved solids to electrical conductivity conversion factor
      real, dimension (:), allocatable :: salt_stress_a,salt_stress_b     !      |a and b parameters in salinity relative yield equations
      
      double precision :: Sul_Conc(2000),Cal_Conc(2000),Mg_Conc(2000)
      double precision :: Sod_Conc(2000),Pot_Conc(2000),Cl_Conc(2000)
      double precision :: Car_Conc(2000),BiCar_Conc(2000)
      integer ::          c11,c22,salt_c3,salt_c4,c5
      double precision :: salt_K1,salt_K2,salt_K3,salt_K4,salt_K5
      !solubility products for salt minerals in the soil profile
      double precision :: Ksp11 = 0.0000000030702
      double precision :: Ksp21 = 0.0000047937
      double precision :: Ksp31 = 0.00007888
      double precision :: Ksp41 = 0.007244
      double precision :: Ksp51 = 37.3
      !solubility products for salt minerals in the aquifer
      double precision :: Ksp12 = 0.0000000030702
      double precision :: Ksp22 = 0.0000047937
      double precision :: Ksp32 = 0.00007888
      double precision :: Ksp42 = 0.007244
      double precision :: Ksp52 = 37.3
      double precision :: upion1,upion2,upion3,upion4,upion5,upion6,upion7,upion8
      double precision :: Sol_CaCO3(1000),Sol_MgCO3(1000),Sol_CaSO4(1000),Sol_MgSO4(1000),Sol_NaCl(1000)
      double precision :: LAMDA(7)
      !generic array to hold salt ion concentrations
      real :: soil_salt_conc(8) !generic array to hold salt ion concentrations
      
      contains

      end module salt_data_module 
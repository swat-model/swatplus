      module salt_data_module
    
      implicit none
    
      real :: salt_tds_ec           !!              |total dissolved solids to electrical conductivity conversion factor
      
      type salt_solubility_product_parms
        character(len=16) :: salt_solid_name         
        real :: CaCO3       !! 1/day         |solubility product parms for calcium carbonate
        real :: MgCO3       !! 1/day         |solubility product parms for magnesium carbonate
        real :: CaSO4       !! 1/day         |solubility product parms for calcium sulfate
        real :: MgSO4       !! 1/day         |solubility product parms for magnesium sulfate
        real :: NaCl        !! none          |solubility product parms for sodium chloride
      end type salt_solubility_product_parms
      type (salt_solubility_product_parms), dimension(:), allocatable  :: ksp

      real :: CaCO3_p(1000)
      real :: MgCO3_p(1000)
      real :: CaSO4_p(1000)
      real :: MgSO4_p(1000)
      real :: NaCl_p(1000)
      
      contains

      end module salt_data_module 
      subroutine cn2_init (j)

      use hru_module, only : cn2, hru
      use soil_module
      use maximum_data_module
      use landuse_data_module
      
      implicit none
  
      integer, intent (in)  :: j
      integer :: icn                 !none       |counter 
      integer :: isol                !none       |counter 
      integer :: ilum                !none       |counter 
      
      !!assign cn2
        ilum = hru(j)%land_use_mgt
        isol = hru(j)%dbs%soil
        !! set initial curve number parameters
        icn = lum_str(ilum)%cn_lu
        select case (sol(isol)%s%hydgrp)
        case ("A")
          cn2(j) = cn(icn)%cn(1)
        case ("B")
          cn2(j) = cn(icn)%cn(2)
        case ("C")
          cn2(j) = cn(icn)%cn(3)
        case ("D")
          cn2(j) = cn(icn)%cn(4)
        end select
        call curno(cn2(j), j)
      
      return
      end subroutine cn2_init
      subroutine wind_ero_veg
    
      implicit none

      !sc_fac = pldb(idp)%wind_stl * hru(j)%stl + pldb(idp)%wind_std * hru(j)%std + pldb(idp)%wind_flat * soil1(j)%rsd(1)%m
      
      ! wind_factors%veg = 1. - sc_fac / (sc_fac + exp(-0.331 - 1.055 * sc_fac)
      
      return
      end subroutine wind_ero_veg
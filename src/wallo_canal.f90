      subroutine wallo_canal (iwallo, itrn, ican)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (in):: iwallo     !water allocation object number
      integer, intent (in) :: ican      !water transfer object number
      integer, intent (in) :: itrn      !water treatment plant object number
      
      !! compute outflow from canal using decision table or simple lag
      if (canal(ican)%dtbl == "null") then
        !! simple drawdown days
          wallod_out(iwallo)%trn(itrn)%trn_flo = canal_om_stor(ican)%flo / canal(ican)%ddown_days
      else
        !! decision table to condition outflow from canal
      end if
      
      !! outflow is the fraction of the withdrawal from the canal
      canal_om_out(ican) = (wallod_out(iwallo)%trn(itrn)%trn_flo / canal_om_stor(ican)%flo) *    &
                                                                            canal_om_stor(ican)
      
      !! subtract amount that is removed
      canal_om_stor(ican) = canal_om_stor(ican) - canal_om_out(ican)
      
      !! organic hydrograph being transfered from the source to the receiving object
      outflo_om = (1. - canal(ican)%loss_fr) * canal_om_out(ican)
      
      !! add loss (outflo_om) to aquifers
      
    return
    end subroutine wallo_canal
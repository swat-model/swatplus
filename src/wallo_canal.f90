      subroutine wallo_canal (ican)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Routes water through a wallo canal: computes outflow, applies loss,
!!    and distributes canal seepage to aquifer (gwflow grid cells or 1-D aquifer).

      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use aquifer_module
      
      implicit none 

      integer, intent (in) :: ican      !water transfer object number
      integer :: ipou                   !place of use number
      integer :: ipor                   !point of return number
      integer :: iaq                    !aquifer number
      real :: seep_tot = 0.0            !toal seepage loss from canal
      real :: fr_aqu = 0.0              !fraction of seepage loss to each aquifer
      real :: fr_aqu_tot = 0.0          !total seepage loss
      
      !! check if water is available in the canal
      if (canal_om_stor(ican)%flo < 1.e-6) return
      
      !! if channel is a POD, allocate water for users and adjust flow in channel accordingly
      if (canal(ican)%wallo_pod > 0) then
        call wallo_control (canal(ican)%wallo_pod)
      end if
      
      !! subtract evaporative losses
      canal_om_stor(ican)%flo = canal(ican)%evap_co * canal_om_stor(ican)%flo
      
      !! add seepage loss to aquifers
      !! total seepage from the canal - m3 = mm/d * m * km
      seep_tot = canal(ican)%sat_con * canal(ican)%d * canal(ican)%l
      !! loop through all aquifers underlying the canal and add seepage to each
      fr_aqu_tot = 0.
      do iaq = 1, canal(ican)%num_aqu
        !! fraction of losses in each aquifer
        fr_aqu = (seep_tot * canal(ican)%aqu_loss_fr(iaq)) / canal_om_stor(ican)%flo
        fr_aqu_tot = fr_aqu_tot + fr_aqu
        !! add seepage, no3, and solp to aquifer
        aqu_d(iaq)%stor = aqu_d(iaq)%stor + fr_aqu * canal_om_stor(ican)%flo
        aqu_d(iaq)%no3_st = aqu_d(iaq)%no3_st + fr_aqu * canal_om_stor(ican)%no3
        aqu_d(iaq)%minp = aqu_d(iaq)%minp + fr_aqu * canal_om_stor(ican)%solp
        
        !! add constituents to aquifer
        cs_aqu(iaq) = cs_aqu(iaq) + fr_aqu * canal_cs_stor(ican)
      end do
      
      !! subtract seepage and from canal storage
      canal_om_stor(ican) = (1.0 - fr_aqu_tot) * canal_om_stor(ican)

      !! compute outflow from canal using decision table or simple lag
      if (canal(ican)%dtbl == "null") then
        !! simple drawdown days
        canal_om_out(ican)%flo = canal_om_stor(ican)%flo / canal(ican)%ddown_days
      else
        !! decision table to condition outflow from canal
      end if

      !! outflow is the fraction of the withdrawal from the canal
      canal_om_out(ican) = (canal_om_out(ican)%flo / canal_om_stor(ican)%flo) *    &
                                                                            canal_om_stor(ican)

      poud_om(ipou)%pors = canal_om_out(ican)
      !! set flow to each POR
      do ipor = 1, pou(ipou)%pors
        poud_om(ipou)%por(ipor) = pou(ipou)%por(ipor)%frac * poud_om(ipou)%pors
      end do
      
      !! subtract amount that is removed
      canal_om_stor(ican) = canal_om_stor(ican) - canal_om_out(ican)
      
    return
    end subroutine wallo_canal

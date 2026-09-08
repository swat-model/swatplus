      subroutine wallo_withdraw (ipod, ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use aquifer_module
      use reservoir_module
      use time_module
      use recall_module
      
      implicit none 

      integer, intent (in):: ipod         !point of diversion number
      integer, intent (in) :: ipou        !place of use number
      integer :: pod_num                  !point of diversion number for each place of use number (ipou)
      integer :: pou_num                  !place of use number for each point of diversion number (ipou)
      integer :: j = 0              !none       |source (delivery) object number
      real :: res_min = 0.          !m3         |min reservoir volume for withdrawal
      real :: can_min = 0.          !m3         |min canal volume for withdrawal
      real :: cha_min = 0.          !m3         |minimum allowable flow in channel for withdrawal
      real :: wtow_min = 0.         !m3         |minimum allowable storage in water tower for withdrawal
<<<<<<< Updated upstream
      real :: rto = 0.              !none       |ratio of water withdrawn to available water in the POD
        
      !! POD (source) object number from POD object
      j = pod(ipod)%typ_num
      
      !! POD number from delivery object, pou number from loop in wallo_control
      pod_num = pod(ipod)%pou(ipou)%pod_num
      pou_num = pod(ipod)%pou(ipou)%num
      
      !! check minimum storage/flow limits and withdraw water from each POD
      select case (pod(ipod)%typ)
      
      !! outside the basin source - daily, monthly, or yearly flow from recall object
      case ("osrc")
        poud_om(pou_num)%pod(ipod) = pou(pou_num)%pod(pod_num)%frac * osrc_om(j)
        osrc_om(j) = (1. - pou(pou_num)%pod(pod_num)%frac) * osrc_om(j)
        
      !! water tower storage
=======
      real :: rto = 0.              !none       |ratio of water withdrawn to available water in the PODl
      real :: wdraw_max = 0.        !m3         |maximum water that can be withdrawn
        
      !! POD (source) object number from POD object
      j = pod(ipod)%typ_num
      
      !! set the duty or compensation for the POD - if compensation, then duty is umet duty for the POU
      if (wallo_comp == "y") then
        wdraw_max = pou(ipou)%pod(ipod)%duty - poud_met(ipou)%pod(ipod)%deliv
      else
        wdraw_max = pou(ipou)%pod(ipod)%duty
      end if
      
      !! zero out the withdrawal for the POD
      outflo_om = hz
      
      !! POD number from delivery object, pou number from loop in wallo_control
      pod_num = pod(ipod)%pou(ipou)%pod_num
      pou_num = pod(ipod)%pou(ipou)%num
      
      !! check minimum storage/flow limits and withdraw water from each POD
      select case (pod(ipod)%typ)
      
      !! unlimited source - unlimited water, no constituents, no minimum storage/flow limits
      case ("unl")
        outflo_om = hz
        outflo_om%flo = pou(pou_num)%pod(pod_num)%frac * wdraw_max
        
      !! outside the basin source
      case ("osrc")
        !! check if withdrawal takes storage below the minimum
        if (osrc_om(j)%flo > 0.) then
          outflo_om%flo = pou(pou_num)%pod(pod_num)%frac * osrc_om(j)%flo
          !! convert concentration to mass
          call hyd_convert_conc_to_mass (outflo_om)
          osrc(j)%wdraw = osrc(j)%wdraw + (1. - pou(pou_num)%pod(pod_num)%frac) * osrc_om(j)%flo
        end if
      
>>>>>>> Stashed changes
      case ("wtow")
        wtow_min = pou(pou_num)%pod(pod_num)%const_min * wtow_om_stor(j)%flo
        !! check if withdrawal takes storage below the minimum
        if (wtow_om_stor(j)%flo > wtow_min) then
<<<<<<< Updated upstream
          rto = pou(pou_num)%pod(pod_num)%duty / wtow_om_stor(j)%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          poud_om(pou_num)%pod(pod_num) = rto * wtow_om_stor(j)
=======
          rto = wdraw_max / wtow_om_stor(j)%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          outflo_om = rto * wtow_om_stor(j)
>>>>>>> Stashed changes
          wtow_om_stor(j) = (1. - rto) * wtow_om_stor(j)
        end if
         
      !! divert flowing water from channel source
      case ("cha")
<<<<<<< Updated upstream
        cha_min = pou(pou_num)%pod(pod_num)%const_min  !m3 = m3/s * 86400s/d
        !! don't divert when flow is below the minimum - cha_min
        if (ht2%flo > cha_min) then
          rto = pou(pou_num)%pod(pod_num)%duty / ht2%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          poud_om(pou_num)%pod(pod_num) = rto * ht2
          ht2%flo = (1. - rto) * ht2%flo
=======
        !! cant compensate for channel flow
        if (wallo_comp == "n") then
          cha_min = pou(pou_num)%pod(pod_num)%const_min  !m3 = m3/s * 86400s/d
          !! don't divert when flow is below the minimum - cha_min
          if (ht2%flo > cha_min) then
            rto = wdraw_max / ht2%flo
            rto = Min(1., rto)
            rto = Max(0., rto)
            outflo_om = rto * ht2
            ht2%flo = (1. - rto) * ht2%flo
          end if
>>>>>>> Stashed changes
        end if
          
      !! canal source
      case ("can")
        can_min = pou(pou_num)%pod(pod_num)%const_min * canal_om_stor(j)%flo
        !! check if withdrawal takes storage below the minimum
        if (canal_om_stor(j)%flo >= can_min) then
<<<<<<< Updated upstream
          rto = pou(pou_num)%pod(pod_num)%duty / canal_om_stor(j)%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          poud_om(pou_num)%pod(pod_num) = rto * canal_om_stor(j)
=======
          rto = wdraw_max / canal_om_stor(j)%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          outflo_om = rto * canal_om_stor(j)
>>>>>>> Stashed changes
          canal_om_stor(j) = (1. - rto) * canal_om_stor(j)
        end if
         
      !! reservoir source
      case ("res")
        res_min = pou(pou_num)%pod(pod_num)%const_min * res_ob(j)%pvol
        !! check if withdrawal takes storage below the minimum
        if (res(j)%flo > res_min) then
<<<<<<< Updated upstream
          rto = pou(pou_num)%pod(pod_num)%duty / res(j)%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          poud_om(pou_num)%pod(pod_num) = rto * res(j)
=======
          rto = wdraw_max / res(j)%flo
          rto = Min(1., rto)
          rto = Max(0., rto)
          outflo_om = rto * res(j)
>>>>>>> Stashed changes
          res(j) = (1. - rto) * res(j)
        end if
         
      !! aquifer source
      case ("aqu") 
        if (aqu_d(j)%dep_wt < pou(pou_num)%pod(pod_num)%const_min) then
<<<<<<< Updated upstream
          poud_om(pou_num)%pod(pod_num) = hz
          !! only have flow, no3, and minp(solp) for aquifer
          rto =  (pou(pou_num)%pod(pod_num)%duty / (10. * aqu_prm(j)%area_ha)) / aqu_d(j)%stor     !mm = m3/(10.*ha)
=======
          !! only have flow, no3, and minp(solp) for aquifer
          rto =  (wdraw_max / (10. * aqu_prm(j)%area_ha)) / aqu_d(j)%stor     !mm = m3/(10.*ha)
>>>>>>> Stashed changes
          rto = Min(1., rto)
          rto = Max(0., rto)
          aqu_d(j)%stor = (1. - rto) * aqu_d(j)%stor
          aqu_d(j)%no3_st = (1. - rto) * aqu_d(j)%no3_st
          aqu_d(j)%minp = (1. - rto) * aqu_d(j)%minp
<<<<<<< Updated upstream
          poud_om(pou_num)%pod(pod_num)%flo = rto * aqu_d(j)%stor * 10. * aqu_prm(j)%area_ha
          poud_om(pou_num)%pod(pod_num)%no3 = rto * aqu_d(j)%no3_st * aqu_prm(j)%area_ha
          poud_om(pou_num)%pod(pod_num)%solp = rto * aqu_d(j)%minp * aqu_prm(j)%area_ha
=======
          outflo_om%flo = rto * aqu_d(j)%stor * 10. * aqu_prm(j)%area_ha
          outflo_om%no3 = rto * aqu_d(j)%no3_st * aqu_prm(j)%area_ha
          outflo_om%solp = rto * aqu_d(j)%minp * aqu_prm(j)%area_ha
>>>>>>> Stashed changes
        end if
        
      !! gwflow source
      case ("gwf") 
        
      end select
      
<<<<<<< Updated upstream
      !! reset annual maximum withdrawals if decision table isn't used
      pou(pou_num)%pod(pod_num)%wdraw_cur = pou(pou_num)%pod(pod_num)%wdraw_cur +            &
                                                       poud_om(pou_num)%pod(pod_num)%flo
            
      !! POD is finished - sum total withdrawal for the POU
      poud_om(pou_num)%pods = poud_om(pou_num)%pods + poud_om(pou_num)%pod(pod_num)
      pou(pou_num)%pod(pod_num)%fin = "y"
      
      !! add to total flow delivered and om withdrawal for the POU
      poud_met(pou_num)%pod(pod_num)%deliv = pou(pou_num)%pod(pod_num)%deliv + poud_om(pou_num)%pod(pod_num)%flo
      poud_met(pou_num)%duty_tot%deliv = poud_met(pou_num)%duty_tot%deliv + poud_om(pou_num)%pod(pod_num)%flo
      poud_om(pou_num)%pors = poud_om(pou_num)%pors + poud_om(pou_num)%pod(pod_num)
      
      !! add constituents withdrawn to total withdrawal for the POU
      
    return
    end subroutine wallo_withdraw
=======
      !! add to total flow delivered and om withdrawal for the POD
      poud_om(pou_num)%pod(pod_num) = poud_om(pou_num)%pod(pod_num) + outflo_om
      
      !! reset annual maximum withdrawals if decision table isn't used
      pou(pou_num)%pod(pod_num)%wdraw_cur = pou(pou_num)%pod(pod_num)%wdraw_cur + outflo_om%flo
            
      !! POD is finished - sum total withdrawal for the POU
      poud_om(pou_num)%pods = poud_om(pou_num)%pods + outflo_om
      pou(pou_num)%pod(pod_num)%fin = "y"
      
      !! add to total flow delivered and om withdrawal for the POU
      poud_met(pou_num)%pod(pod_num)%deliv = pou(pou_num)%pod(pod_num)%deliv + outflo_om%flo
      poud_met(pou_num)%duty_tot%deliv = poud_met(pou_num)%duty_tot%deliv + outflo_om%flo
      poud_om(pou_num)%pors = poud_om(pou_num)%pors + outflo_om
      
      !! add constituents withdrawn to total withdrawal for the POU
      
      return
      end subroutine wallo_withdraw
>>>>>>> Stashed changes

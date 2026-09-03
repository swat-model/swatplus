      subroutine wallo_start_day

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine starts the day for water allocation
!!    sets the out of basin source (OSRC) availability, withdrawal for OSRC and canals,
!!    and set water allocation duty (right) and fractions from each POD and to each POR

      use maximum_data_module
      use conditional_module
      use hydrograph_module
      use water_allocation_module
      use recall_module
      use hru_module
      
      implicit none
      
      integer :: id = 0         !none       |decision table number
      integer :: ipou = 0       !none       |place of use (POU) number
      integer :: ipod = 0       !none       |point of diversion (POD) number
      integer :: ipor = 0       !none       |point of return (POR) number
      integer :: iosrc = 0      !none       |outside basin source number
      integer :: irr = 0        !none       |irrigation object number
      integer :: ican = 0       !none       |canal number
      integer :: istor = 0      !none       |water storage number
      integer :: irec = 0       !none       |recall file number
      integer :: iom = 0        !none       |pointer to organic-mineral file
      integer :: j = 0          !none       |object number for decision table conditioning - leave 0 for generic tables
      integer :: iob = 0        !none       |current object number for decision table conditioning
      integer :: lev            !none       |level for concentration - typically only release at 1 level on a day

      !! allocate water (wallo_control) for all non-natural objects
      
      !! Outside Source Objects - POD Objects - typically measured flow or SWAT+ output
      do iosrc = 1, db_mx%out_src
        lev = 1
        !! use use constant, decision table, or recall object for to set the om conc for outside source water
        select case (osrc(iosrc)%conc(lev)%org_min_typ)
        case ("const")
          iom = osrc(iosrc)%conc(lev)%om_num
          outflo_om = wuse_om_efflu(iom)
                
        case ("dtbl")
          !! decision table - outside source concentrations vary with flow conditions, seasons, etc
          id = osrc(iosrc)%conc(lev)%om_num
          d_tbl => dtbl_lum(id)
          call conditions (j, id)
          call actions (j, iob, id)
          !! actions return the organic mineral number for outside source concentrations
          outflo_om = wuse_om_efflu(iom)
              
        case ("recall")
          !! use recall object for outside source concentrations
          irec = osrc(iosrc)%conc(lev)%om_num
          iom = recall_db(irec)%iorg_min
          select case (recall(iom)%typ)
          case (1)    !daily
            outflo_om = recall(iom)%hd(time%day,time%yrs)
          case (2)    !monthly
            outflo_om = recall(iom)%hd(time%mo,time%yrs)
          case (3)    !yearly
            outflo_om = recall(iom)%hd(1,time%yrs)
          case (4) !constant
            outflo_om = exco(iom)
          end select
              
        end select   ! osrc(iosrc)%conc(lev)%org_min_typ
            
        !! allocate and deliver water at start of day
        ipod = osrc(iosrc)%wallo_pod
        if (ipod > 0) then
          call wallo_control(ipod)
        end if
      end do   ! iosrc = 1, db_mx%out_src
                    
      !! if the POU does not have any PODs (inflow is from a return) - just treat/use and return the water
      do ipou = 1, db_mx%wallo_pou
        if (pou(ipou)%pods == 0) then
          select case (pou(ipou)%typ)
            case ("wtp")
              !! compute outflow and concentrations
              call wallo_treatment (ipou)
            case ("use")
              !! compute outflow and concentrations
              call wallo_use (ipou)
            end select
          !! return water to PORs
          call wallo_return (ipou)
        end if
      end do
              
      !! allocate and deliver water at start of day for water tower storage
      do istor = 1, db_mx%stor 
        ipod = wtow(istor)%wallo_pod
        if (ipod > 0) then
          call wallo_control(ipod)
        end if
      end do
          
      !! zero water allocation objects and set reset POU finishes to no
      
      !! set water allocation duty (right) and fractions from each POD and to each POR
      do ipou = 1, db_mx%wallo_pou
        !! zero water allocation objects and set reset POU finishes to "n"
        pou(ipou)%fin = "n"
        pou(ipou)%pod(:)%fin = "n"
      
        !! need irrig(j) when irrigating for condition and setting POU demand in dtbl
        pou(ipou)%demand = 0.
        if (pou(ipou)%typ == "irr") then
          do ihru = 1, pou(ipou)%irr%hru_num
            j = pou(ipou)%irr%hru(ihru)
            id = pou(ipou)%irr%dtbl_num(ihru)
            d_tbl => dtbl_lum(id)
            call conditions (j, id)
            call actions (j, iob, id)
            !! irrig(j)%demand, applied, runoff (from decision table) for each hru
            !! reset demand or duty for transfer object - convert from mm to m3
            pou(ipou)%demand = pou(ipou)%demand + irrig(j)%demand * hru(ihru)%area_ha * 10.
            irrig(j)%demand = 0.
          end do
        end if
     
        !! if no dtbl, use the maximum rate every day
        pou(ipou)%demand = pou(ipou)%rate_max * 86400. !convert to m3/s
        !! if decision table, use to set demand
        if (pou(ipou)%dtbl_mx_num > 0) then
          dmd_m3 = 0.
          id = pou(ipou)%dtbl_mx_num
          d_tbl => dtbl_flo(id)
          call conditions (j, id)
          call actions (j, iob, id)
          !! reset demand or duty for transfer object
          pou(ipou)%demand = dmd_m3
        end if
        !! set max rate for the day - convert to m3/s
        pou(ipou)%demand = Min(pou(ipou)%rate_max * 86400., pou(ipou)%demand)
        poud_met(ipou)%duty_tot%duty = pou(ipou)%demand
        poud_met(ipou)%duty_tot%deliv = 0.
        
        !! compute POD (source) fractions if decision table is used for POD fractions 
        if (pou(ipou)%dtbl_pod_fr_num > 0) then
          id = pou(ipou)%dtbl_pod_fr_num
          d_tbl => dtbl_flo(id)
          call conditions (j, id)
          call actions (j, iob, id)
          !! reset source fractions for transfer object
          do ipod = 1, pou(ipou)%pods
            pou(ipou)%pod(ipod)%frac = trn_fr(ipod)
            pou(ipou)%pod(ipod)%duty = trn_fr(ipod) * pou(ipou)%demand
            poud_met(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%duty
            poud_met(ipou)%pod(ipod)%deliv = 0.
          end do
        else
          !! use input POD (source) fractions
          do ipod = 1, pou(ipou)%pods
            pou(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%frac * pou(ipou)%demand
            poud_met(ipou)%pod(ipod)%duty = pou(ipou)%pod(ipod)%duty
            poud_met(ipou)%pod(ipod)%deliv = 0.
          end do
        end if
      
        !! compute source fractions if decision table is used for POR fractions
        if (pou(ipou)%dtbl_por_fr_num > 0) then 
          id = pou(ipou)%dtbl_por_fr_num
          j = ipou
          iob = ipou
          if (id > 0) then
            d_tbl => dtbl_flo(id)
            call conditions (j, id)
            call actions (j, iob, id)
            !! reset source fractions for transfer object
            do ipor = 1, pou(ipou)%pors
              pou(ipou)%por(ipor)%frac = trn_fr(ipor)
            end do
          end if
        end if
      
      end do    !POU loop
          
      return
      end subroutine wallo_start_day
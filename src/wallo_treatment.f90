      subroutine wallo_treatment (ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use conditional_module
      use recall_module
      
      implicit none 

      integer, intent (in):: ipou       !water allocation object number
      integer :: iwtp                   !water treatment plant number
      integer :: ipor                   !point of return number
      integer :: lev                    !level for concentration - typically only release at 1 level on a day
      integer :: iom                    !number of organic-mineral concentrations of water use
      integer :: id                     !decision table number
      integer :: j                      !object number for decision table conditioning - leave 0 for generic tables
      integer :: iob                    !current object number for decision table conditioning
      integer :: irec                   !recall object number
      integer :: itrt = 0               !treatment number
      
      !! treating water to wtp concentrations
      iwtp = pou(ipou)%typ_num
      wal_tr_omd(iwtp) = hz
      
      !! crosswalk organic mineral with water treatment database (water_treat.wal)
      do itrt = 1, wtp(iwtp)%num_treats
        select case (wtp(iwtp)%conc(itrt)%org_min_typ)
        case ("const")
          iom = wtp(iwtp)%conc(itrt)%om_num
          outflo_om = wtp_om_treat(iom)
                
        case ("dtbl")
          !! decision table - treatment concentrations vary with flow conditions, seasons, etc
          id = wtp(iwtp)%conc(itrt)%om_num
          d_tbl => dtbl_lum(id)
          call conditions (j, id)
          call actions (j, iob, id)
          !! actions return the organic mineral number for the treatment concentration
          outflo_om = wtp_om_treat(iom)
              
        case ("recall")
          !! use recall object for transfer
          irec = wtp(iwtp)%conc(itrt)%om_num
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
              
        end select   ! wtp(iwtp)%conc(itrt)%org_min_typ
      end do    ! do iom = 1, wtp(iwtp)%num_treats
              
      !! treat to different concentrations for each POR - treatment==>POR
      do ipor = 1, pou(ipou)%pors
      
        !! water in storage that will be treated - later check that treated mass less than stored mass
        wtp_om_out(iwtp) = pou(ipou)%por(ipor)%frac * wtp_om_stor(iwtp)
      
        !! treated outflow is currently set to storage - no lagging
        !! treated outflow = storage * treatment concentration (frac loss) * fraction to POR
        outflo_om%flo = wtp_om_stor(iwtp)%flo * outflo_om%flo * pou(ipou)%por(ipor)%frac
      
        !! convert concentration to mass
        call hyd_convert_conc_to_mass (outflo_om)
      
        !! treated mass can't be higher than stored mass
        call hyd_min (outflo_om, wtp_om_out(iwtp))
        
        poud_om(ipou)%por(ipor) = outflo_om
      
        !! amount that is removed
        wal_tr_omd(iwtp) = wal_tr_omd(iwtp) + wtp_om_out(iwtp) - poud_om(ipou)%por(ipor)
      
        !! treat constituents - convert concentration to mass
        if (cs_db%num_tot > 0) then
          call hydcsout_conc_mass (outflo_om%flo, wtp_cs_treat(iwtp), outflo_cs)
        end if
      end do   ! ipor loop
       
      !! treated outflow = storage - no lagging
      wtp_om_stor(iwtp) = hz
      
      return
    end subroutine wallo_treatment
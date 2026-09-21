      subroutine wallo_use (ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use conditional_module
      use recall_module
      
      implicit none 

      integer, intent (in):: ipou       !water allocation object number
      integer :: ipor                   !point of receiving object number
      integer :: iuse                   !water use number in simulation
      integer :: iom                    !number of organic-mineral concentrations of water use
      integer :: lev                    !level for concentration - typically only release at 1 level on a day
      integer :: id                     !decision table number
      integer :: j                      !object number for decision table conditioning - leave 0 for generic tables
      integer :: iob                    !current object number for decision table conditioning
      integer :: irec                   !recall object number
      
      !! domestic, industrial, commercial use concentrations
      iuse = pou(ipou)%typ_num
      lev = 1
      select case (wuse(iuse)%conc(lev)%org_min_typ)
      case ("const")
        iom = wuse(iuse)%conc(lev)%om_num
        outflo_om = wuse_om_efflu(iom)
                
      case ("dtbl")
        !! decision table - treatment concentrations vary with flow conditions, seasons, etc
        id = wuse(iuse)%conc(lev)%om_num
        d_tbl => dtbl_lum(id)
        call conditions (j, id)
        call actions (j, iob, id)
        !! actions return the organic mineral number for the treatment concentration
        outflo_om = wuse_om_efflu(iom)
              
      case ("recall")
        !! use recall object for transfer
        irec = wuse(iuse)%conc(lev)%om_num
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
              
      end select   ! wuse(iuse)%conc(itrt)%org_min_typ
      
      !! treated outflow is currently set storage - no lagging
      outflo_om%flo = outflo_om%flo * wuse_om_stor(iuse)%flo
      
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      poud_om(ipou)%pors = outflo_om
      
      !! set flow to each POR
      do ipor = 1, pou(ipou)%pors
        poud_om(ipou)%por(ipor) = pou(ipou)%por(ipor)%frac * poud_om(ipou)%pors
      end do
      
      !! amount that is added - effluent is the difference between inflow and outflow
      wal_use_omd(iuse) = poud_om(ipou)%pors - wuse_om_stor(iuse)
      
      !! zero storage after use - no lagging
      wuse_om_stor(iuse) = hz
      
      !! constituents effluent - convert concentration to mass
      if (cs_db%num_tot > 0) then
        call hydcsout_conc_mass (outflo_om%flo, wuse_cs_efflu(iuse), outflo_cs)
      end if
      
      return
    end subroutine wallo_use
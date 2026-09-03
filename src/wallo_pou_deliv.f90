      subroutine wallo_pou_deliv (ipou)
      
      use water_allocation_module
      use conditional_module
      use hydrograph_module
      use hru_module
      use time_module
      use sd_channel_module
      use plant_module
      use plant_data_module
      use constituent_mass_module
      use organic_mineral_mass_module
      use soil_module
      
      implicit none 

      integer, intent (in) :: ipou          !place of use number
      integer :: ipodu = 0                  !place of use daily duty number
      integer :: ipor = 0                   !point of receiving object number
      integer :: iob = 0                    !POD object number
      integer :: icha = 0                   !channel object number
      integer :: ird = 0                    !irrigation district number
      integer :: j = 0                      !hru number for irrigation
      integer :: jj = 0                     !
      integer :: id = 0                     !decision table id
      integer :: irdb = 0                   !irrigation farm/district number
      real :: water_avail = 0.      !m3     |water still available for irrigating each hru
      real :: rto = 0.              !ratio  |ratio of delivered vs amount in delivery object
    
      !! iob is the POU number to deliver - irr delivers to all hrus with a demand
      iob = pou(ipou)%typ_num
          select case (pou(ipou)%typ)
          !! irrigation transfer - set amount applied and runoff
          !! irrig(j)%demand,irrig(j)%applied, and irrig(j)%runoff are set in "irr_demand" action
          case ("irr")
            water_avail = poud_om(ipou)%pods%flo
            !! irrigate hru if amount water is available
            do ird = 1, pou(ipou)%irr%hru_num
              if (irrig(j)%demand < water_avail) then
                  j = pou(ipou)%irr%hru(ihru)
                  id = pou(ipou)%irr%dtbl_num(ihru)
                  d_tbl => dtbl_lum(id)
                  call conditions (j, id)
                  !! irrig(j)%demand, applied, runoff (from decision table) for each hru
                  call actions (j, iob, id)
                  
                  water_avail = water_avail - irrig(j)%demand
                  
                  !! reset days since last irrigation
                  pcom(j)%days_irr = 1
               
                  !! add organics and minerals to the soil
                  rto = irrig(j)%demand / poud_om(ipou)%pods%flo
                  !no3 = rto * poud_om(ipou)%pods%no3
                  !!add no3, nh4, solp, orgn, and orgp to soil1(j)
                  
                  !! rtb salt: irrigation salt mass accounting
                  !if(cs_db%num_salts > 0) then
                  !  jj = itrn !to avoid a compiler warning
                  !  call salt_irrig(iwallo,jj,j)
                  !endif
                  
                  !!rtb cs: irrigation constituent mass accounting
                  !if(cs_db%num_cs > 0) then
                  !  jj = itrn !to avoid a compiler warning
                  !  call cs_irrig(iwallo,jj,j)
                  !endif
              
                  !! add irrigation to yearly sum for dtbl conditioning jga6-25
                  hru(j)%irr_yr = hru(j)%irr_yr + irrig(j)%applied
            
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo, "WATER ALLO", "IRRIGATE", phubase(j),  &
                      pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, pl_mass(j)%rsd_tot%m,      &
                      sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
                  end if
              else 
                !! zero irrigation data if water is not available
                irrig(j)%runoff = 0.
                irrig(j)%applied = 0.
                irrig(j)%eff = 0.
                irrig(j)%demand = 0.
              end if   !demand < available
              
            end do     !ird loop
            
            !! divert flow into the channel in sd_channel_control3
            case ("cha")
              icha = sd_ch(iob)%obj_no
              ob(icha)%trans = ob(icha)%trans + poud_om(ipou)%pods
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
                res(iob) = res(iob) + poud_om(ipou)%pods
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(iob) = aqu(iob) + poud_om(ipou)%pods
              !! calculate water table depth
              
            case ("wtp")
              if (pou(ipou)%pods > 0) then
                !! wastewater treatment 
                wtp_om_stor(iob) = wtp_om_stor(iob) + poud_om(ipou)%pods
                !! compute outflow and concentrations
                call wallo_treatment (ipou)
              end if
              
            case ("use")
              if (pou(ipou)%pods > 0) then
                !! water use (domestic, industrial, commercial) 
                wuse_om_stor(iob) = wuse_om_stor(iob) + poud_om(ipou)%pods
                !! compute outflow and concentrations
                call wallo_use (ipou)
              end if
              
            case ("wtow")
              !! water tower storage - doesn't change concentrations or compute outflow
              wtow_om_stor(iob) = wtow_om_stor(iob) + poud_om(ipou)%pods
           
            case ("can")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(iob) = canal_om_stor(iob) + poud_om(ipou)%pods
              !! compute losses - evap and seepage, and outflow
              call wallo_canal (iob)
              
            case ("orcv")
              !! outside receiving object
              orcv_om(iob) = orcv_om(iob) + poud_om(ipou)%pods
           
            end select
            
      return
      end subroutine wallo_pou_deliv
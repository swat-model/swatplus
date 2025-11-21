      subroutine wallo_control (iwallo)
      
      use water_allocation_module
      use hydrograph_module   !, only : irrig, hz, recall
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module !rtb
      
      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer :: itrn = 0                   !water demand object number
      integer :: iosrc = 0                  !source object number
      integer :: isrc = 0                   !source object number
      integer :: j = 0                      !hru number
      integer :: jj = 0                     !variable for passing
      integer :: irec = 0                   !recall id
      integer :: dum = 0
      real :: irr_mm = 0.                   !mm     |irrigation applied
      real :: div_total = 0.                !m3     |cumulative available diversion water
      real :: div_daily = 0.                !m3     |daily water diverted for irrigation
      

      !! zero demand, withdrawal, and unmet for entire allocation object
      wallo(iwallo)%tot = walloz
      
      !! zero total transfer and treatment and use outflows
      wal_omd(iwallo)%trn(:)%h_tot = hz
      wtp_om_out = hz
      wuse_om_out = hz
      
      !!loop through each demand object
      do itrn = 1, wallo(iwallo)%trn_obs
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wallod_out(iwallo)%trn(itrn)%src(isrc) = walloz
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = hz
        end do
  
        !! compute flow from outside sources
        do isrc = 1, wallo(iwallo)%src_obs
          if (wallo(iwallo)%src(isrc)%ob_typ == "osrc") then
            iosrc = wallo(iwallo)%src(isrc)%ob_num
            select case (wallo(iwallo)%src(isrc)%lim_typ)
            case ("mon_lim")
              osrc_om_out(iosrc)%flo = wallo(iwallo)%src(isrc)%limit_mon(time%mo)
            case ("dtbl")
              !! use decision table for outflow
            case ("recall")
              !! use recall for outflow
            end select
          end if
        end do
          
        !! set demand for each transfer object - wallod_out(iwallo)%trn(itrn)%trn_flo
        call wallo_demand (iwallo, itrn)
        
        !! initialize unmet to total demand and subtract as water is withdrawn
        wallo(iwallo)%trn(itrn)%unmet_m3 = wallod_out(iwallo)%trn(itrn)%trn_flo
      
        !! zero demand, withdrawal, and unmet for each source
        wallod_out(iwallo)%trn(itrn)%src(:) = walloz

        !! compute demand for each source object
        wdraw_om_tot = hz
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wallod_out(iwallo)%trn(itrn)%src(isrc)%demand = wallo(iwallo)%trn(itrn)%src(isrc)%frac *      &
                                                                wallod_out(iwallo)%trn(itrn)%trn_flo
        end do
 
        !! if demand - check source availability and withdraw water
        if (wallod_out(iwallo)%trn(itrn)%trn_flo > 0.) then
            
          !! check if water is available from each source - set withdrawal and unmet - wallo(iwallo)%trn(itrn)%src(isrc)%hd
          wdraw_om_tot = hz
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            trn_m3 = wallod_out(iwallo)%trn(itrn)%src(isrc)%demand
            if (trn_m3 > 1.e-6) then
              call wallo_withdraw (iwallo, itrn, isrc)
            end if
          end do
        
          !! loop through sources again to check if compensation is allowed
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            if (wallo(iwallo)%trn(itrn)%src(isrc)%comp == "y") then
              trn_m3 = wallo(iwallo)%trn(itrn)%unmet_m3
              if (trn_m3 > 1.e-6) then
                call wallo_withdraw (iwallo, itrn, isrc)
              end if
            end if
          end do
        
          !! compute total withdrawal for receiving object from all sources
          wallo(iwallo)%trn(itrn)%withdr_tot = 0.
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            wallo(iwallo)%trn(itrn)%withdr_tot = wallo(iwallo)%trn(itrn)%withdr_tot +           &
                                                  wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr
          end do
        
          !! transfer water (pipes) to receiving object from all sources
          wallo(iwallo)%trn(itrn)%withdr_tot = 0.
          call wallo_transfer (iwallo, itrn)
        
          !! add water withdrawn from source to the receiving object  - wal_omd(iwallo)%trn(itrn)%h_tot
          j = wallo(iwallo)%trn(itrn)%rcv%num
          select case (wallo(iwallo)%trn(itrn)%rcv%typ)
          !! irrigation transfer - set amount applied and runoff
          case ("hru")
            if (wallo(iwallo)%trn(itrn)%withdr_tot > 0.) then
              irr_mm = wallo(iwallo)%trn(itrn)%withdr_tot / (hru(j)%area_ha * 10.)      !mm = m3 / (ha * 10.)
              irrig(j)%applied = irr_mm * wallo(iwallo)%trn(itrn)%irr_eff * (1. - wallo(iwallo)%trn(itrn)%surq)
              irrig(j)%runoff = wallo(iwallo)%trn(itrn)%amount * wallo(iwallo)%trn(itrn)%surq
              pcom(j)%days_irr = 1            ! reset days since last irrigation
              
              !! send runoff to canal?
              
              !rtb salt: irrigation salt mass accounting
              if(cs_db%num_salts > 0) then
                jj = itrn !to avoid a compiler warning
                call salt_irrig(iwallo,jj,j)
              endif
              !rtb cs: irrigation constituent mass accounting
              if(cs_db%num_cs > 0) then
                jj = itrn !to avoid a compiler warning
                call cs_irrig(iwallo,jj,j)
              endif
              
              ! add irrigation to yearly sum for dtbl conditioning jga6-25
              hru(j)%irr_yr = hru(j)%irr_yr + irrig(j)%applied
            
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, wallo(iwallo)%name, "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, soil1(j)%rsd(1)%m,           &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            end if
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
              res(j) = res(j) + wal_omd(iwallo)%trn(itrn)%h_tot
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(j) = aqu(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! calculate water table depth
              
            case ("wtp")
              !! wastewater treatment 
              wtp_om_stor(j) = wtp_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! compute outflow and concentrations
              call wallo_treatment (iwallo, itrn, j)
              
            case ("use")
              !! water use (domestic, industrial, commercial) 
              wuse_om_stor(j) = wuse_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! compute outflow and concentrations
              call wallo_use (iwallo, itrn, j)
              
            case ("stor")
              !! water tower storage - don't change concentrations or compute outflow
              wtow_om_stor(j) = wtow_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
           
            case ("canal")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(j) = canal_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! compute losses - evap and seepage, and outflow
              ! call canal()
          end select
        
        end if      !if there is demand 
        
        !! sum organics 
        wdraw_om_tot = wdraw_om_tot + wdraw_om
        
        !! sum constituents
        
        !! sum demand, withdrawal, and unmet for entire allocation object
        wallo(iwallo)%tot%demand = wallo(iwallo)%tot%demand + wallod_out(iwallo)%trn(itrn)%trn_flo
        wallo(iwallo)%tot%withdr = wallo(iwallo)%tot%withdr + wallo(iwallo)%trn(itrn)%withdr_tot
        wallo(iwallo)%tot%unmet = wallo(iwallo)%tot%unmet + wallo(iwallo)%trn(itrn)%unmet_m3
        
      end do        !demand object loop
        
      return
      end subroutine wallo_control
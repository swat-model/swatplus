      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. Depending on the 
!!    command code on the .fig file line, a command loop is accessed
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, route, routres, transfer, recmon
!!    SWAT: recepic, save, recday, recyear

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hydrograph_module
      use ru_module
      use channel_module
      use hru_lte_module
      use aquifer_module
      use sd_channel_module
      use reservoir_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use hru_module, only : ihru, hru
      use basin_module
      use maximum_data_module
      use gwflow_module
      use soil_module
      implicit none

      real, dimension(time%step) :: hyd_flo     !flow hydrograph
      integer :: in                   !              | 
      integer :: iob                  !              |
      integer :: iday                 !              |
      integer :: isd                  !none          |counter
      integer :: ires                 !none          |reservoir number
      integer :: irec                 !              |
      integer :: iout                 !none          |counter
      integer :: ihtyp                !              |
      integer :: iaq                  !none          |counter
      integer :: j                    !none          |counter
      integer :: nly, ly
      integer :: ihyd                 !              |
      integer :: idr                  !              |
      integer :: iwro                 !              |
      real :: conv                    !              |
      real :: frac_in                 !              |
      integer :: ts1,ts2
      integer i_count                 !rtb gwflow
      integer :: i_mfl,i_chan         !rtb gwflow    |counter
      real :: sumflo

      icmd = sp_ob1%objs
      do while (icmd /= 0)
        !subdaily - set current day of hydrograph
       !if (time%step > 0) then
          if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then
            !! hru and ru can have hyrdographs that lag into next day
            ob(icmd)%day_cur = ob(icmd)%day_cur + 1
            if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
          else
            !! assume only one day is saved for all other objects
            ob(icmd)%day_cur = 1
            !update current day of hydrograph for the object
            ob(icmd)%day_cur = ob(icmd)%day_cur + 1
            if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
          end if
        !end if
        
        
        !sum all receiving hydrographs
        !if (ob(icmd)%rcv_tot > 0) then
          ob(icmd)%hin = hz
          ob(icmd)%hin_sur = hz
          ob(icmd)%hin_lat = hz
          ob(icmd)%hin_til = hz
          ht1 = hz
          if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
            obcs(icmd)%hin = hin_csz
            obcs(icmd)%hin_sur = hin_csz
            obcs(icmd)%hin_lat = hin_csz
            obcs(icmd)%hin_til = hin_csz
          endif
          hcs1 = hin_csz
          hcs2 = hin_csz
          hcs3 = hin_csz
          ob(icmd)%tsin = 0.
          ob(icmd)%peakrate = 0.
          hyd_flo = 0.
          
          if (ob(icmd)%rcv_tot > 0) then
          do in = 1, ob(icmd)%rcv_tot
            iob = ob(icmd)%obj_in(in)
            ihyd = ob(icmd)%ihtyp_in(in)
            frac_in = ob(icmd)%frac_in(in)
            ob(icmd)%peakrate = ob(iob)%peakrate
            
            ! if object is not an hru, need ht1, don't need %hin_sur and %hin_lat
            ! don't have to check if it's in an ru - only hru's can be routed over
            if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru" .or. ob(icmd)%typ == "hru_lte") then
                
              !! if incoming object is not an hru or ru, send total hyd to surface runoff
              if (ob(icmd)%obtyp_in(in) == "hru" .or. ob(icmd)%obtyp_in(in) == "ru" .or.          &
                                                       ob(icmd)%obtyp_in(in) == "hru_lte") then
                ! recieving hru, needs %hin_sur and %hin_lat and %hin_til to route separately in hru_control
                if (ob(icmd)%htyp_in(in) == "tot") then
                  ! if total hyd coming in from hru or ru -> add both surface and lateral flows
                  ! add to surface runon
                  ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(3)
                  if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                    obcs(icmd)%hin_sur(1) = obcs(icmd)%hin_sur(1) + frac_in * obcs(iob)%hd(3)
                  end if
                  ! add to lateral soil runon
                  ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(4)
                  if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                    obcs(icmd)%hin_lat(1) = obcs(icmd)%hin_lat(1) + frac_in * obcs(iob)%hd(4)
                  end if
                else
                  ! if hyd in is not a total hyd from an hru or ru -> add the specified hyd typ 
                  select case (ob(icmd)%htyp_in(in))
                  case ("tot")   ! total flow
                    ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_til(1) = obcs(icmd)%hin_til(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("sur")   ! surface runoff
                    ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_sur(1) = obcs(icmd)%hin_sur(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("lat")   ! lateral soil flow
                    ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_lat(1) = obcs(icmd)%hin_lat(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("til")   ! tile flow
                    ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_til(1) = obcs(icmd)%hin_til(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("aqu")   ! aquifer inflow
                    ob(icmd)%hin_aqu = ob(icmd)%hin_aqu + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_aqu(1) = obcs(icmd)%hin_aqu(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  end select
                end if  
              else
                ! add total inflow to surface runon if channel or recall
                ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(1)
                if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                  obcs(icmd)%hin_sur(1) = obcs(icmd)%hin_sur(1) + frac_in * obcs(iob)%hd(1)
                end if
              end if
              
            else
              ! all objects other than hru's
              ! fraction of organics
              ht1 = frac_in * ob(iob)%hd(ihyd)
              ob(icmd)%hin = ob(icmd)%hin + ht1

              !rtb hydrograph separation
              hdsep1%flo_surq = frac_in * (ob(iob)%hdsep%flo_surq)
              hdsep1%flo_latq = frac_in * (ob(iob)%hdsep%flo_latq)
              hdsep1%flo_gwsw = frac_in * (ob(iob)%hdsep%flo_gwsw)
              hdsep1%flo_swgw = frac_in * (ob(iob)%hdsep%flo_swgw)
              hdsep1%flo_satex = frac_in * (ob(iob)%hdsep%flo_satex)
              hdsep1%flo_satexsw = frac_in * (ob(iob)%hdsep%flo_satexsw)
              hdsep1%flo_tile = frac_in * (ob(iob)%hdsep%flo_tile)
              ob(icmd)%hdsep_in%flo_surq = ob(icmd)%hdsep_in%flo_surq + hdsep1%flo_surq
              ob(icmd)%hdsep_in%flo_latq = ob(icmd)%hdsep_in%flo_latq + hdsep1%flo_latq
              ob(icmd)%hdsep_in%flo_gwsw = ob(icmd)%hdsep_in%flo_gwsw + hdsep1%flo_gwsw
              ob(icmd)%hdsep_in%flo_swgw = ob(icmd)%hdsep_in%flo_swgw + hdsep1%flo_swgw
              ob(icmd)%hdsep_in%flo_satex = ob(icmd)%hdsep_in%flo_satex + hdsep1%flo_satex
              ob(icmd)%hdsep_in%flo_satexsw = ob(icmd)%hdsep_in%flo_satexsw + hdsep1%flo_satexsw
              ob(icmd)%hdsep_in%flo_tile = ob(icmd)%hdsep_in%flo_tile + hdsep1%flo_tile
              
              ! fraction of constituents
              if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                hcs1 = frac_in * obcs(iob)%hd(ihyd)
                obcs(icmd)%hin(1) = obcs(icmd)%hin(1) + hcs1
              end if
              ob(icmd)%hin_d(in) = ht1        !for hydrograph output
              if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                obcs(icmd)%hcsin_d(in) = hcs1   !for constituent hydrograph output
              endif
            end if
            
            !sum subdaily inflow hydrographs
            !if (time%step > 0) then
              iday = ob(iob)%day_cur
              if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "ru") then
                select case (ob(icmd)%htyp_in(in))
                case ("tot")   ! total flow
                  hyd_flo = ob(iob)%hyd_flo(iday,:) + (ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo) / time%step
                case ("sur")   ! surface runoff
                  hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                case ("rhg")   ! recharge
                  hyd_flo(:) = ob(iob)%hd(2)%flo / time%step
                case ("lat")   ! lateral soil flow
                  hyd_flo(:) = ob(iob)%hd(4)%flo / time%step
                case ("til")   ! tile flow
                  hyd_flo(:) = ob(iob)%hd(5)%flo / time%step
                end select
              end if
              select case (ob(iob)%typ)
              case ("aqu")      ! aquifer inflow
                hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
              case ("chandeg")  ! channel inflow
                hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                sumflo = sum (hyd_flo(:))
                sumflo = 1. * sumflo
              case ("res")      ! reservoir inflow
                hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
              case ("recall")   ! point source inflow
                irec = ob(iob)%num
                if (recall(irec)%typ == 0) then    !subdaily
                  hyd_flo(:) = ob(iob)%hyd_flo(ob(iob)%day_cur,:)
                else                                ! monthly, yearly, and ave annual
                  hyd_flo(:) = ob(iob)%hd(1)%flo / time%step
                end if
              end select
                
              !! multiply inflow hyd by the fraction of incoming
              hyd_flo = frac_in * hyd_flo
              !! add flow hydrographs for each incoming object
              ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
              
            !end if

          end do    ! in = 1, ob(icmd)%rcv_tot

          !convert to per area basis
          if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then  !only convert hru and subbasin hyds for routing
            conv = ob(icmd)%area_ha
            ob(icmd)%hin_sur = ob(icmd)%hin_sur / conv
            ob(icmd)%hin_sur%flo = ob(icmd)%hin_sur%flo / 10.      ! m3/10*ha = mm
            ob(icmd)%hin_lat = ob(icmd)%hin_lat / conv
            ob(icmd)%hin_lat%flo = ob(icmd)%hin_lat%flo / 10.      ! m3/10*ha = mm
            ob(icmd)%hin_til = ob(icmd)%hin_til / conv
            ob(icmd)%hin_til%flo = ob(icmd)%hin_til%flo / 10.      ! m3/10*ha = mm
          end if
        end if

        ! select the next command type
        select case (ob(icmd)%typ)
            
          case ("hru")   ! hru
            ihru = ob(icmd)%num
            call hru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output
                      
          case ("hru_lte")   ! hru_lte
            isd = ob(icmd)%num
            call hru_lte_control (isd)
            !if (ob(icmd)%rcv_tot > 0) call hyddep_output
            
          case ("ru")   ! subbasin
            iru = ob(icmd)%num
            call ru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output

          case ("gwflow")   ! gwflow
            call gwflow_simulate
            do i_mfl = 1,sp_ob%gwflow
              icmd = icmd + 1
            enddo
            icmd = icmd - 1
            
          case ("aqu")   ! aquifer
            if (ob(icmd)%dfn_tot == 0) then   !1-D use old bf recession
              call aqu_1d_control
            end if
          
          !case ("chan")   ! channel
          !  jrch = ob(icmd)%num
          !  jrchq = ob(icmd)%props2
          !  if (ob(icmd)%rcv_tot > 0) then
          !    call channel_control
          ! end if

          case ("res")   ! reservoir
            ires = ob(icmd)%num
            if (ob(icmd)%rcv_tot > 0) then
              call res_control (ires)
            end if 
              
          case ("recall")   ! recall hydrograph
            irec = ob(icmd)%num
            select case (recall(irec)%typ)
              case (0)    !subdaily
                ts1 = (time%day - 1) * time%step + 1
                ts2 = time%day * time%step
                ob(icmd)%hyd_flo(ob(icmd)%day_cur,:) = recall(irec)%hyd_flo(ts1:ts2,time%yrs)
                ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
              case (1)    !daily
                if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
                    ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
                    !if negative flow (diversion), then remove nutrient mass
                    if(recall(irec)%hd(time%day,time%yrs)%flo < 0) then
                      call recall_nut(irec)
                    endif
                else
                    ob(icmd)%hd(1) = hz
                end if
              case (2)    !monthly
                if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
                    ob(icmd)%hd(1) = recall(irec)%hd(time%mo,time%yrs)
                else
                    ob(icmd)%hd(1) = hz
                end if
              case (3)    !annual
                if (time%yrc >= recall(irec)%start_yr .or. time%yrc <= recall(irec)%end_yr) then
                  ob(icmd)%hd(1) = recall(irec)%hd(1,time%yrs)
                else
                  ob(icmd)%hd(1) = hz
                end if
              case (4)    !average annual
                ob(icmd)%hd(1) = recall(irec)%hd(1,1)
              end select
              
              rec_d(irec) = ob(icmd)%hd(1)

              if(cs_db%num_salts > 0) call recall_salt(irec) !rtb salt
              if(cs_db%num_cs > 0) call recall_cs(irec) !rtb cs
              
          !case ("exco")   ! export coefficient hyds are set at start

          case ("dr")   ! delivery ratios
            ob(icmd)%hd(1) = ob(icmd)%hin ** dr(ob(icmd)%props) ! ** is an intrinsic function to multiply 
            if (cs_db%num_tot > 0) then
              idr = ob(iob)%props
              
              call constit_hyd_mult (icmd, idr)
            end if
            
          case ("outlet")  !outlet
            ob(icmd)%hd(1) = ob(icmd)%hin
            if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
              obcs(icmd)%hd(1) = obcs(icmd)%hin(1) !rtb salt/cs
            endif
              
          case ("chandeg")  !swatdeg channel
            isdch = ob(icmd)%num
            isd_chsur = ob(icmd)%props2
            if (sd_ch(isdch)%chl > 1.e-3) then
              if (bsn_cc%i_fpwet == 0) then
                call sd_channel_control3
              end if
              if (bsn_cc%i_fpwet == 1) then
                call sd_channel_control
              end if
              if (bsn_cc%i_fpwet == 2) then
                call sd_channel_control2
              end if
            else
                !! artificial channel - length=0 - no transformations
                ob(icmd)%hd(1) = ob(icmd)%hin
                
                ch_in_d(isdch) = ht1                        !set inflow om hydrograph
                chsd_d(isdch)%flo_in = ht1%flo / 86400.     !flow for morphology output
                ch_in_d(isdch)%flo = ht1%flo / 86400.       !flow for om output
                ch_out_d(isdch) = ht1                       !set inflow om hydrograph
                ch_out_d(isdch)%flo = ht1%flo / 86400.      !m3 -> m3/s
                !! output channel morphology
                chsd_d(isdch)%flo = ht1%flo / 86400.        !adjust if overbank flooding is moved to landscape
                chsd_d(isdch)%peakr = 0. 
                chsd_d(isdch)%sed_in = ob(icmd)%hin%sed
                chsd_d(isdch)%sed_out = ob(icmd)%hin%sed
                chsd_d(isdch)%washld = 0.
                chsd_d(isdch)%bedld = 0.
                chsd_d(isdch)%dep = 0.
                chsd_d(isdch)%deg_btm = .0
                chsd_d(isdch)%deg_bank = 0.
                chsd_d(isdch)%hc_sed = 0.
                chsd_d(isdch)%width = sd_ch(isdch)%chw
                chsd_d(isdch)%depth = sd_ch(isdch)%chd
                chsd_d(isdch)%slope = sd_ch(isdch)%chs
                chsd_d(isdch)%deg_btm_m = 0.
                chsd_d(isdch)%deg_bank_m = 0.
                chsd_d(isdch)%hc_m = 0.
                if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                  obcs(icmd)%hd(1) = obcs(icmd)%hin(1)
                end if
            end if
            
          end select
        if (pco%fdcout == "y" .and. ob(icmd)%typ == "chandeg") then
          call flow_dur_curve
          !! compute flashiness index
          ob(icmd)%flash_idx%sum_q_q1 = ob(icmd)%flash_idx%sum_q_q1 + (ob(icmd)%hd(1)%flo - ob(icmd)%flash_idx%q_prev)
          ob(icmd)%flash_idx%q_prev = ob(icmd)%hd(1)%flo
          ob(icmd)%flash_idx%sum_q = ob(icmd)%flash_idx%sum_q + ob(icmd)%hd(1)%flo
        end if
  
        !print all outflow hydrographs
        if (time%yrs > pco%nyskip) then
          if (ob(icmd)%src_tot > 0) then
            do iout = 1, ob(icmd)%src_tot
              ihtyp = ob(icmd)%ihtyp_out(iout)
              ht1 = ob(icmd)%frac_out(iout) * ob(icmd)%hd(ihtyp)
              call hydout_output (iout)
            end do
          end if
        end if
        
        !set the next command
        icmd = ob(icmd)%cmd_next
        
      end do

      !! write object output for entire simulation
      call obj_output
      
      !! print all output files
      if (time%yrs > pco%nyskip) then
      
        !! print water allocation output
        do iwro =1, db_mx%wallo_db
          call water_allocation_output (iwro)
        end do
        
        !! print manure allocation output
        do iwro =1, db_mx%mallo_db
          call manure_source_output (iwro)
          call manure_demand_output (iwro)
        end do
        
        do isd = 1, sp_ob%hru_lte
          call hru_lte_output (isd)
        end do
        
        do ihru = 1, sp_ob%hru
          call hru_output (ihru)
          call hru_carbon_output (ihru)
          if (hru(ihru)%dbs%surf_stor > 0) then
            call wetland_output(ihru)
            if (cs_db%num_salts > 0) then !rtb salt
              call wet_salt_output(ihru)
            endif
            if (cs_db%num_cs > 0) then !rtb cs
              call wet_cs_output(ihru)
            endif
          end if
          if (cs_db%num_tot > 0) then 
            call hru_pesticide_output (ihru)
            call hru_pathogen_output (ihru)
          end if
          if (cs_db%num_salts > 0) then !rtb salt
            call hru_salt_output(ihru)
          endif
          if (cs_db%num_cs > 0) then !rtb cs
            call hru_cs_output(ihru)
          endif
          !sum annual for SWIFT input
          if (bsn_cc%swift_out == 1) then
            icmd = hru(ihru)%obj_no
            do ihyd = 1, 5
              ob(icmd)%hd_aa(ihyd) = ob(icmd)%hd_aa(ihyd) + ob(icmd)%hd(ihyd)
            end do
          end if
          
          !! carbon output for testing  ***jga
          !if ((time%yrc == 2007 .AND. time%day == 213) .OR. (time%yrc == 2010 .AND. time%day == 319)            &
          !                                              .OR.(time%yrc == 2011 .AND. time%day == 324)) then 
          if (ihru == 1) then
            do nly = 1, soil(ihru)%nly
              !soil1(ihru)%tot(nly)%c = soil1(ihru)%hact(nly)%c + soil1(ihru)%hsta(nly)%c + soil1(ihru)%microb(nly)%c
            end do
            write (9999,*) time%day, time%mo, time%day_mo, time%yrc, ob(ihru)%typ, ob(ihru)%name,           &
                                                   (soil1(ihru)%tot(ly)%c/1000, ly = 1, soil(ihru)%nly)
          end if
                                                        
        select case (pco%carbout)
        !! write carbon in soil, plant, and residue at end of the day
          case ("d")
            call soil_nutcarb_write
          !! write carbon in soil, plant, and residue at end the month    
          case ("m")
            if (time%end_mo == 1) then
              call soil_nutcarb_write
            end if 
          !! write carbon in soil, plant, and residue at end of year  
          case ("y")
            if (time%end_yr == 1) then
              call soil_nutcarb_write
            end if
         !! write carbon in soil, plant, and residue at end the simulation  
          case ("a") 
              call soil_nutcarb_write 
         end select 
        
        end do      ! hru loop  
        
        do iaq = 1, sp_ob%aqu
          call aquifer_output (iaq)
          if (cs_db%num_salts > 0) then !rtb salt
            call aqu_salt_output (iaq)
          endif
          if (cs_db%num_cs > 0) then !rtb cs
            call aqu_cs_output(iaq)
          endif  
          if (cs_db%num_tot > 0) then 
            call aqu_pesticide_output (iaq)
          end if       
        end do
        
        do jrch = 1, sp_ob%chan
          call channel_output (jrch)
        end do
                
        do jrch = 1, sp_ob%chandeg
          call sd_chanmorph_output (jrch)
          call sd_chanbud_output (jrch)
          call sd_channel_output (jrch)
          if (cs_db%num_tot > 0) then 
            call cha_pesticide_output (jrch)   
            !call ch_pathogen_output (jrch)
          end if   
          if (cs_db%num_salts > 0) then !rtb salt
            call ch_salt_output (jrch)
          endif
          if (cs_db%num_cs > 0) then
            call ch_cs_output (jrch) !rtb cs
          endif
        end do
        if(cs_db%num_cs > 0) then
          call cs_str_output !rtb cs
        endif
        

        do j = 1, sp_ob%res
          call reservoir_output(j)
         if (cs_db%num_tot > 0) then 
            call res_pesticide_output (j)
            if (cs_db%num_salts > 0) then !rtb salt
              call res_salt_output (j)
						endif
            if (cs_db%num_cs > 0) then !rtb cs
              call res_cs_output (j)
            endif
            !call res_pathogen_output (j)
          end if       
        end do 
        
        do j = 1, sp_ob%ru
          call ru_output(j)
          if(cs_db%num_salts > 0) then !rtb salt
            call ru_salt_output(j)
          endif
          if(cs_db%num_cs > 0) then !rtb cs
            call ru_cs_output(j)
          endif
        end do
        
        do j = 1, sp_ob%recall
          call recall_output (j)
        end do

        call hydin_output   !if all output is no, then don"t call
        !call hcsin_output  gives allocate error
        if (sp_ob%chandeg > 0 .and. cs_db%num_pests > 0) call basin_ch_pest_output  
        if (sp_ob%res > 0 .and. cs_db%num_pests > 0) call basin_res_pest_output     
        if (sp_ob%hru > 0 .and. cs_db%num_pests > 0) call basin_ls_pest_output
        if (sp_ob%aqu > 0 .and. cs_db%num_pests > 0) call basin_aqu_pest_output
        if (db_mx%lsu_elem > 0) call basin_output
        if (db_mx%lsu_out > 0) call lsu_output
        if (db_mx%aqu_elem > 0) call basin_aquifer_output
        !if (sp_ob%aqu > 0) call basin_aquifer_output !rtb - otherwise, aquifer output is not called
        if (sp_ob%res > 0) call basin_reservoir_output
        if (sp_ob%chan > 0) call basin_channel_output
        if (sp_ob%chandeg > 0) call basin_chanmorph_output
        if (sp_ob%chandeg > 0) call basin_chanbud_output
        if (sp_ob%chandeg > 0) call basin_sdchannel_output
        if (sp_ob%recall > 0) call basin_recall_output
        !call soil_nutcarb_output
        !call lsreg_output
        !call region_aquifer_output
        !call region_reservoir_output
        !call region_channel_output
        !call region_recall_output
        
        if(cs_db%num_salts > 0) call salt_balance !rtb salt
        if(cs_db%num_cs > 0) call cs_balance !rtb cs
        
      end if

      gw_daycount = gw_daycount + 1
      
      !rtb hydrograph separation
      !write out hydrograph components for selected channels
      if (bsn_cc%gwflow == 1) then
      do i_chan=1,sp_ob%chandeg
        if(hydsep_flag(i_chan) == 1) then
          write(out_hyd_sep,102) time%yrc,time%day,i_chan,(hyd_sep_array(i_chan,i_count),i_count=1,7)
        endif
      enddo
      endif
      !zero out arrays for next day
      icmd = sp_ob1%objs
      do while (icmd /= 0)
        ob(icmd)%hdsep%flo_surq = 0.
        ob(icmd)%hdsep%flo_latq = 0.
        ob(icmd)%hdsep%flo_gwsw = 0.
        ob(icmd)%hdsep%flo_swgw = 0.
        ob(icmd)%hdsep%flo_satex = 0.
        ob(icmd)%hdsep%flo_satexsw = 0.
        ob(icmd)%hdsep%flo_tile = 0.
        ob(icmd)%hdsep_in%flo_surq = 0.
        ob(icmd)%hdsep_in%flo_latq = 0.
        ob(icmd)%hdsep_in%flo_gwsw = 0.
        ob(icmd)%hdsep_in%flo_swgw = 0.
        ob(icmd)%hdsep_in%flo_satex = 0.
        ob(icmd)%hdsep_in%flo_satexsw = 0.
        ob(icmd)%hdsep_in%flo_tile = 0.  
        icmd = ob(icmd)%cmd_next
      enddo
      
102   format(i6,11x,i3,8x,i5,5x,1000(f16.4))
103   format(4i6,2i8,2x,a,35f12.3)      

      
      return
      end
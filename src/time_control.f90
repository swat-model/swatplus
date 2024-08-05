      subroutine time_control

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the loops governing the modeling of processes
!!    in the watershed 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    biomix(:)   |none          |biological mixing efficiency.
!!                               |Mixing of soil due to activity of earthworms
!!                               |and other soil biota. Mixing is performed at
!!                               |the end of every calendar year.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |counter
!!    iix         |none          |sequence number of current year in rotation
!!    iiz         |none          |sequence number of current crop grown
!!                               |within the current year
!!    xx          |none          |current year in simulation sequence
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Mod, Real
!!    SWAT: std3, xmon, sim_initday, clicon, command
!!    SWAT: writed, writem, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use maximum_data_module
      use calibration_data_module
      use plant_data_module
      use mgt_operations_module
      use hru_module, only : hru, ihru, ipl, phubase, yr_skip
      use plant_module
      use soil_module
      use time_module
      use climate_module
      use basin_module
      use sd_channel_module
      use hru_lte_module
      use basin_module
      use hydrograph_module, only : sp_ob, sp_ob1, ob, chaz, ch_stor_y, ch_in_y, ch_out_y
      use output_landscape_module
      use conditional_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use water_body_module
      use water_allocation_module
      
      implicit none

      integer :: j                   !none          |counter
      integer :: julian_day          !none          |counter
      integer :: id                  !              |
      integer :: isched              !              |
      integer :: ich                 !none          |counter
      integer :: idp                 !              |
      integer :: iplt
      integer :: iupd                !none          |counter
      integer :: ipest               !none          |counter
      integer :: date_time(8)        !              | 
      character*10 b(3)              !              |
      real :: crop_yld_t_ha          !t/ha          |annual and ave annual basin crop yields
      real :: sw_init
      real :: sno_init
      integer :: iob                 !              |
      integer :: curyr               !              |
      integer :: mo                  !              |
      integer :: day_mo              !              |
      integer :: iwallo, imallo
      time%yrc = time%yrc_start
      
      !! generate precip for the first day - %precip_next
      if (Mod(time%yrc,4) == 0) then 
        time%day_end_yr = ndays_leap(13)
      else 
        time%day_end_yr = ndays_noleap(13)
      end if

      time%yrs = 1
      time%day = time%day_start
      call xmon (time%day, mo, day_mo)
      time%mo = mo
      time%day_mo = day_mo
      call cli_precip_control (0)

      do curyr = 1, time%nbyr
    !!!!!  uncomment next three lines for RELEASE version only (Srin/Karim)
          !call DATE_AND_TIME (b(1), b(2), b(3), date_time)
          !write (*,1235) cal_sim, time%yrc
    !1235  format (1x, a, 2x, i4)
          
        time%yrs = curyr

        !! determine beginning and ending dates of simulation in current year
        if (Mod(time%yrc,4) == 0) then
          if (Mod(time%yrc,100) == 0) then
            if (Mod(time%yrc,400) == 0) then
              ndays = ndays_leap
              time%num_leap = time%num_leap + 1
            else
              ndays = ndays_noleap
            end if
          else
            ndays = ndays_leap
            time%num_leap = time%num_leap + 1
          end if
        else 
          ndays = ndays_noleap
        end if

        !! set beginning day of simulation for year
        if (time%yrs > 1 .or. time%day_start == 0) then
          time%day_start = 1
        end if

        !! set ending day of simulation for year
        time%day_end_yr = ndays(13)
        if (time%yrs == time%nbyr .and. time%day_end > 0) then
          time%day_end_yr = time%day_end
          time%day_end_yr =  amin0 (time%day_end_yr, ndays(13))  ! if user inputs 366 on non-leap year
        end if
        
        !! sum years of printing for average annual writes
        if (time%yrs > pco%nyskip) then
          time%yrs_prt = time%yrs_prt + float(time%day_end_yr - time%day_start + 1)
          time%days_prt = time%days_prt + float(time%day_end_yr - time%day_start + 1)
        else
          !! tell user they are skipping more years than simulating
          time%yrs_prt = time%nbyr
        end if
            
        !! set initial soil water for hru, basin and lsu - for checking water balance
        if (pco%sw_init == "n") then
          if (time%yrs > pco%nyskip) then
            call basin_sw_init     !***jga 
            call aqu_pest_output_init
            pco%sw_init = "y"  !! won't reset again
          end if
        end if
          
        do julian_day = time%day_start, time%day_end_yr      !! begin daily loop
          time%day = julian_day
          !! determine month and day of month - time%mo and time%day_mo
          call xmon (time%day, mo, day_mo)
          time%mo = mo
          time%day_mo = day_mo
          
          time%yrc_tot = time%yrc_end - time%yrc_start + 1
          !! Uncomment next three lines for DEBUG version only            
          
          call DATE_AND_TIME (b(1), b(2), b(3), date_time)
          write (*,1234) cal_sim, time%mo, time%day_mo, time%yrc, time%yrs, time%yrc_tot,    &                             
                   date_time(5), date_time(6), date_time(7)
          write (9003,1234) cal_sim, time%mo, time%day_mo, time%yrc, time%yrs, time%yrc_tot, &                             
                   date_time(5), date_time(6), date_time(7) 
         
          !! check for end of month, year and simulation
          time%end_mo = 0
          time%end_yr = 0
          time%end_sim = 0
          if (time%end_aa_prt == 1) then
            time%end_aa_prt = 0
            time%prt_int_cur = 0.
          end if
          if (time%day == ndays(time%mo+1)) then
            time%end_mo = 1
          end if
          if (time%day == time%day_end_yr) then
            time%end_yr = 1
            if (time%yrs == time%nbyr) then
              time%end_sim = 1
              time%yrs_prt = time%yrs_prt / (365. + (time%num_leap / time%nbyr))
            end if
            if (pco%aa_numint > 0) then
              if (time%yrc == pco%aa_yrs(time%prt_int_cur)) then
                time%end_aa_prt = 1
                time%yrs_prt_int = time%yrs_prt_int / (365. + (time%num_leap / time%nbyr))
                time%prt_int_cur = time%prt_int_cur + 1 
              end if
            end if
          end if

          !! check time interval for daily printing
          if (pco%day_print_over == "n") then
          if (pco%day_print == "n") then 
            if (time%day >= pco%day_start .and. time%yrc >= pco%yrc_start) then
              pco%day_print = "y"
            end if 
          else
            if (time%day > pco%day_end .and. time%yrc == pco%yrc_end) then
              pco%day_print = "n"
              pco%day_print_over = "y"
            else
              pco%int_day_cur = pco%int_day_cur + 1
              if (pco%int_day_cur > pco%int_day) pco%int_day_cur = 1
            end if
          end if
          end if

          !! initialize variables at beginning of day for hru's
          if (sp_ob%hru > 0) call sim_initday

          if (time%yrs > pco%nyskip) ndmo(time%mo) = ndmo(time%mo) + 1

          call climate_control      !! read in/generate weather
          
          call cli_atmodep_time_control     !! set array counter for atmospheric deposition

          !! conditional reset of land use and management
          do iupd = 1, db_mx%cond_up
            id = upd_cond(iupd)%cond_num
            d_tbl => dtbl_scen(id)
            !if (upd_cond(iupd)%num_hits < upd_cond(iupd)%max_hits) then
            !  upd_cond(iupd)%num_hits = upd_cond(iupd)%num_hits + 1
              !! all hru fractions are set at once
              if (upd_cond(iupd)%typ == "basin") then
                call conditions (j, id)
                call actions (j, iob, id)
              end if
              !! have to check every hru for land use change
              if (upd_cond(iupd)%typ == "lu_change") then
                do j = 1, sp_ob%hru
                  call conditions (j, id)
                  call actions (j, iob, id)
                end do
              end if
            !end if            
          end do

          !! allocate water for water rights objects
          if (db_mx%wallo_db > 0) then
            do iwallo = 1, db_mx%wallo_db
              !! if a channel is not an object, call at beginning of day
              j = iwallo    ! to avoid a compiler warning
              if (wallo(iwallo)%cha_ob == "n") call wallo_control (j)
            end do
          end if

          !! allocate manure to appropriate demand objects
          if (db_mx%mallo_db > 0) then
            do imallo = 1, db_mx%mallo_db
              call mallo_control (imallo)
            end do
          end if
          
          call command              !! command loop 
          
          ! reset base0 heat units and yr_skip at end of year for southern hemisphere
          ! near winter solstace (winter solstice is around June 22)
          if (time%day == 181) then
            do ihru = 1, sp_ob%hru
              iob = sp_ob1%hru + ihru - 1
              if (ob(iob)%lat < 0) then
                phubase(ihru) = 0.
                yr_skip(ihru) = 0
                isched = hru(ihru)%mgt_ops
                if (isched > 0 .and. sched(isched)%num_ops > 0) then
                  if (sched(isched)%mgt_ops(hru(ihru)%cur_op)%op == "skip") hru(ihru)%cur_op = hru(ihru)%cur_op + 1
                  if (hru(ihru)%cur_op > sched(isched)%num_ops) then
                    hru(ihru)%cur_op = 1
                  end if
                end if
              end if
            end do
          end if

        end do              !! end daily loop

        !! perform end-of-year processes
        
        call calsoft_sum_output
        
        !! write annual basin crop yields and harvested areas
        if (sp_ob%hru > 0) then
        do iplt = 1, basin_plants
          crop_yld_t_ha = bsn_crop_yld(iplt)%yield / (bsn_crop_yld(iplt)%area_ha + 1.e-6)
          write (5100,*) time%yrc, iplt, plants_bsn(iplt), bsn_crop_yld(iplt)%area_ha,            &
                                                bsn_crop_yld(iplt)%yield, crop_yld_t_ha
          bsn_crop_yld_aa(iplt)%area_ha = bsn_crop_yld_aa(iplt)%area_ha + bsn_crop_yld(iplt)%area_ha
          bsn_crop_yld_aa(iplt)%yield = bsn_crop_yld_aa(iplt)%yield + bsn_crop_yld(iplt)%yield
          bsn_crop_yld(iplt) = bsn_crop_yld_z
          if (time%end_sim == 1) then
            crop_yld_t_ha = bsn_crop_yld_aa(iplt)%yield / (bsn_crop_yld_aa(iplt)%area_ha + 1.e-6)
            bsn_crop_yld_aa(iplt)%area_ha = bsn_crop_yld_aa(iplt)%area_ha / time%yrs_prt
            bsn_crop_yld_aa(iplt)%yield = bsn_crop_yld_aa(iplt)%yield / time%yrs_prt
            write (5101,*) time%yrc, iplt, plants_bsn(iplt), bsn_crop_yld_aa(iplt)%area_ha,   &
                                                bsn_crop_yld_aa(iplt)%yield, crop_yld_t_ha
            bsn_crop_yld_aa(iplt) = bsn_crop_yld_z
          end if
        end do
        end if
        
        do j = 1, sp_ob%hru_lte
          !! zero yearly balances after using them in soft data calibration (was in hru_lte_output)
          hltwb_y(j) = hwbz
          hltnb_y(j) = hnbz
          hltpw_y(j) = hpwz
          hltls_y(j) = hlsz
        end do
        
        do ich = 1, sp_ob%chandeg
          !! zero yearly balances after using them in soft data calibration (was in sd_channel_output)
          chsd_y(ich) = chsdz
          ch_stor_y(ich) = chaz
          ch_in_y(ich) = chaz
          ch_out_y(ich) = chaz
          ch_wat_y(ich) = wbodz
        end do
        
        do j = 1, sp_ob%hru
          !! zero yearly balances after using them in soft data calibration (was in hru_output)
          sw_init = hwb_y(j)%sw_final
          sno_init = hwb_y(j)%sno_final
          hwb_y(j) = hwbz
          hwb_y(j)%sw_init = sw_init
          hwb_y(j)%sno_init = sno_init
          hnb_y(j) = hnbz
          hpw_y(j) = hpwz
          hls_y(j) = hlsz
          !! zero yearly pesticide balances after using them in soft data calibration (was in hru_output)
          if (cs_db%num_pests > 0) then
            do ipest = 1, cs_db%num_pests
              hpestb_y(j)%pest(ipest) = pestbz
            end do
          end if
        
          !! compute biological mixing at the end of every year
          if (hru(j)%hyd%biomix > 1.e-6) call mgt_newtillmix (j, hru(j)%hyd%biomix, 0)

          !! update sequence number for year in rotation to that of
          !! the next year and reset sequence numbers for operations
          do ipl = 1, pcom(j)%npl
            idp = pcom(j)%plcur(ipl)%idplt
            if (idp > 0) then
              if (pldb(idp)%typ == "perennial") then
                pcom(j)%plcur(ipl)%curyr_mat = pcom(j)%plcur(ipl)%curyr_mat + 1
                pcom(j)%plcur(ipl)%curyr_mat = Min(pcom(j)%plcur(ipl)%curyr_mat,pldb(idp)%mat_yrs)
              end if
            end if
          end do

          ! reset base0 heat units and yr_skip at end of year for northern hemisphere
          ! on December 31 (winter solstice is around December 22)
          iob = sp_ob1%hru + j - 1
          if (ob(iob)%lat >= 0) then
            phubase(j) = 0.
            yr_skip(j) = 0
            isched = hru(j)%mgt_ops
            if (isched > 0 .and. sched(isched)%num_ops > 0) then
              if (sched(isched)%mgt_ops(hru(j)%cur_op)%op == "skip") hru(j)%cur_op = hru(j)%cur_op + 1
              if (hru(j)%cur_op > sched(isched)%num_ops) then
                hru(j)%cur_op = 1
              end if
            end if
          end if
        end do      

        !! update simulation year
        time%yrc = time%yrc + 1
      end do            !!     end annual loop
      
      do ich = 1, sp_ob%chandeg
        !! write channel morphology - downcutting and widening
        ch_morph(ich)%w_yr = ch_morph(ich)%w_yr / sd_ch(ich)%chw / time%yrs_prt
        ch_morph(ich)%d_yr = ch_morph(ich)%d_yr / sd_ch(ich)%chd / time%yrs_prt
        ch_morph(ich)%fp_mm = ch_morph(ich)%fp_mm / (3. * sd_ch(ich)%chw *           &
                                         sd_ch(ich)%chl * 1000.) / time%yrs_prt
        iob = sp_ob1%chandeg + ich - 1
        !write (7778,*) ich, ob(iob)%name, ch_morph(ich)%w_yr, ch_morph(ich)%d_yr, ch_morph(ich)%fp_mm
      end do
          
      !! ave annual calibration output and reset time for next simulation
      call calsoft_ave_output
      yrs_print = time%yrs_prt
      time = time_init

      return
 1234 format (1x, a, 2i4, 2x,i4,' Yr ', i4,' of ', i4, " Time",2x,i2,":",i2,":",i2)
      
      end subroutine time_control
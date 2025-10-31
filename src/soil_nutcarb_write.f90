      subroutine soil_nutcarb_write(out_freq)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes soil carbon output.
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    out_freq      |              |output frequency (d=daily, m=monthly, y=yearly, a=average annual)

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use soil_module
      use organic_mineral_mass_module
      use hydrograph_module
      use calibration_data_module
      use carbon_module
      use basin_module
      
      implicit none
      
      character(len=2), intent(in) :: out_freq   ! Output frequency (d, m, y, a)
      integer :: ly = 0         !none        |counter
      real :: const = 0.        !none        |counter
      real :: tot_lyr_n
      real :: tot_lyr_p
      real :: tot_prof_n
      real :: tot_prof_p
      real :: lyr_swc           !mm/mm       |layer soil water content including wilting point moisture content.
      real :: prf_swc = 0.0     !mm/mm       |average profile soil water content including wilting point moisture content.
      real :: prf_depth = 0.0   !mm          |depth of soil profile.
      integer :: iihru = 0      !none        |counter
      integer :: j = 0          !none        |counter
      integer :: iob = 0
      integer :: profile_depth
      character (len=7) :: freq_label
      logical :: layer_output
      logical :: print_soil_lyr_depths = .true.
      CHARACTER(LEN=15) :: str

      layer_output = .false.

      select case(out_freq)
        case (" d")
          freq_label = "day"
        case ("dl")
          freq_label = "day"
          layer_output = .true.
        case (" m")
          freq_label = "mon"
        case ("ml")
          freq_label = "mon"
          layer_output = .true.
        case (" y")
          freq_label = "year"
        case ("yl")
          freq_label = "year"
          layer_output = .true.
        case (" b")
          freq_label = "begsim"
        case (" e")
          freq_label = "endsim"
      end select
           
      !! basin output - zero daily basin outputs before summing
      bsn_org_soil = soil_org_z
      bsn_org_pl = soil_org_z
      bsn_org_rsd = soil_org_z

      if (freq_label == "begsim" .or. freq_label == "endsim" ) then
        do j = 1, sp_ob%hru
          iob = sp_ob1%hru + j - 1
          if (freq_label == "begsim") then
            do ly = 1, soil(j)%nly
              write (4586,*) freq_label, soil(j)%snam, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                            !soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c/soil1(j)%tot(ly)%m * 100, &
                            soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c, &
                            soil(j)%phys(ly)%clay, soil(j)%phys(ly)%silt, soil(j)%phys(ly)%sand, soil(j)%phys(ly)%rock, &
                            soil(j)%ly(ly)%alb, soil(j)%ly(ly)%usle_k, soil(j)%ly(ly)%ec, soil(j)%ly(ly)%cal, soil(j)%ly(ly)%ph
            enddo
            if (pco%csvout == "y") then
              do ly = 1, soil(j)%nly
                write (4587,'(*(G0.7,:,","))') freq_label, soil(j)%snam, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                            ! soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c/soil1(j)%tot(ly)%m * 100, &
                            soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c, &
                            soil(j)%phys(ly)%clay, soil(j)%phys(ly)%silt, soil(j)%phys(ly)%sand, soil(j)%phys(ly)%rock, &
                            soil(j)%ly(ly)%alb, soil(j)%ly(ly)%usle_k, soil(j)%ly(ly)%ec, soil(j)%ly(ly)%cal, soil(j)%ly(ly)%ph
              enddo
            endif
          endif
          ! write soil properties at the end of the simulation = "hru_endsim_soil_prop.txt/csv"
          if (freq_label == "endsim") then
            do ly = 1, soil(j)%nly
              write (4584,*) freq_label, soil(j)%snam, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                            ! soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c/soil1(j)%tot(ly)%m * 100, &
                            soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c, &
                            soil(j)%phys(ly)%clay, soil(j)%phys(ly)%silt, soil(j)%phys(ly)%sand, soil(j)%phys(ly)%rock, &
                            soil(j)%ly(ly)%alb, soil(j)%ly(ly)%usle_k, soil(j)%ly(ly)%ec, soil(j)%ly(ly)%cal, soil(j)%ly(ly)%ph
            enddo
            if (pco%csvout == "y") then
              do ly = 1, soil(j)%nly
                write (4585,'(*(G0.7,:,","))') freq_label, soil(j)%snam, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                            ! soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c/soil1(j)%tot(ly)%m * 100, &
                            soil(j)%phys(ly)%bd, soil(j)%phys(ly)%awc, soil(j)%phys(ly)%k, soil1(j)%tot(ly)%c, &
                            soil(j)%phys(ly)%clay, soil(j)%phys(ly)%silt, soil(j)%phys(ly)%sand, soil(j)%phys(ly)%rock, &
                            soil(j)%ly(ly)%alb, soil(j)%ly(ly)%usle_k, soil(j)%ly(ly)%ec, soil(j)%ly(ly)%cal, soil(j)%ly(ly)%ph
              enddo
            endif 
          endif
        enddo
        return
      else
        !! sum the output for the entire soil profile
        do j = 1, sp_ob%hru
          iob = sp_ob1%hru + j - 1
          soil1(j)%tot_org = soil_org_z
          soil1(j)%seq_org = soil_org_z
          soil1(j)%surf_org = soil_org_z
          soil_prof_hact = soil_org_z
          soil_prof_hsta = soil_org_z
          soil_prof_rsd = soil_org_z
          soil_prof_str = soil_org_z
          soil_prof_lig = soil_org_z
          soil_prof_meta = soil_org_z
          soil_prof_man = soil_org_z
          soil_prof_seq_hs = soil_org_z
          soil_prof_seq_hp = soil_org_z
          soil_prof_seq_microb = soil_org_z
          soil_prof_hs = soil_org_z
          soil_prof_hp = soil_org_z
          soil_prof_microb = soil_org_z
          soil_prof_water = soil_org_z
          do ly = 1, soil(j)%nly
            soil_prof_seq_hs = soil_prof_seq_hs + soil1(j)%hs(ly)
            soil_prof_seq_hp = soil_prof_seq_hp + soil1(j)%hp(ly)
            soil_prof_seq_microb = soil_prof_seq_microb + soil1(j)%microb(ly)
            soil_prof_rsd = soil_prof_rsd + soil1(j)%rsd(ly)
            soil_prof_str = soil_prof_str + soil1(j)%str(ly)
            soil_prof_hact = soil_prof_hact + soil1(j)%hact(ly)
            soil_prof_hsta = soil_prof_hsta + soil1(j)%hsta(ly)
            soil_prof_man = soil_prof_man + soil1(j)%man(ly)
            soil_prof_hs = soil_prof_hs + soil1(j)%hs(ly)
            soil_prof_hp = soil_prof_hp + soil1(j)%hp(ly)
            soil_prof_meta = soil_prof_meta + soil1(j)%meta(ly)
            soil_prof_lig = soil_prof_lig +  soil1(j)%lig(ly)
            soil_prof_microb = soil_prof_microb + soil1(j)%microb(ly)
            soil_prof_water = soil_prof_water + soil1(j)%water(ly)
          end do

          ! Total org all layers
          soil1(j)%tot_org = soil_prof_hs + soil_prof_hp + soil_prof_microb + soil_prof_meta &
                            + soil_prof_str + soil_prof_water + soil_prof_man

          ! Sequestered org for soil layers greater than 1
          soil1(j)%seq_org = soil_prof_seq_hs + soil_prof_seq_hp + soil_prof_seq_microb
          
          ! Surface org, just layer 1 excluding residue
          soil1(j)%surf_org = soil1(j)%meta(1) + soil1(j)%str(1) + soil1(j)%microb(1) &
                              + soil1(j)%hs(1) + soil1(j)%man(1) + soil1(j)%water(1)
          
          !write all organic carbon for the plant community file = "hru_plc_stat.txt"
          write (4560,*) freq_label, time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
              pl_mass(j)%tot_com%c, pl_mass(j)%ab_gr_com%c, pl_mass(j)%leaf_com%c,                  &
              pl_mass(j)%stem_com%c, pl_mass(j)%seed_com%c, pl_mass(j)%root_com%c
          ! file = "hru_plc_stat.csv"
          if (pco%csvout == "y") then
              write (4563,'(*(G0.7,:,","))') freq_label, time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
              pl_mass(j)%tot_com%c, pl_mass(j)%ab_gr_com%c, pl_mass(j)%leaf_com%c,                  &
              pl_mass(j)%stem_com%c, pl_mass(j)%seed_com%c, pl_mass(j)%root_com%c
          end if
            
          !write total carbon by soil layer, file = "hru_cbn_lyr.txt"
          ! print header with soil layer depths
          if (print_soil_lyr_depths) then
          write (4548,*)                                     &
            "freq           jday         mon         day        year        unit hru     name           ", (int(soil(j)%phys(ly)%d), "  ", ly = 1, soil(j)%nly)
          endif 

          if (bsn_cc%cswat /= 2) then
            do ly = 1, soil(j)%nly
              soil1(j)%tot(ly)%c = soil1(j)%hact(ly)%c + soil1(j)%hsta(ly)%c + soil1(j)%microb(ly)%c
            end do
          end if
          write (4548,*) freq_label, time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%typ, ob(iob)%name,           &
                                                  (soil1(j)%tot(ly)%c/1000.0, ly = 1, soil(j)%nly)
          if (pco%csvout == "y") then
            if (print_soil_lyr_depths) then
              write (4549,*)                                     &
                  "freq,jday,mon,day,year,unit,hru,name,", (int(soil(j)%phys(ly)%d), ",", ly = 1, soil(j)%nly)
            endif 
            write (4549,'(*(G0.7,:,","))') freq_label, time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%typ, ob(iob)%name,           &
                                                    (soil1(j)%tot(ly)%c/1000.0, ly = 1, soil(j)%nly)
          end if

          !write total sequestered  by soil layer, file = "hru_seq_lyr.txt"
          if (bsn_cc%cswat /= 2) then
            do ly = 1, soil(j)%nly
              soil1(j)%seq(ly)%c = soil1(j)%hact(ly)%c + soil1(j)%hsta(ly)%c + soil1(j)%microb(ly)%c
            end do
          end if
          if (print_soil_lyr_depths) then
          write (4558,*)                                     &
              "freq           jday         mon         day        year        unit hru     name           ", (int(soil(j)%phys(ly)%d), "  ", ly = 1, soil(j)%nly)
          endif 
          write (4558,*) freq_label, time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%typ, ob(iob)%name,           &
                                                  (soil1(j)%seq(ly)%c/1000.0, ly = 1, soil(j)%nly)
          if (pco%csvout == "y") then

            if (print_soil_lyr_depths) then
              write (4559,*)                                     &
                  "freq,jday,mon,day,year,unit,hru,name,", (int(soil(j)%phys(ly)%d), ",", ly = 1, soil(j)%nly)
            endif 
            write (4559,'(*(G0.7,:,","))') freq_label, time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%typ, ob(iob)%name,           &
                                                  (soil1(j)%seq(ly)%c/1000.0, ly = 1, soil(j)%nly)
          end if
          print_soil_lyr_depths = .false.
          
          !write the cswat == 2 related files. 
          if (bsn_cc%cswat == 2) then
            !write all organic carbon for the residue file = "hru_rsdc_stat.txt/csv"
            if (layer_output) then
              profile_depth = int(soil(j)%phys(soil(j)%nly)%d)
              do ly = 1, soil(j)%nly
                write (4561,10) freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    soil1(j)%rsd(1)%c, soil1(j)%meta(1)%c, soil1(j)%str(1)%c, soil1(j)%lig(1)%c,              &
                    soil1(j)%rsd(ly)%c, soil1(j)%meta(ly)%c, soil1(j)%str(ly)%c, soil1(j)%lig(ly)%c
              enddo
            endif
            write (4561,10) freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                soil1(j)%rsd(1)%c, soil1(j)%meta(1)%c, soil1(j)%str(1)%c, soil1(j)%lig(1)%c,              &
                soil_prof_rsd%c, soil_prof_meta%c, soil_prof_str%c, soil_prof_lig%c
            if (pco%csvout == "y") then
              do ly = 1, soil(j)%nly
                write (4564,'(*(G0.7,:,","))') freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    soil1(j)%rsd(1)%c, soil1(j)%meta(1)%c, soil1(j)%str(1)%c, soil1(j)%lig(1)%c,              &
                    soil1(j)%rsd(ly)%c, soil1(j)%meta(ly)%c, soil1(j)%str(ly)%c, soil1(j)%lig(ly)%c
              enddo
              write (4564,'(*(G0.7,:,","))') freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                soil1(j)%rsd(1)%c, soil1(j)%meta(1)%c, soil1(j)%str(1)%c, soil1(j)%lig(1)%c,              &
                soil_prof_rsd%c, soil_prof_meta%c, soil_prof_str%c, soil_prof_lig%c
            end if

            !write sequestered carbon for the soil profile (except layer1), file = "hru_soilc_stat.txt/csv"
            if (layer_output) then
              do ly = 1, soil(j)%nly
                write (4562,*) freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    soil1(j)%seq(ly)%c, soil1(j)%hs(ly)%c, soil1(j)%hp(ly)%c, soil1(j)%microb(ly)%c 
              enddo
            endif
            write (4562,*) freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                soil1(j)%seq_org%c, soil_prof_seq_hs%c, soil_prof_seq_hp%c, soil_prof_seq_microb%c
            if (pco%csvout == "y") then
              if (layer_output) then
                do ly = 1, soil(j)%nly
                  write (4565,'(*(G0.7,:,","))') freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                      soil1(j)%seq(ly)%c, soil1(j)%hs(ly)%c, soil1(j)%hp(ly)%c, soil1(j)%microb(ly)%c 
                enddo
              endif
              write (4565,'(*(G0.7,:,","))') freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                soil1(j)%seq_org%c, soil_prof_seq_hs%c, soil_prof_seq_hp%c, soil_prof_seq_microb%c
            end if

        
            !write organic flux pools for the soil profile file = "hru_cflux_stat.txt" make this non-cumulative
            if (layer_output) then
              do ly = 1, soil(j)%nly
                write (4567,*) freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                              soil1(j)%org_flx_lr(ly)
              enddo
            endif
            write (4567,*) freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                          soil1(j)%org_flx_tot 
            if (pco%csvout == "y") then
              if (layer_output) then
                do ly = 1, soil(j)%nly
                  write (4568,'(*(G0.7,:,","))') freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                soil1(j)%org_flx_lr(ly)
                enddo 
              endif
              write (4568,'(*(G0.7,:,","))') freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                              soil1(j)%org_flx_tot 
            endif 

            !write soil carb mass balance for the soil profile file = "hru_soilcarb_mb_stat.txt/csv"
            if (layer_output) then
              do ly = 1, soil(j)%nly
                write (4570,*) freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                              soil1(j)%tot(ly)%c, soil1(j)%hs(ly)%c, soil1(j)%hp(ly)%c, soil1(j)%microb(ly)%c,            & 
                              soil1(j)%meta(ly)%c, soil1(j)%str(ly)%c, soil1(j)%lig(ly)%c, soil1(j)%man(ly)%c,           &
                              soil1(j)%org_flx_lr(ly)
              enddo
            endif
            write (4570,*) freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                          soil1(j)%tot_org%c, soil_prof_hs%c, soil_prof_hp%c, soil_prof_microb%c,               &
                          soil_prof_meta%c, soil_prof_str%c, soil_prof_lig%c, soil_prof_man%c,                  &
                          soil1(j)%org_flx_tot 
            if (pco%csvout == "y") then
              if (layer_output) then
                do ly = 1, soil(j)%nly
                  write (4571,'(*(G0.7,:,","))') freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                soil1(j)%tot(ly)%c, soil1(j)%hs(ly)%c, soil1(j)%hp(ly)%c, soil1(j)%microb(ly)%c,           & 
                                soil1(j)%meta(ly)%c, soil1(j)%str(ly)%c, soil1(j)%lig(ly)%c, soil1(j)%man(ly)%c,           &
                                soil1(j)%org_flx_lr(ly)
                enddo
              endif
              write (4571,'(*(G0.7,:,","))') freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                          soil1(j)%tot_org%c, soil_prof_hs%c, soil_prof_hp%c, soil_prof_microb%c,               &
                          soil_prof_meta%c, soil_prof_str%c, soil_prof_lig%c, soil_prof_man%c,                  &
                          soil1(j)%org_flx_tot 
            endif 

            !write carbon pool for the soil profile file = "hru_cpool_stat.txt/csv"
            prf_swc = 0.0
            prf_depth = 0.0
            do ly = 1, soil(j)%nly
              prf_swc = prf_swc + soil(j)%phys(ly)%tot_sw
              prf_depth = prf_depth + soil(j)%phys(ly)%thick
            enddo
            prf_swc = prf_swc / prf_depth

            if (layer_output) then
              do ly = 1, soil(j)%nly
                write (4572,*) freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                              soil1(j)%rsd(ly)%c, soil1(j)%str(ly)%c, soil1(j)%meta(ly)%c, soil1(j)%hs(ly)%c, soil1(j)%hp(ly)%c, &
                              soil1(j)%microb(ly)%c, soil1(j)%lig(ly)%c, soil1(j)%water(ly)%c, soil1(j)%man(ly)%c, soil(j)%phys(ly)%tot_sw
              enddo
            endif
            write (4572,*) freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                          soil_prof_rsd%c, soil_prof_str%c, soil_prof_meta%c, soil_prof_hs%c, soil_prof_hp%c,         &
                          soil_prof_microb%c, soil_prof_lig%c, soil_prof_water%c, soil_prof_man%c, prf_swc
            if (pco%csvout == "y") then
              if (layer_output) then
                do ly = 1, soil(j)%nly
                  write (4573,'(*(G0.7,:,","))'), freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                soil1(j)%rsd(ly)%c, soil1(j)%str(ly)%c, soil1(j)%meta(ly)%c, soil1(j)%hs(ly)%c, soil1(j)%hp(ly)%c, &
                                soil1(j)%microb(ly)%c, soil1(j)%lig(ly)%c, soil1(j)%water(ly)%c, soil1(j)%man(ly)%c, soil(j)%phys(ly)%tot_sw
                enddo
              endif
              write (4573,'(*(G0.7,:,","))') freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                            soil_prof_rsd%c, soil_prof_str%c, soil_prof_meta%c, soil_prof_hs%c, soil_prof_hp%c,         &
                            soil_prof_microb%c, soil_prof_lig%c, soil_prof_water%c, soil_prof_man%c, prf_swc
            endif 
            
            !write n and p pool values for the soil profile file = "hru_n_p_pool_stat.txt/csv"
            if (layer_output) then
              do ly = 1, soil(j)%nly
                tot_lyr_n = soil1(j)%rsd(ly)%n + soil1(j)%str(ly)%n + soil1(j)%meta(ly)%n + soil1(j)%hs(ly)%n + soil1(j)%hp(ly)%n + &
                              soil1(j)%microb(ly)%n + soil1(j)%lig(ly)%n + soil1(j)%water(ly)%n + soil1(j)%man(ly)%n 
                tot_lyr_p = soil1(j)%rsd(ly)%p + soil1(j)%str(ly)%p + soil1(j)%meta(ly)%p + soil1(j)%hs(ly)%p + soil1(j)%hp(ly)%p + &
                              soil1(j)%microb(ly)%p + soil1(j)%lig(ly)%p + soil1(j)%water(ly)%p + soil1(j)%man(ly)%p
                write (4582,*) freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                              tot_lyr_n, soil1(j)%rsd(ly)%n, soil1(j)%str(ly)%n, soil1(j)%meta(ly)%n, soil1(j)%hs(ly)%n, soil1(j)%hp(ly)%n, &
                              soil1(j)%microb(ly)%n, soil1(j)%lig(ly)%n, soil1(j)%water(ly)%n, soil1(j)%man(ly)%n, &
                              tot_lyr_p, soil1(j)%rsd(ly)%p, soil1(j)%str(ly)%p, soil1(j)%meta(ly)%p, soil1(j)%hs(ly)%p, soil1(j)%hp(ly)%p, &
                              soil1(j)%microb(ly)%p, soil1(j)%lig(ly)%p, soil1(j)%water(ly)%p, soil1(j)%man(ly)%p
                if (pco%csvout == "y") then
                  write (4583,'(*(G0.7,:,","))'), freq_label, ly, int(soil(j)%phys(ly)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                tot_lyr_n, soil1(j)%rsd(ly)%n, soil1(j)%str(ly)%n, soil1(j)%meta(ly)%n, soil1(j)%hs(ly)%n, soil1(j)%hp(ly)%n, &
                                soil1(j)%microb(ly)%n, soil1(j)%lig(ly)%n, soil1(j)%water(ly)%n, soil1(j)%man(ly)%n,  &
                                tot_lyr_p, soil1(j)%rsd(ly)%p, soil1(j)%str(ly)%p, soil1(j)%meta(ly)%p, soil1(j)%hs(ly)%p, soil1(j)%hp(ly)%p, &
                                soil1(j)%microb(ly)%p, soil1(j)%lig(ly)%p, soil1(j)%water(ly)%p, soil1(j)%man(ly)%p
                endif
              enddo
            endif
            tot_prof_n = soil_prof_rsd%n + soil_prof_str%n + soil_prof_meta%n + soil_prof_hs%n + soil_prof_hp%n +         &
                          soil_prof_microb%n + soil_prof_lig%n + soil_prof_water%n + soil_prof_man%n
            tot_prof_p = soil_prof_rsd%p + soil_prof_str%p + soil_prof_meta%p + soil_prof_hs%p + soil_prof_hp%p +         &
                          soil_prof_microb%p + soil_prof_lig%p + soil_prof_water%p + soil_prof_man%p
            write (4582,*) freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                          tot_prof_n, soil_prof_rsd%n, soil_prof_str%n, soil_prof_meta%n, soil_prof_hs%n, soil_prof_hp%n,         &
                          soil_prof_microb%n, soil_prof_lig%n, soil_prof_water%n, soil_prof_man%n, &
                          tot_prof_n, soil_prof_rsd%p, soil_prof_str%p, soil_prof_meta%p, soil_prof_hs%p, soil_prof_hp%p,         &
                          soil_prof_microb%p, soil_prof_lig%p, soil_prof_water%p, soil_prof_man%p
            if (pco%csvout == "y") then
              write (4583,'(*(G0.7,:,","))') freq_label, -1, int(profile_depth), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                            tot_prof_n, soil_prof_rsd%n, soil_prof_str%n, soil_prof_meta%n, soil_prof_hs%n, soil_prof_hp%n,         &
                            soil_prof_microb%n, soil_prof_lig%n, soil_prof_water%n, soil_prof_man%n, &
                            soil_prof_rsd%p, soil_prof_str%p, soil_prof_meta%p, soil_prof_hs%p, soil_prof_hp%p,         &
                            tot_prof_p, soil_prof_microb%p, soil_prof_lig%p, soil_prof_water%p, soil_prof_man%p
            endif 
          end if


        end do    !! hru loop
        
        !! summing hru output for the basin
        do j = 1, sp_ob%hru
          iihru = lsu_elem(j)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hru") then
              bsn_org_soil = bsn_org_soil + const * soil1(iihru)%tot_org
              bsn_org_pl = bsn_org_pl + const * pl_mass(iihru)%tot_com
              bsn_org_rsd = bsn_org_rsd + const * soil1(iihru)%rsd(1)
              bsn_mn = bsn_mn + const * soil1(iihru)%tot_mn
              bsn_mp = bsn_mp + const * soil1(iihru)%tot_mp
            end if
          end if
        end do
                
        !! write all carbon, organic n and p, and mineral n and p for the soil profile, plants, and residue
        write (4566,*) time%day, time%yrc, ' basin ', bsn_org_soil%c, bsn_org_pl%c, bsn_org_rsd%c

      endif      

      10 FORMAT(A5,8I5,1X,A15,8F15.7)
      return

      end subroutine soil_nutcarb_write

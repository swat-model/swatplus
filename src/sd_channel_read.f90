      subroutine sd_channel_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use channel_velocity_module
      use ch_pesticide_module
      use ch_salt_module !rtb salt
      use ch_cs_module !rtb cs
      use sd_channel_module
      use hydrograph_module
      use constituent_mass_module
      use pesticide_data_module
      use pathogen_data_module
      use water_body_module

      implicit none
      
      character (len=80) :: titldum     !          |title of file
      character (len=80) :: header      !          |header of file
      integer :: eof                    !          |end of file
      integer :: imax                   !units     |description
      logical :: i_exist                !          |check to determine if file exists
      integer :: ichi                   !none      |counter
      integer :: isp_ini                !          |counter
      integer :: ics                    !none      |counter
      integer :: inut                   !none      |counter 
      integer :: ihydsed                !none      |counter
      integer :: i                      !none      |counter
      integer :: k                      !none      |counter
      integer :: isalt                  !none      |counter for salt ion

      eof = 0
      imax = 0
            
      !! allocate sd channel variables
      allocate (gully(0:0))
      allocate (sd_ch(0:sp_ob%chandeg))
      allocate (ch_morph(0:sp_ob%chandeg))
      allocate (ch_sed_bud(0:sp_ob%chandeg))
      allocate (ch_sed_bud_m(0:sp_ob%chandeg))
      allocate (ch_sed_bud_y(0:sp_ob%chandeg))
      allocate (ch_sed_bud_a(0:sp_ob%chandeg))
      allocate (ch_rcurv(0:sp_ob%chandeg))
      allocate (sd_ch_vel(0:sp_ob%chandeg))
      allocate (chsd_d(0:sp_ob%chandeg))
      allocate (chsd_m(0:sp_ob%chandeg))
      allocate (chsd_y(0:sp_ob%chandeg))
      allocate (chsd_a(0:sp_ob%chandeg))
      allocate (ch_water(0:sp_ob%chandeg))
      allocate (ch_benthic(0:sp_ob%chandeg))
      allocate (ch_stor(0:sp_ob%chandeg))
      allocate (fp_stor(0:sp_ob%chandeg))
      allocate (tot_stor(0:sp_ob%chandeg))
      allocate (wet_stor(0:sp_ob%chandeg))
      allocate (ch_fp_wb(0:sp_ob%chandeg))
      allocate (ch_stor_m(0:sp_ob%chandeg))
      allocate (ch_stor_y(0:sp_ob%chandeg))
      allocate (ch_stor_a(0:sp_ob%chandeg))
      allocate (ch_wat_d(0:sp_ob%chandeg))
      allocate (ch_wat_m(0:sp_ob%chandeg))
      allocate (ch_wat_y(0:sp_ob%chandeg))
      allocate (ch_wat_a(0:sp_ob%chandeg))
      allocate (ch_in_d(0:sp_ob%chandeg))
      allocate (ch_in_m(0:sp_ob%chandeg))
      allocate (ch_in_y(0:sp_ob%chandeg))
      allocate (ch_in_a(0:sp_ob%chandeg))
      allocate (ch_out_d(0:sp_ob%chandeg))
      allocate (ch_out_m(0:sp_ob%chandeg))
      allocate (ch_out_y(0:sp_ob%chandeg))
      allocate (ch_out_a(0:sp_ob%chandeg))
      allocate (ch_om_water_init(0:sp_ob%chandeg))
      allocate (fp_om_water_init(0:sp_ob%chandeg))
      allocate (chpst_d(0:sp_ob%chandeg))
      allocate (chpst_m(0:sp_ob%chandeg))
      allocate (chpst_y(0:sp_ob%chandeg))
      allocate (chpst_a(0:sp_ob%chandeg))
      allocate (chsalt_d(0:sp_ob%chandeg)) !rtb salt
      allocate (chsalt_m(0:sp_ob%chandeg))
      allocate (chsalt_y(0:sp_ob%chandeg))
      allocate (chsalt_a(0:sp_ob%chandeg))
      allocate (chcs_d(0:sp_ob%chandeg)) !rtb cs
      allocate (chcs_m(0:sp_ob%chandeg))
      allocate (chcs_y(0:sp_ob%chandeg))
      allocate (chcs_a(0:sp_ob%chandeg))
      
      !rtb hydrograph separation
      allocate (ch_stor_hdsep(sp_ob%chandeg))
      allocate (hyd_sep_array(sp_ob%chandeg,7))
      hyd_sep_array = 0.
      
      if (cs_db%num_pests > 0) then
        allocate (chpst%pest(cs_db%num_pests))
        allocate (chpstz%pest(cs_db%num_pests))
        allocate (bchpst_d%pest(cs_db%num_pests))
        allocate (bchpst_m%pest(cs_db%num_pests))
        allocate (bchpst_y%pest(cs_db%num_pests))
        allocate (bchpst_a%pest(cs_db%num_pests))
        do ich = 1, sp_ob%chandeg
          allocate (sd_ch(ich)%aq_mix(cs_db%num_pests))
          allocate (chpst_d(ich)%pest(cs_db%num_pests))
          allocate (chpst_m(ich)%pest(cs_db%num_pests))
          allocate (chpst_y(ich)%pest(cs_db%num_pests))   
          allocate (chpst_a(ich)%pest(cs_db%num_pests))
          allocate (ch_water(ich)%pest(cs_db%num_pests))
          allocate (ch_benthic(ich)%pest(cs_db%num_pests))
        end do
      end if
            
      if (cs_db%num_paths > 0) then
        do ich = 1, sp_ob%chandeg
          allocate (ch_water(ich)%path(cs_db%num_paths))
          allocate (ch_benthic(ich)%path(cs_db%num_paths))
        end do
      end if
                  
      if (cs_db%num_metals > 0) then
        do ich = 1, sp_ob%chandeg
          allocate (ch_water(ich)%hmet(cs_db%num_metals))
          allocate (ch_benthic(ich)%hmet(cs_db%num_metals))
        end do
      end if
                  
      if (cs_db%num_salts > 0) then !rtb salt
        do ich = 1, sp_ob%chandeg
          allocate (chsalt_d(ich)%salt(cs_db%num_salts))
          allocate (chsalt_m(ich)%salt(cs_db%num_salts))
          allocate (chsalt_y(ich)%salt(cs_db%num_salts))   
          allocate (chsalt_a(ich)%salt(cs_db%num_salts))
          allocate (ch_water(ich)%salt(cs_db%num_salts))
          allocate (ch_water(ich)%saltc(cs_db%num_salts))
          allocate (ch_benthic(ich)%salt(cs_db%num_salts))
          do isalt=1,cs_db%num_salts
            chsalt_m(ich)%salt(isalt)%tot_in = 0.
            chsalt_m(ich)%salt(isalt)%tot_out = 0.
            chsalt_m(ich)%salt(isalt)%irr = 0.
            chsalt_m(ich)%salt(isalt)%water = 0.
            chsalt_m(ich)%salt(isalt)%conc = 0.
            chsalt_y(ich)%salt(isalt)%tot_in = 0.
            chsalt_y(ich)%salt(isalt)%tot_out = 0.
            chsalt_y(ich)%salt(isalt)%irr = 0.
            chsalt_y(ich)%salt(isalt)%water = 0.
            chsalt_y(ich)%salt(isalt)%conc = 0.
            chsalt_a(ich)%salt(isalt)%tot_in = 0.
            chsalt_a(ich)%salt(isalt)%tot_out = 0.
            chsalt_a(ich)%salt(isalt)%irr = 0.
            chsalt_a(ich)%salt(isalt)%water = 0.
            chsalt_a(ich)%salt(isalt)%conc = 0.
					enddo
          ch_water(ich)%salt = 0.
          ch_water(ich)%saltc = 0.
        end do
      end if

      if (cs_db%num_cs > 0) then !rtb cs
        do ich = 1, sp_ob%chandeg
          allocate (chcs_d(ich)%cs(cs_db%num_cs))
          allocate (chcs_m(ich)%cs(cs_db%num_cs))
          allocate (chcs_y(ich)%cs(cs_db%num_cs))   
          allocate (chcs_a(ich)%cs(cs_db%num_cs))
          allocate (ch_water(ich)%cs(cs_db%num_cs))
          allocate (ch_water(ich)%csc(cs_db%num_cs))
          allocate (ch_benthic(ich)%cs(cs_db%num_cs))
          do ics=1,cs_db%num_cs
            chcs_m(ich)%cs(ics)%tot_in = 0.
            chcs_m(ich)%cs(ics)%tot_out = 0.
            chcs_m(ich)%cs(ics)%irr = 0.
            chcs_m(ich)%cs(ics)%water = 0.
            chcs_m(ich)%cs(ics)%conc = 0.
            chcs_y(ich)%cs(ics)%tot_in = 0.
            chcs_y(ich)%cs(ics)%tot_out = 0.
            chcs_y(ich)%cs(ics)%irr = 0.
            chcs_y(ich)%cs(ics)%water = 0.
            chcs_y(ich)%cs(ics)%conc = 0.
            chcs_a(ich)%cs(ics)%tot_in = 0.
            chcs_a(ich)%cs(ics)%tot_out = 0.
            chcs_a(ich)%cs(ics)%irr = 0.
            chcs_a(ich)%cs(ics)%water = 0.
            chcs_a(ich)%cs(ics)%conc = 0.
					enddo
          ch_water(ich)%cs = 0.
          ch_water(ich)%csc = 0.
        end do
      end if

      inquire (file=in_cha%chan_ez, exist=i_exist)
      if (.not. i_exist .or. in_cha%chan_ez == "null") then
        allocate (sd_dat(0:0))
      else   
      do
       open (105,file=in_cha%chan_ez)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
        
      db_mx%sdc_dat = imax

      allocate (sd_dat(0:imax))
      
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
      
      do ichi = 1, db_mx%sdc_dat
         read (105,*,iostat=eof) i
         if (eof < 0) exit
         backspace (105)
         read (105,*,iostat=eof) k, sd_dat(i)%name, sd_dat(i)%initc, sd_dat(i)%hydc, sd_dat(i)%sedc, &
            sd_dat(i)%nutc
         if (eof < 0) exit
         
        !! initialize orgaincs and minerals in water
        do isp_ini = 1, db_mx%ch_init
          if (sd_dat(ichi)%initc == ch_init(isp_ini)%name) then
            sd_dat(ichi)%init = isp_ini
            !! initial organic mineral
            do ics = 1, db_mx%om_water_init
              if (ch_init(isp_ini)%org_min == om_init_name(ics)) then
                sd_init(isp_ini)%org_min = ics
                exit
              end if
            end do
            !! initial pesticides
            do ics = 1, db_mx%pestw_ini
              if (ch_init(isp_ini)%pest == pest_init_name(ics)) then
                sd_init(isp_ini)%pest = ics
                exit
              end if
            end do
            !! initial pathogens
            do ics = 1, db_mx%pathw_ini
              if (ch_init_cs(isp_ini)%path == path_init_name(ics)) then
                sd_init(isp_ini)%path = ics
                exit
              end if
            end do
!            !! initial heavy metals
!            do ics = 1, db_mx%hmetw_ini
!              if (ch_init(isp_ini)%hmetc == ch_hmet_init(ics)%name) then
!                sd_init(isp_ini)%hmet = ics
!                exit
!              end if
!            end do
             !! initial salts
             do ics=1,db_mx%salt_cha_ini
               if (ch_init_cs(isp_ini)%salt == salt_cha_ini(ics)%name) then
                 sd_init(isp_ini)%salt = ics
                 exit
               end if
             end do
             !! initial constituents !rtb cs
             do ics=1,db_mx%cs_cha_ini
               if (ch_init_cs(isp_ini)%cs == cs_cha_ini(ics)%name) then
                 sd_init(isp_ini)%cs = ics
                 exit
               end if
             end do
          end if
        end do
        
          !! set hydraulic and sediment input data
          do ihydsed = 1, db_mx%ch_lte
            if (sd_chd(ihydsed)%name == sd_dat(ichi)%hydc) then
              sd_dat(ichi)%hyd = ihydsed
              exit
            end if
          end do
          
          !! set new nutrient and sediment input data
          do ihydsed = 1, db_mx%ch_sednut
            if (sd_chd1(ihydsed)%name == sd_dat(ichi)%hydc) then
              sd_dat(ichi)%sednut = ihydsed
              exit
            end if
          end do
          
          do inut = 1, db_mx%ch_nut
            if (ch_nut(inut)%name == sd_dat(ichi)%nutc) then
              sd_dat(ichi)%nut = inut
              exit
            end if
          end do   

      end do
      close (105)
      exit
      end do
      
      end if

      return    
      end subroutine sd_channel_read
      subroutine sd_hydsed_init
      
      use input_file_module
      use sd_channel_module
      use channel_velocity_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      use pesticide_data_module
      use basin_module
      
      implicit none      

      real :: kh
      integer :: idb, idb1              !             |
      integer :: i                      !none         |counter  
      integer :: iob, ichdat
      integer :: ich_ini                !none      |counter
      integer :: iom_ini                !none      |counter
      integer :: ipest_ini              !none      |counter
      integer :: ipest_db               !none      |counter
      integer :: ipath_ini              !none      |counter
      integer :: isalt_ini							!none      |counter
      integer :: ics_ini                !none      |counter
      integer :: ipest                  !none      |counter
      integer :: ipath                  !none      |counter
      integer :: idat
      integer :: i_dep                  !none      |counter
      integer :: icha
      integer :: isalt
      integer :: ics
      
      real :: aa                      !none         |area/area=1 (used to calculate velocity with
                                      !             |Manning"s equation)
      real :: a                       !m^2          |cross-sectional area of channel
      real :: b                       !m            |bottom width of channel
      real :: d                       !m            |depth of flow
      real :: p                       !m            |wetting perimeter
      real :: chside                  !none         |change in horizontal distance per unit
                                      !             |change in vertical distance on channel side
                                      !             |slopes; always set to 2 (slope=1/2)
      real :: fps                     !none         |change in horizontal distance per unit
                                      !             |change in vertical distance on floodplain side
                                      !             |slopes; always set to 4 (slope=1/4)
      integer :: max                  !             |
      real :: rh                      !m            |hydraulic radius
      real :: qman                    !m^3/s or m/s |flow rate or flow velocity
      real :: bedvol                  !m^3          |volume of river bed sediment
      
      real :: dep                     !             |
      real :: vel                     !             |
      real :: flow_dep
      real :: celerity
      real :: msk1         !units             |description 
      real :: msk2         !units             |description 
      real :: detmax       !units             |description 
      real :: xkm          !hr                |storage time constant for the reach
      real :: det          !hr                |time step
      real :: denom        !none              |variable to hold intermediate calculation
      real :: rto          !none              |ratio of channel volume to total volume
      real :: rto1         !none              |ratio of flood plain volume to total volume
      real :: sumc         !none              |sum of Muskingum coefficients
      
      do i = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + i - 1
        idat = ob(icmd)%props
        idb = sd_dat(idat)%hyd
        idb1 = sd_dat(idat)%sednut
        sd_ch(i)%name = sd_chd(idb)%name
        sd_ch(i)%obj_no = icmd
        sd_ch(i)%order = sd_chd(idb)%order
        sd_ch(i)%chw = sd_chd(idb)%chw
        sd_ch(i)%chd = sd_chd(idb)%chd
        sd_ch(i)%chs = sd_chd(idb)%chs
        if (sd_ch(i)%chs < 1.e-9) sd_ch(i)%chs = .000001
        sd_ch(i)%chl = sd_chd(idb)%chl
        sd_ch(i)%chn = sd_chd(idb)%chn
        if (sd_ch(i)%chn < .05) sd_ch(i)%chn = .05   !***jga
        sd_ch(i)%chk = sd_chd(idb)%chk      
        sd_ch(i)%cherod = sd_chd(idb)%cherod
        sd_ch(i)%cov = sd_chd(idb)%cov
        sd_ch(i)%sinu = sd_chd(idb)%sinu
        if (sd_ch(i)%sinu < 1.05) sd_ch(i)%sinu = 1.05
        sd_ch(i)%chseq = sd_chd(idb)%chseq
        if (sd_ch(i)%chseq < 1.e-6) sd_ch(i)%chseq = 0.5
        sd_ch(i)%d50 = sd_chd(idb)%d50
        sd_ch(i)%ch_clay = sd_chd(idb)%ch_clay
        sd_ch(i)%carbon = sd_chd(idb)%carbon
        sd_ch(i)%ch_bd = sd_chd(idb)%ch_bd
        sd_ch(i)%chss = sd_chd(idb)%chss
        sd_ch(i)%n_conc = sd_chd(idb)%n_conc
        sd_ch(i)%p_conc = sd_chd(idb)%p_conc
        sd_ch(i)%p_bio = sd_chd(idb)%p_bio
        sd_ch(i)%bankfull_flo = sd_chd(idb)%bankfull_flo
        if (sd_ch(i)%bankfull_flo <= 1.e-6) sd_ch(i)%bankfull_flo = 0.
        sd_ch(i)%fps = sd_chd(idb)%fps
        if (sd_ch(i)%fps > sd_ch(i)%chs) sd_ch(i)%fps = sd_ch(i)%chs
        if (sd_ch(i)%fps <= 1.e-6) sd_ch(i)%fps = .00001       !!! nbs 1/24/22
        sd_ch(i)%fpn = sd_chd(idb)%fpn
        sd_ch(i)%hc_kh = gully(0)%hc_kh
        sd_ch(i)%hc_hgt = gully(0)%hc_hgt
        sd_ch(i)%hc_ini = gully(0)%hc_ini
        sd_ch(i)%pk_rto = sd_chd1(idb1)%pk_rto
        sd_ch(i)%fp_inun_days = sd_chd1(idb1)%fp_inun_days
        sd_ch(i)%n_setl = sd_chd1(idb1)%n_setl
        sd_ch(i)%p_setl = sd_chd1(idb1)%p_setl
        sd_ch(i)%n_sol_part = sd_chd1(idb1)%n_sol_part
        sd_ch(i)%p_sol_part = sd_chd1(idb1)%p_sol_part
        sd_ch(i)%n_dep_enr = sd_chd1(idb1)%n_dep_enr
        sd_ch(i)%p_dep_enr = sd_chd1(idb1)%p_dep_enr
        sd_ch(i)%arc_len_fr = sd_chd1(idb1)%arc_len_fr
        sd_ch(i)%part_size = sd_chd1(idb1)%part_size
        sd_ch(i)%wash_bed_fr = sd_chd1(idb1)%wash_bed_fr
          
        !! compute headcut parameters
        kh = sd_ch(i)%hc_kh
        if (kh > 1.e-6) then
          sd_ch(i)%hc_co = .37 * (17.83 + 16.56 * kh - 15. * sd_ch(i)%cov)
          sd_ch(i)%hc_co = max (0., sd_ch(i)%hc_co)
        else
          sd_ch(i)%hc_co = 0.
        end if

        !! compute travel time coefficients - delete when finished with flood plain
        aa = 1.
        b = 0.
        d = 0.
        chside = sd_ch(i)%chss
        fps = 4.
        b = sd_ch(i)%chw - 2. * sd_ch(i)%chd * chside

        !! check IF bottom width (b) is < 0
        if (b <= 0.) then
            b = .5 * sd_ch(i)%chw
            b = Max(0., b)
            chside = (sd_ch(i)%chw - b) / (2. * sd_ch(i)%chd)
        end if
        sd_ch_vel(i)%wid_btm = b
        sd_ch_vel(i)%dep_bf = sd_ch(i)%chd  !delete sd_ch_vel when finished
        !! compute travel time coefficients - delete when finished with flood plain

        !! compute rating curve
        call sd_rating_curve (i)
        
        !! set Muskingum parameters
        !! compute storage discharge for Muskingum at 0.1 and 1.0 times bankfull depth
      do i_dep = 1, 2
        if (i_dep == 1) dep = 0.1 * sd_ch(i)%chd
        if (i_dep == 2) dep = sd_ch(i)%chd
        !! c^2=a^2+b^2 - a=dep; a/b=slope; b^2=a^2/slope^2
        p = b + 2. * Sqrt(dep ** 2 * (1. + 1. / (sd_ch(i)%chss ** 2)))
        a = b * dep + dep / sd_ch(i)%chss
        rh = a / p
        vel = Qman(1., rh, sd_ch(i)%chn, sd_ch(i)%chs)
        celerity = vel * 5. / 3.
        if (i_dep == 1) then
          !! 0.1*bankfull storage discharge coef
          sd_ch(i)%stor_dis_01bf = sd_ch(i)%chl / (3.6 * celerity)
        else
          !! bankfull storage discharge coef
          sd_ch(i)%stor_dis_bf = sd_ch(i)%chl / (3.6 * celerity)
        end if
      end do
        
      !! Compute storage time constant for reach (msk_co1 + msk_co2 = 1.)
	  msk1 = bsn_prm%msk_co1 / (bsn_prm%msk_co1 + bsn_prm%msk_co2)
	  msk2 = bsn_prm%msk_co2 / (bsn_prm%msk_co1 + bsn_prm%msk_co2)
      xkm = sd_ch(i)%stor_dis_bf * msk1 + sd_ch(i)%stor_dis_01bf * msk2
      
      !! Muskingum numerical stability -Jaehak Jeong, 2011
      detmax = 2.* xkm * (1.- bsn_prm%msk_x)
      det = time%dtm / 60.      !hours
      sd_ch(i)%msk%substeps = 1
      
      !! Discretize time interval to meet the stability criterion 
      if (det > detmax) then
        sd_ch(i)%msk%substeps = Int(det / detmax) + 1
      end if
      if (bsn_cc%rte == 0 .and. time%step <= 1) then
        sd_ch(i)%msk%substeps = 1
      end if
      sd_ch(i)%msk%nsteps = time%step * sd_ch(i)%msk%substeps
              
        !! intial inflow-outflow
        if (sd_ch(i)%msk%nsteps > 0) then
          sd_ch(i)%in1_vol = rcurv%flo_rate / (86400. / sd_ch(i)%msk%nsteps)
          sd_ch(i)%out1_vol = rcurv%flo_rate / (86400. / sd_ch(i)%msk%nsteps)
        end if
        
        !! compute coefficients
        det = det / sd_ch(i)%msk%substeps
        denom = 2. * xkm * (1. - bsn_prm%msk_x) + det
        sd_ch(i)%msk%c1 = (det - 2. * xkm * bsn_prm%msk_x) / denom
        sd_ch(i)%msk%c1 = Max(0., sd_ch(i)%msk%c1)
        sd_ch(i)%msk%c2 = (det + 2. * xkm * bsn_prm%msk_x) / denom
        sd_ch(i)%msk%c3 = (2. * xkm * (1. - bsn_prm%msk_x) - det) / denom
        !! c1+c2+c3 must equal 1
        sumc = sd_ch(i)%msk%c1 + sd_ch(i)%msk%c2 + sd_ch(i)%msk%c3
        sd_ch(i)%msk%c1 = sd_ch(i)%msk%c1 / sumc
        sd_ch(i)%msk%c2 = sd_ch(i)%msk%c2 / sumc
        sd_ch(i)%msk%c3 = sd_ch(i)%msk%c3 / sumc

      end do    !end of channel loop
 
      ! initialize organics-minerals in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        ! only initialize storage for real channels (length > 1 m)
        if (sd_ch(ich)%chl > 1.e-3) then
          iob = sp_ob1%chandeg + ich - 1
          ichdat = ob(iob)%props
          ich_ini = sd_dat(ichdat)%init
          iom_ini = sd_init(ich_ini)%org_min
          tot_stor(ich) = om_init_water(iom_ini)
                    
          !! intialize rating curves - inflow and outflow at current time step
          flow_dep = om_init_water(iom_ini)%flo * sd_ch(ich)%chd
          icha = ich
          call rcurv_interp_dep (icha, flow_dep)
          ch_rcurv(ich)%in1 = rcurv
          ch_rcurv(ich)%out1 = rcurv
          
          !! initial volume is frac of flow depth - frac*m*m*km*1000. = m3
          !tot_stor(ich)%flo = om_init_water(iom_ini)%flo * sd_ch(ich)%chd * sd_ch(ich)%chw * sd_ch(ich)%chl * 1000.
          tot_stor(ich)%flo = rcurv%vol
          
          !! convert concentration to mass
          call hyd_convert_conc_to_mass (tot_stor(ich))
          
          !! partition water between channel and flood plain
          if (om_init_water(iom_ini)%flo <= 1.0) then
            !! depth below bankfull
            ch_stor(ich) = tot_stor(ich)
            fp_stor(ich) = hz
          else
            !! depth above bankfull
            rto = rcurv%vol_ch / rcurv%vol
            ch_stor(ich) = rto * tot_stor(ich)
            rto1 = 1. - rto
            fp_stor(ich) = rto1 * tot_stor(ich)
          end if
        else
          ch_stor(ich) = hz
          fp_stor(ich) = hz
        end if
        !! save initial water if calibrating and rerunning
        ch_om_water_init(ich) = ch_stor(ich)
        fp_om_water_init(ich) = fp_stor(ich)
      end do
      
      ! initialize pesticides in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + ich - 1
        ichdat = ob(iob)%props
        ich_ini = sd_dat(ichdat)%init
        ipest_ini = sd_init(ich_ini)%pest
        do ipest = 1, cs_db%num_pests
          ipest_db = cs_db%pest_num(ipest)
          ! mg = mg/kg * m3*1000. (kg=m3*1000.)
          ch_water(ich)%pest(ipest) = pest_water_ini(ipest_ini)%water(ipest) * ch_stor(ich)%flo * 1000.
          !! calculate volume of active river bed sediment layer - m3
          bedvol = sd_ch(ich)%chw *sd_ch(ich)%chl * 1000.* pestdb(ipest_ini)%ben_act_dep
          ch_benthic(ich)%pest(ipest) = pest_water_ini(ipest_ini)%benthic(ipest) * bedvol * 1000.   ! mg = mg/kg * m3*1000.
          !! calculate mixing velocity using molecular weight and porosity
          sd_ch(ich)%aq_mix(ipest) = pestdb(ipest_db)%mol_wt ** (-.6666) * (1. - sd_ch(ich)%ch_bd / 2.65) * (69.35 / 365)
        end do
      end do
      
      ! initialize pathogens in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + ich - 1
        ichdat = ob(iob)%props
        ich_ini = sd_dat(ichdat)%init
        ipath_ini = sd_init(ich_ini)%path
        do ipath = 1, cs_db%num_paths
          ch_water(ich)%path(ipath) = path_water_ini(ipest_ini)%water(ipath)
          ch_benthic(ich)%path(ipath) = path_water_ini(ipest_ini)%benthic(ipath)
        end do
      end do
      
      !initial salt ion concentration (g/m3) in channel water, from input data (salt_channel.ini)
      if(cs_db%num_salts > 0) then
        do ich=1,sp_ob%chandeg
          iob = sp_ob1%chandeg + ich - 1
          ichdat = ob(iob)%props
          ich_ini = sd_dat(ichdat)%init
          isalt_ini = sd_init(ich_ini)%salt
          do isalt=1,cs_db%num_salts
            ch_water(ich)%saltc(isalt) = salt_cha_ini(isalt_ini)%conc(isalt) !g/m3
            ch_water(ich)%salt(isalt) = (salt_cha_ini(isalt_ini)%conc(isalt)/1000.) * tot_stor(ich)%flo !kg
          enddo
        enddo
      endif

      !initial constituent concentration (g/m3) in channel water, from input data (cs_channel.ini)
      if(cs_db%num_cs > 0) then
        do ich=1,sp_ob%chandeg
          iob = sp_ob1%chandeg + ich - 1
          ichdat = ob(iob)%props
          ich_ini = sd_dat(ichdat)%init
          ics_ini = sd_init(ich_ini)%cs
          do ics=1,cs_db%num_cs
            ch_water(ich)%csc(ics) = cs_cha_ini(ics_ini)%conc(ics)
            ch_water(ich)%cs(ics) = (cs_cha_ini(ics_ini)%conc(ics)/1000.) * tot_stor(ich)%flo !kg
          enddo
        enddo
			endif
      
      return
      end subroutine sd_hydsed_init
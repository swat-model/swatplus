      subroutine plant_init (init, iihru)

      use hru_module, only : cvm_com, hru, ipl, rsdco_plcom
      use soil_module
      use plant_module
      use hydrograph_module
      use climate_module, only : wst, wgn, wgn_pms
      use time_module
!      use hru_lte_module
      use maximum_data_module
      use plant_data_module
      use landuse_data_module
      use mgt_operations_module
      use urban_data_module
      use conditional_module
      use organic_mineral_mass_module
      
      implicit none
      
      integer, intent (in) :: init   !           |
      integer, intent (in) :: iihru  !none       |hru number to send to plant_init
      integer :: day_mo
      integer :: icom                !           |plant community counter
      integer :: idp                 !           |
      integer :: j                   !none       |counter
      integer :: iob                 !           |spatial object number
      integer :: iwgn                !           |weather generator number
      integer :: mo                  !none       |counter 
      integer :: iday                !none       |counter 
      integer :: icp                 !none       |counter 
      integer :: ilum                !none       |counter 
      integer :: idb                 !none       |counter 
      integer :: isched              !           |
      integer :: iop                 !none       |management operation counter 
      integer :: irot                !none       |rotation year counter 
      integer :: igrow               !none       |julian day growth begins
      integer :: iday_sum            !none       |day for southern hemisphere (182-181)
      integer :: iday_sh             !none       |julian day growth begins in souther hemisphere
      integer :: jday_prev           !none       |julian day of previous operation
      real :: phutot                 !heat unit  |total potential heat units for year (used
                                     !           |when no crop is growing)
      real :: tave                   !           |
      real :: phuday                 !           |
      real :: xx                     !           |
      real :: xm                     !           |
      real :: sin_sl                 !           |
      real :: sl_len                 !           | 
      real :: phu0                   !deg C      |base zero heat units for year
      real :: sd                     !radians    |solar declination: latitude at which the sun
                                     !           |is directly overhead at noon
      real :: sdlat                  !none       |(-Tan(sd)*Tan(lat))
      real :: h                      !none       |Acos(-Tan(sd)*Tan(lat))
      real :: daylength              !hours      |daylength
      real :: laimx_pop              !           |max lai given plant population
      real :: matur_frac             !frac       |fraction to maturity - use hu for annuals and years to maturity for perennials
      real :: f                      !none       |fraction of plant's maximum lai corresponding to a given fraction of phu
      real :: dd                  !none          |relative distance of the earth from the sun
      
      j = iihru

      !! allocate plants
        icom = hru(j)%plant_cov
        if (icom == 0) then
          pcom(j)%npl = 0
        else
          if (init > 0) then
            deallocate (pcom(j)%pl)
            deallocate (pcom(j)%plg) 
            deallocate (pcom(j)%plm)
            deallocate (pl_mass(j)%tot)
            deallocate (pl_mass(j)%ab_gr)
            deallocate (pl_mass(j)%leaf)
            deallocate (pl_mass(j)%stem)
            deallocate (pl_mass(j)%seed)
            deallocate (pl_mass(j)%root)
            deallocate (pl_mass(j)%yield_tot)
            deallocate (pl_mass(j)%yield_yr)
            deallocate (pcom(j)%plstr) 
            deallocate (pcom(j)%plcur) 
            deallocate (rsd1(j)%tot)
          end if
        
        pcom(j)%npl = pcomdb(icom)%plants_com
        ipl = pcom(j)%npl
        allocate (pcom(j)%pl(ipl))
        allocate (pcom(j)%plg(ipl)) 
        allocate (pcom(j)%plm(ipl)) 
        allocate (pl_mass(j)%tot(ipl)) 
        allocate (pl_mass(j)%ab_gr(ipl))
        allocate (pl_mass(j)%leaf(ipl))
        allocate (pl_mass(j)%stem(ipl))
        allocate (pl_mass(j)%seed(ipl))
        allocate (pl_mass(j)%root(ipl))
        allocate (pl_mass(j)%yield_tot(ipl))
        allocate (pl_mass(j)%yield_yr(ipl))
        allocate (pcom(j)%plstr(ipl)) 
        allocate (pcom(j)%plcur(ipl)) 
        allocate (rsd1(j)%tot(ipl))
        allocate (rsd1(j)%meta(ipl))
        allocate (rsd1(j)%str(ipl))
        allocate (rsd1(j)%lignin(ipl))
        !! allocate water uptake by layer
        do ipl = 1, pcom(j)%npl
          allocate (pcom(j)%plcur(ipl)%uptake(soil(j)%nly))
          pcom(j)%plcur(ipl)%uptake = 0.
        end do

        pcom(j)%rsd_covfac = 0.
        cvm_com(j) = 0.
        rsdco_plcom(j) = 0.
        pcom(j)%pcomdb = icom
        pcom(j)%rot_yr = 1
        pcom(j)%laimx_sum = 0.
        do ipl = 1, pcom(j)%npl
          pcom(j)%pl(ipl) = pcomdb(icom)%pl(ipl)%cpnm
          pcom(j)%plcur(ipl)%gro = pcomdb(icom)%pl(ipl)%igro
          pcom(j)%plcur(ipl)%idorm = "y"
          idp = pcomdb(icom)%pl(ipl)%db_num
          rsd1(j)%tot(ipl)%m = pcomdb(icom)%pl(ipl)%rsdin
          !set fresh organic pools--assume cn ratio = 57 and cp ratio = 300
          rsd1(j)%tot(ipl)%c = 0.43 * rsd1(j)%tot(ipl)%m
          rsd1(j)%tot(ipl)%n = 0.43 * rsd1(j)%tot(ipl)%m / 57.
          rsd1(j)%tot(ipl)%p = 0.43 * rsd1(j)%tot(ipl)%m / 300.
          
          ! set heat units to maturity
          ! first compute base0 units for entire year
          iob = hru(j)%obj_no
          iwst = ob(iob)%wst
          iwgn = wst(iwst)%wco%wgn
          phu0 = 0.
          do iday = 1, 365
            call xmon (iday, mo, day_mo)
            tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
            if (tave > 0.) phu0 = phu0 + tave
          end do
          iday_sh = 181

          ! if days to maturity are not input (0) - assume the plant is potentially active during entire growing season
          if (pldb(idp)%days_mat < 1.e-6 .and. pldb(idp)%days_mat > -1.e-6) then
            ! if zero assume growing season over entire year
            phutot = 0.
            do iday = 1, 365
              call xmon (iday, mo, day_mo)
              tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
              phuday = tave - pldb(idp)%t_base
              if (phuday > 0.) then
                phutot = phutot + phuday
              end if
            end do
            pcom(j)%plcur(ipl)%phumat = .95 * phutot
          else if (pldb(idp)%days_mat < 2.e-6) then
            ! if negative assume heat units to maturity
            pcom(j)%plcur(ipl)%phumat = -pcom(j)%plcur(ipl)%phumat
          else
            ! if positive assume days to maturity
            ! calculate planting day for summer annuals
            if (pldb(idp)%typ == "warm_annual" .or. pldb(idp)%typ == "warm_annual_tuber" .or.   &
                pldb(idp)%typ == "cold_annual" .or. pldb(idp)%typ == "cold_annual_tuber") then
              iday_sum = 181
              phutot = 0.
              phu0 = 0.15 * phu0    !assume planting at 0.15 base 0 heat units
              do iday = 1, 365
                if (wgn(iwgn)%lat > 0.) then
                  call xmon (iday, mo, day_mo)
                else
                  ! find Southern Hemisphere day
                  iday_sum = iday_sum + 1
                  if (iday_sum > 365) then
                    iday_sh = iday_sum - 365
                  else
                    iday_sh = iday_sum
                  end if
                  call xmon (iday_sh, mo, day_mo)
                end if
                tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
                phuday = tave
                if (phuday > 0.) then
                  phutot = phutot + phuday
                end if
                if (phutot > phu0) then
                  if (wgn(iwgn)%lat > 0.) then
                    igrow = iday
                  else
                    igrow = iday_sh
                  end if
                  exit   ! exit on plant day at 0.15 base 0 heat units
                end if
              end do
            end if
          
            ! switched from starting hu at dormancy (daylength) to 0.15 hu (above) like summer annuals
            if (pldb(idp)%typ == "null" .or. pldb(idp)%typ == "null1") then
              if (wgn(iwgn)%lat > 0.) then
                igrow = 1
              else
                igrow = 181
              end if
              do iday = igrow, igrow + 180
                call xmon (iday, mo, day_mo)
                tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
                phuday = tave - pldb(idp)%t_base
                !if (phuday > 0.) then
                  !! start accumulating hu at end of dormancy (daylength)
                  !! calculate solar declination: equation 2.1.2 in SWAT manual
                  sd = Asin(.4 * Sin((Real(iday) - 82.) / 58.09))  !!365/2pi = 58.09
                  !! calculate the relative distance of the earth from the sun the eccentricity of the orbit
                  dd = 1.0 + 0.033 * Cos(Real(iday) / 58.09)
                  sdlat = -wgn_pms(iwgn)%latsin * Tan(sd) / wgn_pms(iwgn)%latcos
                  if (sdlat > 1.) then    !! sdlat will be >= 1. if latitude exceeds +/- 66.5 deg in winter
                     h = 0.
                  elseif (sdlat >= -1.) then
                    h = Acos(sdlat)
                  else
                    h = 3.1416         !! latitude exceeds +/- 66.5 deg in summer
                  endif 
                  daylength = 7.6394 * h
                  if (daylength - wgn_pms(iwgn)%daylth >= wgn_pms(iwgn)%daylmn) exit
                !end if
              end do
              igrow = iday
            end if
            
            ! calculate heat units from plant day (iday) to maturity (add days to maturity)
            phutot = 0.
            do iday = igrow, igrow + pldb(idp)%days_mat
              if (wgn(iwgn)%lat > 0.) then
                call xmon (iday, mo, day_mo)
              else
                ! find Southern Hemisphere day
                if (iday > 365) then
                  iday_sh = iday - 365
                else
                  iday_sh = iday
                end if
                call xmon (iday_sh, mo, day_mo)
              end if
              tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
              phuday = tave - pldb(idp)%t_base
              if (phuday > 0.) then
                phutot = phutot + phuday
              end if
            end do
            pcom(j)%plcur(ipl)%phumat = phutot
          end if
          
          ! set initial operation for date scheduling
          isched = hru(j)%mgt_ops
          if (sched(isched)%num_ops > 0) then
          if (sched(isched)%mgt_ops(1)%jday > 0) then
            irot = 1
            jday_prev = sched(isched)%mgt_ops(1)%jday
            do iop = 1, sched(isched)%num_ops
              if (irot == pcomdb(icom)%rot_yr_ini .and. time%day_start <= sched(isched)%mgt_ops(iop)%jday) then
                exit
              else
                if (sched(isched)%mgt_ops(iop)%op == "skip" .or. sched(isched)%mgt_ops(iop)%jday < jday_prev) then
                  irot = irot + 1
                end if
              end if
              jday_prev = sched(isched)%mgt_ops(iop)%jday
            end do
            sched(isched)%first_op = min (iop-1, sched(isched)%num_ops)
            sched(isched)%first_op = max (1, sched(isched)%first_op)
          else
            sched(isched)%first_op = 1
          end if
          end if
          hru(j)%cur_op = sched(isched)%first_op

          pcom(j)%name = pcomdb(icom)%name
          ! set initial rotation year for dtable scheduling
          pcom(j)%rot_yr = pcomdb(icom)%rot_yr_ini
          
          pcom(j)%plcur(ipl)%phuacc = pcomdb(icom)%pl(ipl)%phuacc
          !! set fraction to maturity and initial canopy height for annuals and perennials
          if (pldb(idp)%typ == "perennial") then
            matur_frac = pcomdb(icom)%pl(ipl)%fr_yrmat
            pcom(j)%plg(ipl)%cht = matur_frac * pldb(idp)%chtmx
          else  !annuals
            matur_frac = pcomdb(icom)%pl(ipl)%phuacc
            f = pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +     &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc))
            pcom(j)%plg(ipl)%cht = pldb(idp)%chtmx * Sqrt(f)
          end if
          
          pcom(j)%plg(ipl)%laimxfr = matur_frac / (matur_frac +     &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * matur_frac))
          if (pcomdb(icom)%pl(ipl)%igro == "y") then
            pcom(j)%plg(ipl)%lai = pcomdb(icom)%pl(ipl)%lai
          else
            pcom(j)%plg(ipl)%lai = 0.
          end if
          pcom(j)%laimx_sum = pcom(j)%laimx_sum + pldb(idp)%blai
          pl_mass(j)%tot(ipl)%m = pcomdb(icom)%pl(ipl)%bioms
          pcom(j)%plcur(ipl)%curyr_mat = int (pcomdb(icom)%pl(ipl)%fr_yrmat * float(pldb(idp)%mat_yrs))
          pcom(j)%plcur(ipl)%curyr_mat = max (1, pcom(j)%plcur(ipl)%curyr_mat)
          ! set total hu to maturity for perennials
          pcom(j)%plcur(ipl)%phumat_p = pcom(j)%plcur(ipl)%phumat * pldb(idp)%mat_yrs
            
          cvm_com(j) = plcp(idp)%cvm + cvm_com(j)
          pcom(j)%rsd_covfac = pcom(j)%rsd_covfac + pldb(idp)%rsd_covfac
          rsdco_plcom(j) = rsdco_plcom(j) + pldb(idp)%rsdco_pl
          pcom(j)%plcur(ipl)%idplt = pcomdb(icom)%pl(ipl)%db_num
          
          !! set intial n and p contents in total plant
          pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1- pldb(idp)%pltnfr3) *              &
             (1.- matur_frac /(matur_frac + Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *       &
             matur_frac))) + pldb(idp)%pltnfr3
          pl_mass(j)%tot(ipl)%n = pcom(j)%plm(ipl)%n_fr * pl_mass(j)%tot(ipl)%m
          pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1-pldb(idp)%pltpfr3)*                &
             (1. - matur_frac / (matur_frac + Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *     &
             matur_frac))) + pldb(idp)%pltpfr3
          pl_mass(j)%tot(ipl)%p = pcom(j)%plm(ipl)%p_fr * pl_mass(j)%tot(ipl)%m                  
          
          if (pcom(j)%plcur(ipl)%pop_com < 1.e-6) then
            laimx_pop = pldb(idp)%blai
          else
            xx = pcom(j)%plcur(ipl)%pop_com / 1001.
            laimx_pop = pldb(idp)%blai * xx / (xx + exp(pldb(idp)%pop1 - pldb(idp)%pop2 * xx))
          end if
          pcom(j)%plcur(ipl)%harv_idx = pldb(idp)%hvsti
          pcom(j)%plcur(ipl)%lai_pot = laimx_pop
          
          !! initialize plant mass if plant growing
          if (pcom(j)%plcur(ipl)%gro == "y") then
            call pl_root_gro(j)
            call pl_seed_gro(j)
            call pl_partition(j)
          end if

        end do   ! ipl loop
        
        !! get average residue cover factor for community
        if (pcom(j)%npl > 0) then
          pcom(j)%rsd_covfac = pcom(j)%rsd_covfac / pcom(j)%npl
          cvm_com(j) = cvm_com(j) / pcom(j)%npl
        else
          pcom(j)%rsd_covfac = 0.
          cvm_com(j) = 0.
        end if
        
        end if   ! icom > 0

        ilum = hru(iihru)%land_use_mgt
                 
        !! set epco parameter for each crop
        do ipl = 1, pcom(iihru)%npl
          pcom(iihru)%plcur(ipl)%epco = hru(iihru)%hyd%epco
        end do
        
        !! set p factor and slope length (ls factor)
        icp = lum_str(ilum)%cons_prac
        xm = .6 * (1. - Exp(-35.835 * hru(iihru)%topo%slope))
        sin_sl = Sin(Atan(hru(iihru)%topo%slope))
        sl_len = amin1 (hru(iihru)%topo%slope_len, cons_prac(icp)%sl_len_mx)
        hru(iihru)%lumv%usle_ls = (hru(iihru)%topo%slope_len / 22.128) ** xm *          & 
                      (65.41 * sin_sl * sin_sl + 4.56 * sin_sl + .065)
        hru(iihru)%lumv%usle_p = cons_prac(icp)%pfac
        
        !! xwalk urban land use type with urban name in urban.urb
        hru(iihru)%luse%urb_ro = lum(ilum)%urb_ro
        do idb = 1, db_mx%urban
          if (lum(ilum)%urb_lu == urbdb(idb)%urbnm) then
            hru(iihru)%luse%urb_lu = idb
            exit
          endif
        end do
        
        !! xwalk overland n with name in ovn_table.lum
        do idb = 1, db_mx%ovn
          if (lum(ilum)%ovn == overland_n(idb)%name) then
            hru(iihru)%luse%ovn = overland_n(idb)%ovn
            exit
          endif
        end do
 
    return
    end subroutine plant_init
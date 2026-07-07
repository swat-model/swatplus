      subroutine plant_init (init, iihru)

      use hru_module, only : cn2, cvm_com, hru, ipl, isol, rsdco_plcom
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
      integer :: ilug                !none       |counter 
      integer :: iob                 !           |spatial object number
      integer :: iwgn                !           |weather generator number
      integer :: mo                  !none       |counter 
      integer :: iday                !none       |counter 
      integer :: iplt                !none       |counter 
      integer :: i                   !none       |counter
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
      real :: grow_start             !           |
      real :: grow_end               !           | 
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
        allocate (pcom(j)%plstr(ipl)) 
        allocate (pcom(j)%plcur(ipl)) 
        allocate (rsd1(j)%tot(ipl))

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
          if (pldb(idp)%days_mat < 1.e-6) then
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
          else
            ! calculate planting day for summer annuals
            if (pldb(idp)%typ == "warm_annual" .or. pldb(idp)%typ == "warm_annual_tuber") then
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
          
            ! caculate planting day for winter annuals at end of dormancy
            if (pldb(idp)%typ == "cold_annual") then
              if (wgn(iwgn)%lat > 0.) then
                igrow = 1
              else
                igrow = 181
              end if
              do iday = igrow, igrow + 180
                call xmon (iday, mo, day_mo)
                tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
                phuday = tave - pldb(idp)%t_base
                if (phuday > 0.) then
                  !exit and assume start accumulating hu when temperature goes above base temp
                  !could switch to end of dormancy (daylength)
                  exit
                end if
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
          
          ! set initial heat units and other data
          pcom(j)%plcur(ipl)%phuacc = pcomdb(icom)%pl(ipl)%phuacc
          pcom(j)%plg(ipl)%laimxfr = pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +     &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc))
          pcom(j)%plg(ipl)%lai = pcomdb(icom)%pl(ipl)%lai
          pcom(j)%laimx_sum = pcom(j)%laimx_sum + pldb(idp)%blai
          pl_mass(j)%tot(ipl)%m = pcomdb(icom)%pl(ipl)%bioms
          pcom(j)%plcur(ipl)%curyr_mat = int (pcomdb(icom)%pl(ipl)%fr_yrmat * float(pldb(idp)%mat_yrs))
          pcom(j)%plcur(ipl)%curyr_mat = max (1, pcom(j)%plcur(ipl)%curyr_mat)
          cvm_com(j) = plcp(idp)%cvm + cvm_com(j)
          rsdco_plcom(j) = rsdco_plcom(j) + pldb(idp)%rsdco_pl
          pcom(j)%plcur(ipl)%idplt = pcomdb(icom)%pl(ipl)%db_num
          idp = pcom(j)%plcur(ipl)%idplt
          pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1-pldb(idp)%pltpfr3)*        &
          (1. - pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +          &
           Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                                &
           pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3
          pl_mass(j)%tot(ipl)%n = pcom(j)%plm(ipl)%n_fr * pl_mass(j)%tot(ipl)%m                  
          pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1- pldb(idp)%pltnfr3) *      &
           (1.- pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +          &
           Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                                &
          pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3
           pl_mass(j)%tot(ipl)%p = pcom(j)%plm(ipl)%p_fr * pl_mass(j)%tot(ipl)%m
          if (pcom(j)%plcur(ipl)%pop_com < 1.e-6) then
            laimx_pop = pldb(idp)%blai
          else
            xx = pcom(j)%plcur(ipl)%pop_com / 1001.
            laimx_pop = pldb(idp)%blai * xx / (xx +          &
                    exp(pldb(idp)%pop1 - pldb(idp)%pop2 * xx))
          end if
          pcom(j)%plcur(ipl)%harv_idx = pldb(idp)%hvsti
          pcom(j)%plcur(ipl)%lai_pot = laimx_pop
          
          !! initialize plant mass
          call pl_root_gro(j)
          call pl_seed_gro(j)
          call pl_partition(j)

        end do   ! ipl loop
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
        
        !! set parameters for structural land use/managment
        if (lum(ilum)%tiledrain /= "null") then
          call structure_set_parms("tiledrain       ", lum_str(ilum)%tiledrain, j)
        end if
      
        if (lum(ilum)%septic /= "null") then
          call structure_set_parms("septic          ", lum_str(ilum)%septic, j)
        end if
        
        if (lum(ilum)%fstrip /= "null") then
          call structure_set_parms("fstrip          ", lum_str(ilum)%fstrip, j)
        end if
        
        if (lum(ilum)%grassww /= "null") then
          call structure_set_parms("grassww         ", lum_str(ilum)%grassww, j)
        end if

        if (lum(ilum)%bmpuser /= "null") then
          call structure_set_parms("bmpuser         ", lum_str(ilum)%bmpuser, j)
        end if

    return
    end subroutine plant_init
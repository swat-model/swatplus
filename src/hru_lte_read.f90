      subroutine hru_lte_read
      
      use maximum_data_module
      use plant_data_module
      use hru_lte_module
      use hydrograph_module
      use input_file_module
      use output_landscape_module
      use climate_module
      use time_module
      use soil_data_module
      use conditional_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: grow_start           !           |
      integer :: grow_end             !           |
      integer :: ipl                  !none       |counter
      integer :: istart
      integer :: iend
      integer :: itext
      real :: rtos                    !none       |fraction difference between CN=99 and CN1 
      real :: rto3                    !none       |fraction difference between CN3 and CN1 
      real :: a1                      !           |
      real :: a2                      !           |
      integer :: i                    !           | 
      integer :: isd_h                !none       |counter 
      integer :: k                    !           |
      integer :: idb                  !none       |counter
      integer :: mo                   !none       |counter
      real :: qn1                     !           |
      real :: qn3                     !           |
      real :: s3                      !none       |retention parameter for CN3 
      real :: sumul                   !mm H2O     |amount of water held in soil profile at saturation
      real :: sumfc                   !mm H2O     |amount of water held in the soil profile at field capacity 
      real :: xi                      !           |
      real :: xx                      !           |
      real :: sin                     !           |
      real :: cos                     !           |
      integer :: iwgn                 !           |
      integer :: iplt                 !none       |counter
      integer :: imo                  !none       |counter
      real :: hu_init                 !           |
      real :: phutot                  !heat unit  |total potential heat units for year (used
                                      !           |when no crop is growing) 
      integer :: iday                 !none       |counter
      real :: tave                    !           |
      real :: phuday                  !           | 
      real :: xm                      !           |
      real :: sin_sl                  !           |
      real :: ch_len                  !           |
      real :: ch_sl                   !           | 
      real :: sd_sl                   !           |
      integer :: msd_h                !           | 
      
      eof = 0
      imax = 0

      a1 = .2 
      a2 = .8 
      
      inquire (file=in_hru%hru_ez, exist=i_exist)
      if (.not. i_exist .or. in_hru%hru_ez == "null") then
        allocate (hlt_db(0:0))
      else
      do
        open (1,file=in_hru%hru_ez)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
         do while (eof == 0)
            read (1,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
         end do
         
        !assumes data for each hru -> ok since there is only one file
        allocate (hlt_db(0:imax))
        allocate (hlt(sp_ob%hru_lte))
      !! dimension swatdeg output variables
      msd_h = sp_ob%hru_lte
      allocate (hltwb_d(msd_h))
      allocate (hltwb_m(msd_h))
      allocate (hltwb_y(msd_h))
      allocate (hltwb_a(msd_h))
      allocate (hltnb_d(msd_h))
      allocate (hltnb_m(msd_h))
      allocate (hltnb_y(msd_h))
      allocate (hltnb_a(msd_h))
      allocate (hltls_d(msd_h))
      allocate (hltls_m(msd_h))
      allocate (hltls_y(msd_h))
      allocate (hltls_a(msd_h))
      allocate (hltpw_d(msd_h))
      allocate (hltpw_m(msd_h))
      allocate (hltpw_y(msd_h))
      allocate (hltpw_a(msd_h))

        rewind (1)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit

      do isd_h = 1, imax
        read (1,*,iostat=eof) i
        if (eof < 0) exit
        backspace (1)
        read (1,*,iostat=eof) k, hlt_db(i)
        if (eof < 0) exit
      end do

      do i = 1, sp_ob%hru_lte
         icmd = sp_ob1%hru_lte + i - 1
         idb = ob(icmd)%props
         hlt(i)%name = ob(icmd)%name
         hlt(i)%props = idb
         hlt(i)%obj_no = icmd
         hlt(i)%km2 = hlt_db(idb)%dakm2
         hlt(i)%cn2 = hlt_db(idb)%cn2
         hlt(i)%cn2 = max(35., hlt(i)%cn2)
         hlt(i)%cn2 = amin1(98., hlt(i)%cn2)
         hlt(i)%etco = hlt_db(idb)%etco
         hlt(i)%perco = hlt_db(idb)%perco
         hlt(i)%tdrain = hlt_db(idb)%tdrain
         hlt(i)%revapc = hlt_db(idb)%revapc
         hlt(i)%plant = hlt_db(idb)%plant
         hlt(i)%stress = hlt_db(idb)%stress
         hlt(i)%soildep = hlt_db(idb)%soildep
         hlt_db(idb)%abf = EXP(-hlt_db(idb)%abf) 
         qn1 = hlt_db(idb)%cn2 - (20. * (100. - hlt_db(idb)%cn2)) /        &
            (100.-hlt_db(idb)%cn2 + EXP(2.533-.063*(100.-hlt_db(idb)%cn2)))
         qn1 = Max(qn1, .4 * hlt_db(idb)%cn2)
         qn3 = hlt_db(idb)%cn2 * EXP(.00673*(100.-hlt_db(idb)%cn2)) 
         hlt(i)%smx = 254. * (100. / qn1 - 1.) 
         s3 = 254. * (100. / qn3 - 1.)
         rto3 = 1. - s3 / hlt(i)%smx
         rtos = 1. - 2.54 / hlt(i)%smx
         
         xi = 30. * time%mo - 15. 
         xx = hlt_db(idb)%xlat / 57.3 
         hlt(i)%yls = SIN(xx) 
         hlt(i)%ylc = COS(xx) 
         hlt(i)%phu = 2000. 
         hlt(i)%dm = 0. 
         hlt(i)%alai = .15 
         hlt(i)%g = 0. 
                 
         !crosswalk plant with plants.plt
         do ipl = 1, db_mx%plantparm
            if (hlt_db(idb)%plant == pldb(ipl)%plantnm) then
              hlt(i)%iplant = ipl
              exit
            end if 
         end do
         
         if (hlt(i)%iplant == 0) write (9001,*) hlt_db(idb)%plant, "not found (plants.plt)"
        
      !crosswalk
         do istart = 1, db_mx%dtbl_lum
            if (hlt_db(idb)%igrow1 == dtbl_lum(istart)%name) then
              hlt(i)%start = istart
              exit
            endif
         end do
         
        if (hlt(i)%start == 0) write (9001,*) hlt_db(idb)%igrow1, " entry in (hru-lte.hru) not found in (lum.dtl)"
         
      !crosswalk
         do iend = 1, db_mx%dtbl_lum
            if (hlt_db(idb)%igrow2 == dtbl_lum(iend)%name) then
              hlt(i)%end = iend
              exit
            endif
         end do
         
        if (hlt(i)%end == 0) write (9001,*) hlt_db(idb)%igrow2, " entry in (hru-lte.hru) not found in (lum.dtl)"
         
         !compute heat units from growing season and weather generator
         iwst = ob(icmd)%wst
         iwgn = wst(iwst)%wco%wgn
         iplt = hlt(i)%iplant
         if (hlt_db(idb)%igrow2 > hlt_db(idb)%igrow1) then
           grow_start = hlt(i)%start
           grow_end = hlt(i)%end
           hu_init = .15
         else
           grow_start = hlt(i)%start
           grow_end = hlt(i)%end
           hu_init = .85
         end if
         mo = 1
         imo = 2
         phutot = 0.
         do iday = 1, 365
           if (iday > ndays(imo)) then
             imo = imo + 1
             mo = mo + 1
           end if
           if (grow_end > grow_start) then
             if (iday > grow_start .and. iday < grow_end) then
               tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
               phuday = tave - pldb(iplt)%t_base
               if (phuday > 0.) then
                 phutot = phutot + phuday
               end if
             end if
           else 
             if (iday > grow_start .or. iday < grow_end) then
               tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
               phuday = tave - pldb(iplt)%t_base
               if (phuday > 0.) then
                 phutot = phutot + phuday
               end if
             end if
           end if
         end do
         ! change from growing season to time to maturity
         hlt(i)%phu = .9 * phutot
         hlt(i)%phu = Max(500., hlt(i)%phu)
         
         if (pldb(iplt)%typ == "warm_annual" .or. pldb(iplt)%typ == "cold_annual" .or.  &
             pldb(iplt)%typ == "warm_annual_tuber" .or. pldb(iplt)%typ == "cold_annual_tuber") then
           hlt(i)%phu = Min(2000., hlt(i)%phu)
         end if

         ! compute musle factors
         ! calculate USLE slope length factor
         xm = 0.
         sin_sl = 0.
         xm = .6 * (1. - EXP(-35.835 * hlt_db(idb)%slope))
         sin_sl = SIN(Atan(hlt_db(idb)%slope))
         hlt_db(idb)%uslels = (hlt_db(idb)%slopelen/22.128)**xm *          &
                        (65.41 * sin_sl * sin_sl + 4.56 * sin_sl + .065)
!     !      calculate composite usle value
         hlt(i)%uslefac = hlt_db(idb)%uslek * hlt_db(idb)%uslep *           &
           hlt_db(idb)%uslels * hlt_db(idb)%uslec * 11.8
         
        ! compute time of concentration using Kirpich equation
        IF (hlt_db(idb)%tc < 1.e-6) THEN
         ch_len = hlt_db(idb)%dakm2
         ch_sl = hlt_db(idb)%slope
         hlt_db(idb)%tc = .0078 * (ch_len * 3210.) ** .77 * sd_sl        &   
                                                        ** (-.385)
        END IF
        
        !! xwalk with soil_lte
        do itext = 1, 12
          if (soil_lte(itext)%texture == hlt_db(idb)%text) then
            hlt(i)%awc = soil_lte(itext)%awc * hlt_db(idb)%soildep
            hlt(i)%por = soil_lte(itext)%por * hlt_db(idb)%soildep
            hlt(i)%sc = soil_lte(itext)%scon
            hlt(i)%sw = hlt_db(idb)%sw * hlt(i)%awc
            hltwb_d(i)%sw_init = hlt(i)%sw
            hlt(i)%hk = (hlt(i)%por - hlt(i)%awc) / hlt(i)%sc   
            hlt(i)%hk = Max(2., hlt(i)%hk)
            sumul = hlt(i)%por
            sumfc = hlt(i)%awc + hlt_db(idb)%cn3_swf * (sumul - hlt(i)%awc)
            !! calculate shape parameters
            call ascrv(rto3, rtos, sumfc, sumul, hlt(i)%wrt1, hlt(i)%wrt2)
          end if
        end do
        
            hlt_db(idb)%tc = hlt_db(idb)%tc * 60.     !!min to seconds
        end do

      exit
      end do
      endif
      
      close (1)
        
      return
      end subroutine hru_lte_read   
      subroutine hyd_connect

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches, 
!!     and reservoirs

      use hydrograph_module
      use input_file_module
      use recall_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use ru_module
      use basin_module
      use gwflow_module, only: nat_model
      
      implicit none

      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      character (len=3) :: iob_out    !           !object type out
      character (len=3) :: iobtyp     !none       |object type
      integer :: nspu                 !           |
      integer :: cmdno                !           |
      integer :: idone                !           | 
      !integer :: hydno
      integer :: cmd_prev             !           |
      integer :: ob1                  !none       |beginning of loop
      integer :: ob2                  !none       |ending of loop
      integer :: iobj_tot             !           |
      real :: mexco_sp                !           |
      integer :: i                    !none       |counter
      integer :: ii                   !none       |counter
      integer :: ielem                !none       |counter 
      integer :: k                    !none       |counter
      integer :: iob                  !           |
      integer :: kk                   !none       |counter
      integer :: j                    !           |
      integer :: ielem_db             !           |
      integer :: jj                   !none       |counter
      integer :: iord                 !none       |counter
      integer :: isrc_tot             !           |
      integer :: iorder               !           |
      integer :: ircv                 !none       |counter
      integer :: ircv_ob              !           |
      integer :: max                  !           |
      logical :: i_exist
    
      eof = 0
      imax = 0
      mexco_sp = 0
      cmd_prev = 0

      allocate (rcv_sum(sp_ob%objs))
      allocate (dfn_sum(sp_ob%objs))
      allocate (ru_seq(sp_ob%objs))
      rcv_sum = 0
      dfn_sum = 0
      ru_seq = 0
      
      !! set first object number of each type
      nspu = 1
      if (sp_ob%hru > 0) then         ! 1==hru
        sp_ob1%hru = nspu
        nspu = sp_ob%hru + nspu
      end if
      if (sp_ob%hru_lte > 0) then     ! 2==hru_lte
        sp_ob1%hru_lte = nspu
        nspu = sp_ob%hru_lte + nspu
      end if
      if (sp_ob%ru > 0) then         ! 3==subbasin
        sp_ob1%ru = nspu
        nspu = sp_ob%ru + nspu
      end if
      if (sp_ob%gwflow > 0) then     ! 4==gwflow
        sp_ob1%gwflow = nspu
        nspu = sp_ob%gwflow + nspu
        inquire(file='gwflow.huc12cell',exist=i_exist)
      end if
      if (sp_ob%aqu > 0) then         ! 5==aquifer
        sp_ob1%aqu = nspu
        nspu = sp_ob%aqu + nspu
      end if
      if (sp_ob%chan > 0) then        ! 6==channel
        sp_ob1%chan = nspu
        nspu = sp_ob%chan + nspu
      end if
      if (sp_ob%res > 0) then         ! 7==reservoir
        sp_ob1%res = nspu
        nspu = sp_ob%res + nspu
      end if
      if (sp_ob%recall > 0) then      ! 8==recall
        sp_ob1%recall = nspu
        nspu = sp_ob%recall + nspu
      end if
      if (sp_ob%exco > 0) then        ! 11==exco
        sp_ob1%exco = nspu
        nspu = sp_ob%exco + nspu
      end if
      if (sp_ob%dr > 0) then          ! 12==dr
        sp_ob1%dr = nspu
        nspu = sp_ob%dr + nspu
      end if
      if (sp_ob%canal > 0) then       ! 13==canal
        sp_ob1%canal = nspu
        nspu = sp_ob%canal + nspu
      end if
      if (sp_ob%pump > 0) then        ! 14==pump
        sp_ob1%pump = nspu
        nspu = sp_ob%pump + nspu
      end if
      if (sp_ob%outlet > 0) then      ! 15==outlet
        sp_ob1%outlet = nspu
        nspu = sp_ob%outlet + nspu
      end if
      if (sp_ob%chandeg > 0) then     ! 16==swat-deg channel
        sp_ob1%chandeg = nspu
        nspu = sp_ob%chandeg + nspu
      end if
!      if (sp_ob%a2d > 0) then        ! 17==2D aquifer
!        sp_ob1%a2d = nspu
!        nspu = sp_ob%a2d + nspu
!      end if

      !read connect file for hrus
      if (sp_ob%hru > 0) then
        call hyd_read_connect(in_con%hru_con, "hru     ", sp_ob1%hru, sp_ob%hru, hd_tot%hru, bsn_prm%day_lag_mx)     
      end if
      
      !read connect file for hru_lte"s
      if (sp_ob%hru_lte > 0) then
        call hyd_read_connect(in_con%hruez_con, "hru_lte ", sp_ob1%hru_lte, sp_ob%hru_lte, hd_tot%hru_lte, bsn_prm%day_lag_mx) 
      end if
      
      !read connect file for routing units
      if (sp_ob%ru > 0) then
        call hyd_read_connect(in_con%ru_con, "ru      ", sp_ob1%ru, sp_ob%ru, hd_tot%ru, bsn_prm%day_lag_mx)
        call ru_read
        call ru_read_elements
      end if
                     
      !read connect file for aquifer (1-D)
      if (sp_ob%aqu > 0) then
        call hyd_read_connect(in_con%aqu_con, "aqu     ", sp_ob1%aqu, sp_ob%aqu, hd_tot%aqu, 1)
        call aqu2d_read
      end if
                  
      !read connect file for channels
      if (sp_ob%chan > 0) then
        call hyd_read_connect(in_con%chan_con, "chan    ", sp_ob1%chan, sp_ob%chan, hd_tot%chan, bsn_prm%day_lag_mx) 
        call overbank_read
      end if
                                  
      !read connect file for reservoirs
      if (sp_ob%res > 0) then
        call hyd_read_connect(in_con%res_con, "res     ", sp_ob1%res, sp_ob%res, hd_tot%res, 1) 
      end if
                
      !read connect file for recalls
      if (sp_ob%recall > 0) then
        call hyd_read_connect(in_con%rec_con, "recall  ", sp_ob1%recall, sp_ob%recall, hd_tot%recall, 1) 
        call recall_read
        call recall_read_salt !rtb salt
        call recall_read_cs !rtb cs
      end if
                
      !read connect file for export coefficients
      if (sp_ob%exco > 0) then
        call hyd_read_connect(in_con%exco_con, "exco    ", sp_ob1%exco, sp_ob%exco, hd_tot%exco, 1) 
      endif
                  
      !read connect file for delivery ratio
      if (sp_ob%dr > 0) then
        call hyd_read_connect(in_con%delr_con, "dr      ", sp_ob1%dr, sp_ob%dr, hd_tot%dr, 1) 
        call dr_db_read
      end if
                  
      !read connect file for outlet
      if (sp_ob%outlet > 0) then
        call hyd_read_connect(in_con%out_con, "outlet  ", sp_ob1%outlet, sp_ob%outlet, hd_tot%outlet, bsn_prm%day_lag_mx)
      end if
          
      !read connect file for swat-deg channels
      if (sp_ob%chandeg > 0) then
        call hyd_read_connect(in_con%chandeg_con, "chandeg ", sp_ob1%chandeg, sp_ob%chandeg, hd_tot%chandeg, bsn_prm%day_lag_mx)
      end if
      
      !read connect file for gwflow
      if (sp_ob%gwflow > 0) then
        call gwflow_chan_read !first, read in channel cell information
        call hyd_read_connect(in_con%gwflow_con, "gwflow  ", sp_ob1%gwflow, sp_ob%gwflow, hd_tot%gwflow, 1)
        call gwflow_read
      end if
      
      !! for each hru or defining unit, set all subbasins that contain it 
        do i = 1, sp_ob%objs
          nspu = ob(i)%ru_tot
          if (nspu > 0) then
            allocate (ob(i)%obj_subs(nspu))
          end if
        end do

        do iru = 1, sp_ob%ru
          !! determine subbasin the hrus are in and add subbasin area to basin area
          do ii = 1, ru_def(iru)%num_tot
            !!only have hrus set up - need to add other objects
            ielem = ru_def(iru)%num(ii)        !points to element in sub_element
            k = ru_elem(ielem)%obj              !object type number of element (ie hru)
            iob = sp_ob1%ru + iru - 1         !object number of the routing unit
            ru_seq(k) = ru_seq(k) + 1           !sequential number of routing unit the element is in
            kk = ru_seq(k)
            ob(k)%obj_subs(kk) = iob            !routing unit the element is in
            dfn_sum(iob) = dfn_sum(iob) + 1     !sum of elements in the routing unit object
          end do
        end do
      
      !! determine number of recieving units and set object numbers for outflow hyds
        do i = 1, sp_ob%objs
          do ii = 1, ob(i)%src_tot
            iob_out = ob(i)%obtyp_out(ii)          !object type out
            select case (iob_out)
            case ("hru")   !hru
              ob(i)%obj_out(ii) = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("hlt")   !hru_lte
              ob(i)%obj_out(ii) = sp_ob1%hru_lte+ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("ru")   !routing unit
              ob(i)%obj_out(ii) = sp_ob1%ru + ob(i)%obtypno_out(ii) - 1
              iru = ob(i)%obtypno_out(ii)
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              do kk = 1, ru_def(iru)%num_tot
                ielem = ru_def(iru)%num(kk)
                iob = ru_elem(ielem)%obj
                ob(iob)%rcv_tot = ob(iob)%rcv_tot + 1
              end do
            case ("gwflow")   !gwflow
              ob(i)%obj_out(ii) = sp_ob1%gwflow+ob(i)%obtypno_out(ii)-1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("aqu")   !aquifer
              ob(i)%obj_out(ii) = sp_ob1%aqu + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("cha")   !channel
              !! all channels receive flow from modflow
              if (ob(i)%typ == "modflow" .and. ob(i)%obtypno_out(ii) <= 0) then
                do ich = 1, sp_ob%chan
                  ob(i)%obj_out(ii) = sp_ob1%chan + ich - 1
                  j = ob(i)%obj_out(ii)
                  ob(j)%rcv_tot = ob(j)%rcv_tot + 1
                end do
              else
                ob(i)%obj_out(ii) = sp_ob1%chan +ob(i)%obtypno_out(ii) - 1
                j = ob(i)%obj_out(ii)
                ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              end if
            case ("res")   !reservoir
              ob(i)%obj_out(ii) = sp_ob1%res + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("exc")   !export coefficients
              ob(i)%obj_out(ii) = sp_ob1%exco + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("dr")   !delivery ratio
              ob(i)%obj_out(ii) = sp_ob1%dr + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("out")   !outlet
              ob(i)%obj_out(ii) = sp_ob1%outlet + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            case ("sdc")   !swat-deg channel
              ob(i)%obj_out(ii) = sp_ob1%chandeg+ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
            end select
            
            select case (ob(i)%htyp_out(ii))
            case ("tot")   !total flow
               ob(i)%ihtyp_out(ii) = 1
            case ("rhg")   !recharge
               ob(i)%ihtyp_out(ii) = 2              
            case ("sur")   !surface
               ob(i)%ihtyp_out(ii) = 3 
            case ("lat")   !lateral
               ob(i)%ihtyp_out(ii) = 4
            case ("til")   !tile
               ob(i)%ihtyp_out(ii) = 5  
            end select             
          end do
        end do

      !! allocate zero arrays for constituents
      allocate (hin_csz%pest(cs_db%num_pests))
      allocate (hin_csz%path(cs_db%num_paths))
      allocate (hin_csz%hmet(cs_db%num_metals))
      allocate (hin_csz%salt(cs_db%num_salts)) !rtb salt
      allocate (hin_csz%cs(cs_db%num_cs)) !rtb se 
          
      allocate (hcs1%pest(cs_db%num_pests))
      allocate (hcs1%path(cs_db%num_paths))
      allocate (hcs1%hmet(cs_db%num_metals))
      allocate (hcs1%salt(cs_db%num_salts)) !rtb salt
      allocate (hcs1%cs(cs_db%num_cs)) !rtb cs
        
      allocate (hcs2%pest(cs_db%num_pests))
      allocate (hcs2%path(cs_db%num_paths))
      allocate (hcs2%hmet(cs_db%num_metals))
      allocate (hcs2%salt(cs_db%num_salts)) !rtb salt
      allocate (hcs2%cs(cs_db%num_cs)) !rtb cs
      
      allocate (hcs3%pest(cs_db%num_pests))
      allocate (hcs3%path(cs_db%num_paths))
      allocate (hcs3%hmet(cs_db%num_metals))
      allocate (hcs3%salt(cs_db%num_salts)) !rtb salt
      allocate (hcs3%cs(cs_db%num_cs)) !rtb cs

      hin_csz%pest = 0.
      hin_csz%path = 0.
      hin_csz%hmet = 0.
      hin_csz%salt = 0. !rtb salt
      hin_csz%cs = 0. !rtb cs

      !! allocate receiving arrays
      do i = 1, sp_ob%objs
        if (ob(i)%rcv_tot > 0) then
          nspu = ob(i)%rcv_tot
          allocate (ob(i)%obj_in(nspu))
          allocate (ob(i)%obtyp_in(nspu))
          allocate (ob(i)%obtypno_in(nspu))
          allocate (ob(i)%htyp_in(nspu))
          allocate (ob(i)%ihtyp_in(nspu))
          allocate (ob(i)%frac_in(nspu))
          allocate (ob(i)%hin_uh(nspu))
          !! allocate unit hyd for all incoming hyd's
          do ii = 1, nspu
            allocate (ob(i)%hin_uh(ii)%uh(bsn_prm%day_lag_mx,time%step))
            allocate (ob(i)%hin_uh(ii)%hyd_flo(bsn_prm%day_lag_mx,time%step))
            ob(i)%hin_uh(ii)%uh = 0.
            ob(i)%hin_uh(ii)%hyd_flo = 0.
          end do
          allocate (ob(i)%hin_d(nspu))
          allocate (ob(i)%hin_m(nspu))
          allocate (ob(i)%hin_y(nspu))
          allocate (ob(i)%hin_a(nspu))
          allocate (obcs(i)%hcsin_d(nspu))
          allocate (obcs(i)%hcsin_m(nspu))
          allocate (obcs(i)%hcsin_y(nspu))
          allocate (obcs(i)%hcsin_a(nspu))
        end if
      end do

      !! loop through again and set receiving dependencies 
      do i = 1, sp_ob%objs                                  ! i=object number of source object
        do ii = 1, ob(i)%src_tot                            ! ii=sequential source number
          !! if modflow object and set for all channels
          if (ob(i)%obtyp_out(ii) == "cha" .and. ob(i)%obtypno_out(ii) <= 0) then
            !! set all incoming channel object data to modflow
            do ich = 1, sp_ob%chan
              kk = sp_ob1%chan + ich - 1
              rcv_sum(kk) = rcv_sum(kk) + 1                   ! setting sequential inflow number
              jj = rcv_sum(kk)                                ! jj=seqential receiving number
              ob(kk)%obj_in(jj) = i                           ! source object number (for receiving unit)
              ob(kk)%obtyp_in(jj) = ob(i)%typ
              ob(kk)%htyp_in(jj) = ob(i)%htyp_out(ii)
              ob(kk)%ihtyp_in(jj) = ob(i)%ihtyp_out(ii)
              ob(kk)%frac_in(jj) = ob(i)%frac_out(ii)
            end do
          else
            kk = ob(i)%obj_out(ii)                          ! kk=object number of outflow object
            rcv_sum(kk) = rcv_sum(kk) + 1                   ! setting sequential receiving hyd number
            jj = rcv_sum(kk)                                ! jj=seqential receiving number
            ob(kk)%obj_in(jj) = i                           ! source object number (for receiving unit)
            ob(kk)%obtyp_in(jj) = ob(i)%typ
            ob(kk)%obtypno_in(jj) = ob(i)%num
            ob(kk)%htyp_in(jj) = ob(i)%htyp_out(ii)
            ob(kk)%ihtyp_in(jj) = ob(i)%ihtyp_out(ii)
            ob(kk)%frac_in(jj) = ob(i)%frac_out(ii)
            ob(i)%rcvob_inhyd(ii) = jj
            !! for subbasins, set receiving objects for each element (need for parallelization order)
            if (ob(kk)%typ == "ru") then
              iru = ob(kk)%num
              do ielem = 1, ru_def(iru)%num_tot
                ielem_db = ru_def(iru)%num(ielem)
                kk = ru_elem(ielem_db)%obj
                ob(kk)%obj_in(jj) = i                       ! source object number (for receiving unit)
                ob(kk)%obtyp_in(jj) = ob(i)%typ
                ob(kk)%obtypno_in(jj) = ob(i)%props
                ob(kk)%htyp_in(jj) = ob(i)%htyp_out(ii)
                ob(kk)%ihtyp_in(jj) = ob(i)%ihtyp_out(ii)
                !! if routing over ru - the hru frac is included
                ob(kk)%frac_in(jj) = ob(i)%frac_out(ii) * ru_elem(ielem_db)%frac
                ob(i)%rcvob_inhyd(ii) = jj
              end do
            end if
          end if
        end do
      end do

      !! loop through again to set command (object) sequence
      cmdno = 0
      idone = 0
      iobj_tot = 0
      rcv_sum = 0
      iord = 1
      dfn_sum = 0
      
    do while (idone == 0)
        do i = 1, sp_ob%objs
        
        if (iord > 1000) then        
          open (9002,file="looping.con",recl = 8000)
          write (9002, *) "LOOPING.CON CHECKING INFINITE LOOPS"
          do iob = 1, sp_ob%objs
            if (ob(iob)%fired == 0 .and. ob(iob)%rcv_tot > 0) then
              kk=1
              write (9002, *) ob(iob)%typ, ob(iob)%num, ob(iob)%rcv_tot, (ob(iob)%obtyp_in(jj),  &
                                    ob(iob)%obtypno_in(jj), jj = 1, ob(iob)%rcv_tot)
            end if 
          end do
          write (*,1002)
          call exit(1)
        end if 
   
      
          !check if all incoming and defining objects have been met
          !if not sum incoming 
          if (rcv_sum(i) == ob(i)%rcv_tot .and.                        & 
                                      dfn_sum(i) == ob(i)%dfn_tot) then
            if (ob(i)%fired == 0) then
            ob(i)%fired = 1
            
                !! calculate drainage area to compare to input area - need reservoir surface area
                do ircv = 1, ob(i)%rcv_tot
                  if (ob(i)%obtyp_in(ircv) == "hru" .or. ob(i)%obtyp_in(ircv) == "ru" .or.      &
                        ob(i)%obtyp_in(ircv) == "chandeg" .or. ob(i)%obtyp_in(ircv) == "recall" &
                        .or. ob(i)%obtyp_in(ircv) == "res" .or. ob(i)%obtyp_in(ircv) == "outlet") then
                    !if (ircv == 1) then
                      ob1 = ob(i)%obj_in(ircv)
                      ob(i)%area_ha_calc = ob(i)%area_ha_calc + ob(ob1)%area_ha_calc * ob(i)%frac_in(ircv)
                    !else
                    !  do ob2 = 1, ircv
                    !    ob1 = ob(i)%obj_in(ircv)
                    !    if (ob(i)%obj_in(ircv) /= ob(i)%obj_in(ob2)) then
                    !      ob(i)%area_ha_calc = ob(i)%area_ha_calc + ob(ob1)%area_ha_calc * ob(i)%frac_in(ircv)
                    !    end if
                    !  end do
                    !end if
                  end if
                end do
                    
            iobj_tot = iobj_tot + 1    ! check to see if all objects are done
            if (iobj_tot == sp_ob%objs) idone = 1
            !sum defining units for each subbasin
            do k = 1, ob(i)%ru_tot
              dfn_sum(ob(i)%obj_subs(k)) = dfn_sum(ob(i)%obj_subs(k)) + 1
              !kk = ob(i)%obj_subs(k)       !ob number of subbasin
              !dfn_sum(kk) = dfn_sum(kk) + 1
            end do
          
            isrc_tot = Max(ob(i)%src_tot, 1)  !force to go through once
            do ii = 1, isrc_tot
              !! add receiving object for single objects
              if (ob(i)%src_tot > 0) then
                if (ob(i)%obtypno_out(ii) > 0) then
                  k = ob(i)%obj_out(ii)
                    rcv_sum(k) = rcv_sum(k) + 1
                    !! add subbasin elements
                    if (ob(k)%typ == "ru") then
                      iru = ob(k)%props
                      do jj = 1, ru_def(iru)%num_tot
                        ielem = ru_def(iru)%num(jj)
                        iob = ru_elem(ielem)%obj
                        rcv_sum(iob) = rcv_sum(iob) + 1
                      end do
                  end if
                end if
              else
                !! modflow - for all inflow objects
                if (ob(i)%typ == "modflow") then
                  iobtyp = ob(i)%obtyp_out(ii)
                  !! add recieving for all channels from modflow
                  if (iobtyp == "cha") then
                    ob1 = sp_ob1%chan
                    ob2 = sp_ob1%chan + sp_ob%chan - 1
                    do ich = ob1, ob2
                      rcv_sum(ich) = rcv_sum(ich) + 1
                    end do
                  end if
                end if
              end if
              
              !! compute object order for parallelization (similar to stream order)
              if (ob(i)%rcv_tot == 0 .and. ob(i)%typ /= "ru") then
                ob(i)%cmd_order = 1
              else
                iorder = 0
                !! compute object order of highest receiving object and add 1
                do ircv = 1, ob(i)%rcv_tot
                  ircv_ob = ob(i)%obj_in(ircv)
                  iorder = Max (iorder, ob(ircv_ob)%cmd_order)
                end do
                !! subbasin has to be in parallel order after elements in the subbasin 
                if (ob(i)%typ == "ru" .and. ob(i)%rcv_tot == 0) then
                  iorder = 1
                end if
                ob(i)%cmd_order = iorder + 1
              end if
              
              if (ob(i)%typ /= "exco") then   !exco"s are not commands
                ob(i)%cmd_prev = cmd_prev
                if (cmd_prev > 0) then
                  ob(cmd_prev)%cmd_next = i
                else
                  sp_ob1%objs = i  !first command number
                end if
                cmd_prev = i
                !rcv_sum(i) = rcv_sum(i) + 1
                ob(i)%cmd_order = iord
              end if  !exco"s are not commands
            
            end do
            end if
          end if
        end do
        iord = iord + 1
      end do

    !! write calculated and input drainage areas for all objects except hru's
      do iob = 1, sp_ob%objs
        if (ob(iob)%typ /= "hru" .and. ob(iob)%typ /= "ru") then
          write (9004,*) iob, ob(iob)%typ, ob(iob)%num, ob(iob)%area_ha, ob(iob)%area_ha_calc,             &
            ob(iob)%rcv_tot, (ob(iob)%obtyp_in(jj), ob(iob)%obtypno_in(jj), ob(iob)%obj_in(jj),             &
            ob(iob)%frac_in(jj), &
            ob(ob(iob)%obj_in(jj))%area_ha, ob(ob(iob)%obj_in(jj))%area_ha_calc, jj = 1, ob(iob)%rcv_tot)
        end if 
      end do
      
1002  format (5x,/,"ERROR - An infinite loop is detected in the connect file(s)",/, 15x, "the simulation will end",       &
                       /, 9x, "(review looping.con file for more info)",/)
      return
      end subroutine hyd_connect
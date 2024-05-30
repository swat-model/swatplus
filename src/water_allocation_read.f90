      subroutine water_allocation_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use sd_channel_module
      use conditional_module
      use hru_module, only : hru
      
      implicit none 
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: k                    !none       |counter
      integer :: isrc                 !none       |counter
      integer :: iwro                 !none       |number of water allocation objects
      integer :: num_objs
      integer :: idmd
      integer :: idb
      integer :: idb_irr
      integer :: ihru
      integer :: isrc_wallo
      integer :: div_found
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file=in_watrts%transfer_wro, exist=i_exist)
      if (.not. i_exist .or. in_watrts%transfer_wro == "null") then
        allocate (wallo(0:0))
      else
      do 
        open (107,file=in_watrts%transfer_wro)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%wallo_db = imax
        if (eof < 0) exit
        
        allocate (wallo(imax))
        allocate (wallod_out(imax))
        allocate (wallom_out(imax))
        allocate (walloy_out(imax))
        allocate (walloa_out(imax))

        do iwro = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) wallo(iwro)%name, wallo(iwro)%rule_typ, wallo(iwro)%src_obs, &
                                                    wallo(iwro)%dmd_obs, wallo(iwro)%cha_ob
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          num_objs = wallo(iwro)%src_obs
          allocate (wallo(iwro)%src(num_objs))
          num_objs = wallo(iwro)%dmd_obs
          allocate (wallo(iwro)%dmd(num_objs))
          allocate (wallod_out(iwro)%dmd(num_objs))
          allocate (wallom_out(iwro)%dmd(num_objs))
          allocate (walloy_out(iwro)%dmd(num_objs))
          allocate (walloa_out(iwro)%dmd(num_objs))
                    
          !! read source object data
          do isrc = 1, wallo(iwro)%src_obs
            read (107,*,iostat=eof) i
            wallo(iwro)%src(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%src(i)%ob_typ
            backspace (107)
            !! if source is diversion into the basin, read the recall name
            if (wallo(iwro)%src(i)%ob_typ == "div_in") then
              read (107,*,iostat=eof) k, wallo(iwro)%src(i)%ob_typ, wallo(iwro)%src(i)%div_rec
              !! xwalk with recall.rec
              do idb = 1, db_mx%recall_max
                if (wallo(iwro)%src(i)%div_rec == recall(idb)%name) then
                  wallo(iwro)%src(i)%rec_num = idb
                  exit
                end if
              end do
            else
              read (107,*,iostat=eof) k, wallo(iwro)%src(i)%ob_typ, wallo(iwro)%src(i)%ob_num,    &
                                                                  wallo(iwro)%src(i)%limit_mon
              !! call wallo_control from channel
              if (wallo(iwro)%src(i)%ob_typ == "cha") then
                sd_ch(wallo(iwro)%src(i)%ob_num)%wallo = iwro
                wallo(iwro)%cha = wallo(iwro)%src(i)%ob_num
              end if
            end if
          end do
          
          !! read demand object data
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do idmd = 1, num_objs
            read (107,*,iostat=eof) i
            wallo(iwro)%dmd(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%dmd(i)%ob_typ, wallo(iwro)%dmd(i)%ob_num,            &
              wallo(iwro)%dmd(i)%withdr, wallo(iwro)%dmd(i)%amount, wallo(iwro)%dmd(i)%right,           &
              wallo(iwro)%dmd(i)%treat_typ, wallo(iwro)%dmd(i)%treatment,  wallo(iwro)%dmd(i)%rcv_ob,   &
              wallo(iwro)%dmd(i)%rcv_num, wallo(iwro)%dmd(i)%rcv_dtl, num_objs
            allocate (wallo(iwro)%dmd(i)%src(num_objs))
            allocate (wallo(iwro)%dmd(i)%src_ob(num_objs))
            allocate (wallod_out(iwro)%dmd(i)%src(num_objs))
            allocate (wallom_out(iwro)%dmd(i)%src(num_objs))
            allocate (walloy_out(iwro)%dmd(i)%src(num_objs))
            allocate (walloa_out(iwro)%dmd(i)%src(num_objs))
            
            !! for hru irrigtion, need to xwalk with irrigation demand decision table
            if (wallo(iwro)%dmd(i)%ob_typ == "hru") then
              !! xwalk with lum decision table
              do idb = 1, db_mx%dtbl_lum
                if (wallo(iwro)%dmd(i)%withdr == dtbl_lum(idb)%name) then
                  ihru = wallo(iwro)%dmd(i)%ob_num
                  hru(ihru)%irr_dmd_dtbl = idb
                  do idb_irr = 1, db_mx%irrop_db 
                    if (dtbl_lum(idb)%act(1)%option == irrop_db(idb_irr)%name) then
                      wallo(iwro)%dmd(idmd)%irr_eff = irrop_db(idb_irr)%eff
                      wallo(iwro)%dmd(idmd)%surq = irrop_db(idb_irr)%surq
                      exit
                    end if
                  end do
                end if
              end do
            end if
            
            !! for municipal and divert demands, can use recall for daily, monthly, or annual withdrawals
            if (wallo(iwro)%dmd(i)%ob_typ == "muni" .or. wallo(iwro)%dmd(i)%ob_typ == "divert") then
              if (wallo(iwro)%dmd(i)%withdr /= "ave_day") then
                !! xwalk with recall database
                do idb = 1, db_mx%dtbl_flo
                  if (wallo(iwro)%dmd(i)%withdr == dtbl_flo(idb)%name) then
                    wallo(iwro)%dmd(i)%rec_num = idb
                    exit
                  end if
                end do
              end if
            end if
          
            !! for municipal treatment - recall option for daily, monthly, or annual mass
            if (wallo(iwro)%dmd(i)%treat_typ == "recall") then
              !! xwalk with recall database
              do idb = 1, db_mx%recall_max
                if (wallo(iwro)%dmd(i)%treatment == recall(idb)%name) then
                  wallo(iwro)%dmd(i)%trt_num = idb
                  exit
                end if
              end do
            end if
          
            !! for municipal treatment - treatment option for fraction of flow and ppm
            if (wallo(iwro)%dmd(i)%treat_typ == "treat") then
              !! xwalk with recall database
              do idb = 1, db_mx%trt_om
                if (wallo(iwro)%dmd(idmd)%treatment == trt_om_name(idb)) then
                  wallo(iwro)%dmd(i)%trt_num = idb
                  exit
                end if
              end do
            end if
            
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%dmd(i)%ob_typ, wallo(iwro)%dmd(i)%ob_num,    &
              wallo(iwro)%dmd(i)%withdr, wallo(iwro)%dmd(i)%amount, wallo(iwro)%dmd(i)%right,   &
              wallo(iwro)%dmd(i)%treat_typ, wallo(iwro)%dmd(i)%treatment,  wallo(iwro)%dmd(i)%rcv_ob,   &
              wallo(iwro)%dmd(i)%rcv_num, wallo(iwro)%dmd(i)%rcv_dtl,                           &
              wallo(iwro)%dmd(i)%dmd_src_obs, (wallo(iwro)%dmd(i)%src(isrc), isrc = 1, num_objs)

            !! set object name and number for each source for each demand object
            do isrc = 1, num_objs
              isrc_wallo = wallo(iwro)%dmd(i)%src(isrc)%src
              wallo(iwro)%dmd(i)%src_ob(isrc)%ob_typ = wallo(iwro)%src(isrc_wallo)%ob_typ
              wallo(iwro)%dmd(i)%src_ob(isrc)%ob_num = wallo(iwro)%src(isrc_wallo)%ob_num
            end do
            
            !! zero output variables for summing
            do isrc = 1, num_objs
              wallod_out(iwro)%dmd(i)%src(isrc) = walloz
              wallom_out(iwro)%dmd(i)%src(isrc) = walloz
              walloy_out(iwro)%dmd(i)%src(isrc) = walloz
              walloa_out(iwro)%dmd(i)%src(isrc) = walloz
            end do
            
          end do
          
          !if canal diversions are used as source water, read in the number of days that diversion water can be
          !available for irrigation (rtb)
          div_found = 0
          do isrc = 1, wallo(iwro)%src_obs
            if(wallo(iwro)%src(isrc)%ob_typ == "div") then
              div_found = 1
            endif
          enddo
          if(div_found == 1) then
            !prepare array
            allocate(div_volume_daily(sp_ob%recall))
            allocate(div_volume_total(sp_ob%recall))
            allocate(div_volume_used(sp_ob%recall))
            div_volume_daily = 0.
            div_volume_total = 0.
            div_volume_used = 0.
            !read the days parameter
            read(107,*)
            read(107,*) div_delay
            div_delay = Exp(-1./(div_delay + 1.e-6))
          endif
          
        end do

        exit
      end do
      end if
      close(107)

      return
      end subroutine water_allocation_read
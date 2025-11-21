      subroutine water_allocation_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use sd_channel_module
      use conditional_module
      use constituent_mass_module
      use hru_module, only : hru
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: k = 0                !none       |counter
      integer :: isrc = 0             !none       |counter
      integer :: jsrc = 0             !none       |counter  
      integer :: ircv = 0             !none       |counter
      integer :: iwro = 0             !none       |number of water allocation objects
      integer :: num_objs = 0
      integer :: num_src = 0
      integer :: num_rcv = 0
      integer :: itrn = 0
      integer :: idb = 0
      integer :: idb_irr = 0
      integer :: ihru = 0
      integer :: isrc_wallo = 0
      integer :: div_found = 0
      
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
        allocate (wal_omd(imax))
        allocate (wal_omm(imax))
        allocate (wal_omy(imax))
        allocate (wal_oma(imax))
        allocate (wallod_out(imax))
        allocate (wallom_out(imax))
        allocate (walloy_out(imax))
        allocate (walloa_out(imax))

        do iwro = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) wallo(iwro)%name, wallo(iwro)%rule_typ, wallo(iwro)%src_obs, &
            wallo(iwro)%trn_obs, wallo(iwro)%out_src, wallo(iwro)%out_rcv, wallo(iwro)%wtp,    &
            wallo(iwro)%uses, wallo(iwro)%stor, wallo(iwro)%pipe, wallo(iwro)%canal,           &
            wallo(iwro)%pump, wallo(iwro)%cha_ob
          
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          allocate (wuse_om_stor(wallo(iwro)%uses))
          allocate (wtp_om_stor(wallo(iwro)%wtp))
          allocate (wtow_om_stor(wallo(iwro)%stor))
          allocate (canal_om_stor(wallo(iwro)%canal))
          allocate (wuse_om_out(wallo(iwro)%uses))
          allocate (wtp_om_out(wallo(iwro)%wtp))
          allocate (wtow_om_out(wallo(iwro)%stor))
          allocate (canal_om_out(wallo(iwro)%canal))
          allocate (wuse_cs_stor(wallo(iwro)%uses))
          allocate (wtp_cs_stor(wallo(iwro)%wtp))
          allocate (wtow_cs_stor(wallo(iwro)%stor))
          allocate (canal_cs_stor(wallo(iwro)%canal))
          allocate (osrc_om_out(wallo(iwro)%out_src))
          num_objs = wallo(iwro)%src_obs
          allocate (wallo(iwro)%src(num_objs))
          num_objs = wallo(iwro)%trn_obs
          allocate (wallo(iwro)%trn(num_objs))
          allocate (wal_omd(iwro)%trn(num_objs))
          allocate (wal_omm(iwro)%trn(num_objs))
          allocate (wal_omy(iwro)%trn(num_objs))
          allocate (wal_oma(iwro)%trn(num_objs))
          allocate (wallod_out(iwro)%trn(num_objs))
          allocate (wallom_out(iwro)%trn(num_objs))
          allocate (walloy_out(iwro)%trn(num_objs))
          allocate (walloa_out(iwro)%trn(num_objs))
          
          allocate (wal_tr_omd(wallo(iwro)%wtp))
          allocate (wal_tr_omm(wallo(iwro)%wtp))
          allocate (wal_tr_omy(wallo(iwro)%wtp))
          allocate (wal_tr_oma(wallo(iwro)%wtp))
          allocate (wal_use_omd(wallo(iwro)%uses))
          allocate (wal_use_omm(wallo(iwro)%uses))
          allocate (wal_use_omy(wallo(iwro)%uses))
          allocate (wal_use_oma(wallo(iwro)%uses))
                    
          !! read source object data
          do isrc = 1, wallo(iwro)%src_obs
            read (107,*,iostat=eof) i
            wallo(iwro)%src(i)%num = i
            if (eof < 0) exit
            backspace (107)
              read (107,*,iostat=eof) k, wallo(iwro)%src(i)%ob_typ, wallo(iwro)%src(i)%ob_num,    &
                                      wallo(iwro)%src(i)%lim_typ, wallo(iwro)%src(i)%lim_name,    &
                                      (wallo(iwro)%src(i)%limit_mon(k), k=1,12)
          end do
          
          !! read demand object data
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do itrn = 1, num_objs
            read (107,*,iostat=eof) i
            wallo(iwro)%trn(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%trn(i)%trn_typ, wallo(iwro)%trn(i)%trn_typ_name,   &
              wallo(iwro)%trn(i)%amount, wallo(iwro)%trn(i)%right, wallo(iwro)%trn(i)%src_num,            &
              wallo(iwro)%trn(i)%dtbl_src, wallo(iwro)%trn(i)%num
            
            num_src = wallo(iwro)%trn(i)%src_num
            allocate (wallo(iwro)%trn(i)%src(num_src))
            allocate (wallo(iwro)%trn(i)%src_wal(num_src))
            wallo(iwro)%trn(i)%src_wal = 0
            allocate (wal_omd(iwro)%trn(i)%src(num_src))
            allocate (wal_omm(iwro)%trn(i)%src(num_src))
            allocate (wal_omy(iwro)%trn(i)%src(num_src))
            allocate (wal_oma(iwro)%trn(i)%src(num_src))
            allocate (wallod_out(iwro)%trn(i)%src(num_src))
            allocate (wallom_out(iwro)%trn(i)%src(num_src))
            allocate (walloy_out(iwro)%trn(i)%src(num_src))
            allocate (walloa_out(iwro)%trn(i)%src(num_src))
            
            !! for hru irrigation, need to xwalk with irrigation demand decision table
            if (wallo(iwro)%trn(i)%trn_typ == "dtbl_irr") then
              !! xwalk with lum decision table
              do idb = 1, db_mx%dtbl_lum
                if (wallo(iwro)%trn(i)%trn_typ_name == dtbl_lum(idb)%name) then
                  ihru = wallo(iwro)%trn(i)%rcv%num
                  hru(ihru)%irr_trn_dtbl = idb
                  do idb_irr = 1, db_mx%irrop_db
                    if (dtbl_lum(idb)%act(1)%option == irrop_db(idb_irr)%name) then
                      wallo(iwro)%trn(itrn)%irr_eff = irrop_db(idb_irr)%eff
                      wallo(iwro)%trn(itrn)%surq = irrop_db(idb_irr)%surq
                      exit
                    end if
                  end do
                end if
              end do
            end if
            
            !! for wallo demand amount, source available, and source and receiving allocating
            !! xwalk with flow control decision table
            if (wallo(iwro)%trn(i)%trn_typ == "dtbl_con") then
              !! xwalk with flo control decision table
              do idb = 1, db_mx%dtbl_flo
                if (wallo(iwro)%trn(i)%trn_typ_name == dtbl_flo(idb)%name) then
                  wallo(iwro)%trn(itrn)%dtbl_num = idb
                  exit
                end if
              end do
            end if
            
            !! for municipal treatment - recall option for daily, monthly, or annual mass
            if (wallo(iwro)%trn(i)%trn_typ == "recall") then
              !! xwalk with recall database
              do idb = 1, db_mx%recall_max
                if (wallo(iwro)%trn(i)%trn_typ_name == recall(idb)%name) then
                  wallo(iwro)%trn(i)%rec_num = idb
                  exit
                end if
              end do
            end if
            
            backspace (107)
            backspace (107)
            !read (107,*,iostat=eof) k
            read (107,*,iostat=eof) k, wallo(iwro)%trn(i)%trn_typ, wallo(iwro)%trn(i)%trn_typ_name,   &
              wallo(iwro)%trn(i)%amount, wallo(iwro)%trn(i)%right, wallo(iwro)%trn(i)%src_num,        &
              wallo(iwro)%trn(i)%dtbl_src, & !wallo(iwro)%trn(i)%num,                                 &
              (wallo(iwro)%trn(i)%src(isrc), isrc = 1, num_src), wallo(iwro)%trn(i)%rcv
            
            !! set src_wal links to main source objects
            do isrc = 1, num_src
              !! find the corresponding source object in the main source list
              do jsrc = 1, wallo(iwro)%src_obs
                if (wallo(iwro)%trn(i)%src(isrc)%typ == wallo(iwro)%src(jsrc)%ob_typ .and.    &
                      wallo(iwro)%trn(i)%src(isrc)%num == wallo(iwro)%src(jsrc)%ob_num) then
                  wallo(iwro)%trn(i)%src_wal(isrc) = jsrc
                  exit
                end if
              end do
            end do
            
            !! zero output variables for summing
            do isrc = 1, num_src
              wallod_out(iwro)%trn(i)%src(isrc) = walloz
              wallom_out(iwro)%trn(i)%src(isrc) = walloz
              walloy_out(iwro)%trn(i)%src(isrc) = walloz
              walloa_out(iwro)%trn(i)%src(isrc) = walloz
            end do
            
          end do
          
        end do

        exit
      end do
      end if
      close(107)

      return
    end subroutine water_allocation_read
    
    
    subroutine water_treatment_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: iwtp = 0             !none       |number of water treatment objects
      integer :: iom = 0              !none       |counter
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='water_treat.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_treat.wal' == "null") then
        allocate (wtp(0:0))
      else
      do 
        open (107,file='water_treat.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%water_treat = imax
        if (eof < 0) exit
        
        allocate (wtp(imax))

        do iwtp = 1, imax
          read (107,*,iostat=eof) i, wtp(iwtp)%name, wtp(iwtp)%stor_mx,    &
                                            wtp(iwtp)%lag_days, wtp(iwtp)%loss_fr, &
                                            wtp(iwtp)%org_min, wtp(iwtp)%pests, &
                                            wtp(iwtp)%paths, wtp(iwtp)%salts, &
                                            wtp(iwtp)%constit, wtp(iwtp)%descrip
          if (eof < 0) exit
          
          !! crosswalk organic mineral with 
          do iom = 1, db_mx%om_treat
            if (om_treat_name(iom) == wtp(iwtp)%org_min) then
              wtp(iwtp)%iorg_min = iom
              exit
            end if
          end do
            
          !! read pseticide concentrations of treated water
          if (cs_db%num_pests > 0) then
            allocate (wtp_cs_treat(iwtp)%pest(cs_db%num_pests))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) wtp_cs_treat(iwtp)%pest
          end if
          
          !! read pathogen concentrations of treated water
          if (cs_db%num_paths > 0) then
            allocate (wtp_cs_treat(iwtp)%path(cs_db%num_paths))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) wtp_cs_treat(iwtp)%path
          end if
        end do
          
        exit
      end do
      end if
      close(107)

      return
    end subroutine water_treatment_read
    
    
    subroutine water_use_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      use sd_channel_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!         |title of file
      character (len=80) :: header = "" !         |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: iwuse = 0            !none       |number of water treatment objects
      integer :: iom = 0              !none       |counter
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='water_use.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_use.wal' == "null") then
        allocate (wuse(0:0))
      else
      do 
        open (107,file='water_use.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%water_use = imax
        if (eof < 0) exit
        
        allocate (wuse(imax))

        do iwuse = 1, imax
          read (107,*,iostat=eof) i, wuse(iwuse)%name, wuse(iwuse)%stor_mx,     &
                                     wuse(iwuse)%lag_days, wuse(iwuse)%loss_fr, &
                                     wuse(iwuse)%org_min, wuse(iwuse)%pests,    &
                                     wuse(iwuse)%paths, wuse(iwuse)%salts,      &
                                     wuse(iwuse)%constit, wuse(iwuse)%descrip
          if (eof < 0) exit
          
          !! crosswalk organic mineral with 
          do iom = 1, db_mx%om_use
            if (om_use_name(iom) == wuse(iwuse)%org_min) then
              wuse(iwuse)%iorg_min = iom
              exit
            end if
          end do
            
          !! read pseticide concentrations of treated water
          if (cs_db%num_pests > 0) then
            allocate (wuse_cs_efflu(iwuse)%pest(cs_db%num_pests))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) wuse_cs_efflu(iwuse)%pest
          end if
          
          !! read pathogen concentrations of treated water
          if (cs_db%num_paths > 0) then
            allocate (wuse_cs_efflu(iwuse)%path(cs_db%num_paths))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) wuse_cs_efflu(iwuse)%path
          end if
        end do
          
        exit
      end do
      end if
      close(107)

      return
    end subroutine water_use_read
    
    
    subroutine water_tower_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: iwtow = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='water_tower.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_tower.wal' == "null") then
        if (.not. allocated(wtow)) then
          allocate (wtow(0:0))
        end if
      else
      do 
        open (107,file='water_tower.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        !db_mx%water_treat = imax
        if (eof < 0) exit
        
        if (.not. allocated(wtow)) then
          allocate (wtow(imax))
        end if

        do iwtow = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit 
          read (107,*,iostat=eof) i, wtow(iwtow)%name, wtow(iwtow)%stor_mx,         &
                                        wtow(iwtow)%lag_days, wtow(iwtow)%loss_fr
          if (eof < 0) exit
        end do
      end do
      end if
      close(107)

      return
    end subroutine water_tower_read
      
      
    subroutine water_pipe_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: ipipe = 0
      integer :: num_aqu = 0
      integer :: iaq = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='water_pipe.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_pipe.wal' == "null") then
        allocate (pipe(0:0))
      else
      do 
        open (107,file='water_pipe.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        !db_mx%water_treat = imax
        if (eof < 0) exit
        
        allocate (pipe(imax))

        do ipipe = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit 
          read (107,*,iostat=eof) i, pipe(ipipe)%name, pipe(ipipe)%stor_mx,                &
                                     pipe(ipipe)%lag_days, pipe(ipipe)%loss_fr, num_aqu
          if (eof < 0) exit
          
          !! allocate and read aquifer loss data
          allocate (pipe(ipipe)%aqu_loss(num_aqu))
          
          read (107,*,iostat=eof) i, pipe(ipipe)%name, pipe(ipipe)%stor_mx, pipe(ipipe)%lag_days,   &
                pipe(ipipe)%loss_fr, pipe(ipipe)%num_aqu, (pipe(ipipe)%aqu_loss(iaq), iaq = 1, num_aqu)
        end do
        
      end do
      end if
      
      close(107)

      return
    end subroutine water_pipe_read
    
    
    subroutine om_treat_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: iom_tr = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='om_treat.wal', exist=i_exist)
      if (.not. i_exist .or. 'om_treat.wal' == "null") then
        allocate (wtp_om_treat(0:0))
        allocate (om_treat_name(0:0))
      else
      do 
        open (107,file='om_treat.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%om_treat = imax
        if (eof < 0) exit
        
        allocate (wtp_om_treat(imax))
        allocate (om_treat_name(imax))

        do iom_tr = 1, imax
          read (107,*,iostat=eof) om_treat_name(iom_tr), wtp_om_treat(iom_tr)
        end do
      end do
      end if
      
      close(107)

      return
    end subroutine om_treat_read
    
    
    subroutine om_use_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: iom_use = 0          !none       |
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='om_use.wal', exist=i_exist)
      if (.not. i_exist .or. 'om_use.wal' == "null") then
        allocate (wuse_om_efflu(0:0))
        allocate (om_use_name(0:0))
      else
      do 
        open (107,file='om_use.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%om_use = imax
        if (eof < 0) exit
        
        allocate (wuse_om_efflu(imax))
        allocate (om_use_name(imax))

        do iom_use = 1, imax
          if (eof < 0) exit 
          read (107,*,iostat=eof) om_use_name(iom_use), wuse_om_efflu(iom_use)
        end do
      end do
      end if
      
      close(107)

      return
      end subroutine om_use_read
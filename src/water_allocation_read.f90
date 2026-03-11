      subroutine water_allocation_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use sd_channel_module
      use conditional_module
      use constituent_mass_module
      use recall_module
      use exco_module
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
      integer :: iwro = 0             !none       |number of water allocation objects
      integer :: num_objs = 0
      integer :: num_src = 0
      integer :: itrn = 0
      integer :: idb = 0
      integer :: idb_irr = 0
      integer :: ihru = 0
      integer :: iexco = 0
      integer :: iexco_om = 0
      integer :: irec = 0
      integer :: iom = 0
      
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
          read (107,*,iostat=eof) wallo(iwro)%name, wallo(iwro)%rule_typ, wallo(iwro)%trn_obs
          
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
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
          
              
          !! read transfer object data
          if (eof < 0) exit
          do itrn = 1, num_objs
            read (107,*,iostat=eof) i
            wallo(iwro)%trn(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%trn(i)%trn_typ, wallo(iwro)%trn(i)%trn_typ_name,   &
                    wallo(iwro)%trn(i)%amount, wallo(iwro)%trn(i)%right, wallo(iwro)%trn(i)%src_num
          
            num_src = wallo(iwro)%trn(i)%src_num
            allocate (wallo(iwro)%trn(i)%src(num_src))
            allocate (wallo(iwro)%trn(i)%osrc(num_src))
            allocate (wal_omd(iwro)%trn(i)%src(num_src))
            allocate (wal_omm(iwro)%trn(i)%src(num_src))
            allocate (wal_omy(iwro)%trn(i)%src(num_src))
            allocate (wal_oma(iwro)%trn(i)%src(num_src))
            allocate (wallod_out(iwro)%trn(i)%src(num_src))
            allocate (wallom_out(iwro)%trn(i)%src(num_src))
            allocate (walloy_out(iwro)%trn(i)%src(num_src))
            allocate (walloa_out(iwro)%trn(i)%src(num_src))
            
            !! for hru irrigation, need to xwalk with irrigation demand decision table
            if (wallo(iwro)%trn(i)%trn_typ == "dtbl_lum") then
              !! xwalk with lum decision table
              do idb = 1, db_mx%dtbl_lum
                if (wallo(iwro)%trn(i)%trn_typ_name == dtbl_lum(idb)%name) then
                  ihru = wallo(iwro)%trn(i)%rcv%num
                  wallo(iwro)%trn(itrn)%dtbl_lum = idb
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
            
            backspace (107)
            !read (107,*,iostat=eof) k
            read (107,*,iostat=eof) k, wallo(iwro)%trn(i)%trn_typ, wallo(iwro)%trn(i)%trn_typ_name,   &
              wallo(iwro)%trn(i)%amount, wallo(iwro)%trn(i)%right, wallo(iwro)%trn(i)%src_num,        &
              wallo(iwro)%trn(i)%dtbl_src, & !wallo(iwro)%trn(i)%num,                                 &
              (wallo(iwro)%trn(i)%src(isrc), isrc = 1, num_src), wallo(iwro)%trn(i)%rcv
          
            !! check if a channel is a source
            do isrc = 1, num_src
              if (wallo(iwro)%trn(i)%src(isrc)%typ == "cha") then
                wallo(iwro)%trn(i)%ch_src = wallo(iwro)%trn(i)%src(isrc)%num
                exit
              end if
            end do
        
            !! xwalk with recall file to get sequential number
            do isrc = 1, num_src
              irec = wallo(iwro)%trn(i)%src(isrc)%num
              if (wallo(iwro)%trn(i)%src(isrc)%typ == "osrc") then
                wallo(iwro)%trn(i)%osrc(isrc)%daymoyr  = recall_db(irec)%iorg_min
                exit
              end if
            end do
                
            !! xwalk with exco file to get sequential number
            do isrc = 1, num_src
              if (wallo(iwro)%trn(i)%src(isrc)%typ == "osrc_a") then
                iexco = wallo(iwro)%trn(i)%src(isrc)%num
                do iexco_om = 1, db_mx%exco_om
                  if (exco_db(iexco)%om_file == exco_om_name(iexco_om)) then
                    wallo(iwro)%trn(i)%osrc(isrc)%aa = iexco_om
                    exit
                  end if
                end do
              end if
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
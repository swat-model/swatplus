      subroutine water_use_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      use sd_channel_module
      use recall_module
      use conditional_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!         |title of file
      character (len=80) :: header = "" !         |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iwuse = 0            !none       |number of water treatment objects
      integer :: iom = 0              !none       |counter
      integer :: idb = 0              !none       |data file number
      integer :: lev                  !none       |level for concentration - typically only release at 1 level on a day
      
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
        db_mx%uses = imax
        if (eof < 0) exit
        
        !! allocate wuse and read the water use data
        allocate (wuse(imax))
        do iwuse = 1, imax
          lev = 1
          allocate (wuse(iwuse)%conc(lev))
          read (107,*,iostat=eof) wuse(iwuse)%name, wuse(iwuse)%stor_mx,wuse(iwuse)%lag_days,             &
              wuse(iwuse)%loss_fr, wuse(iwuse)%conc(lev)%org_min_typ, wuse(iwuse)%conc(lev)%org_min_name, &
              wuse(iwuse)%conc(lev)%pests_typ, wuse(iwuse)%conc(lev)%pests_name,                          &
              wuse(iwuse)%conc(lev)%paths_typ, wuse(iwuse)%conc(lev)%paths_name,                          &
              wuse(iwuse)%conc(lev)%salts_typ, wuse(iwuse)%conc(lev)%salts_name,                          &
              wuse(iwuse)%conc(lev)%constit_typ, wuse(iwuse)%conc(lev)%constit_name,                      &
              wuse(iwuse)%descrip
          if (eof < 0) exit
          
          !! determine water use concentration id number for organic mineral
            select case (wuse(iwuse)%conc(lev)%org_min_typ)
            case ("const")
              !! om_num - number of the organic mineral in water treatment database (water_treat.wal)
              do iom = 1, db_mx%om_use
                if (om_use_name(iom) == wuse(iwuse)%conc(lev)%org_min_name) then
                  wuse(iwuse)%conc(lev)%om_num = iom
                  exit
                end if
              end do
                
            case ("dtbl")
              !! xwalk with flow control decision table
              do idb = 1, db_mx%dtbl_flo
                if (wuse(iwuse)%conc(lev)%org_min_name == dtbl_flo(idb)%name) then
                  wuse(iwuse)%conc(lev)%om_num = idb
                  exit
                end if
              end do
              
            case ("recall")
              !! xwalk with flow control decision table
              do idb = 1, db_mx%recallom_max
                if (wuse(iwuse)%conc(lev)%org_min_name == recall_db(idb)%org_min) then
                  wuse(iwuse)%conc(lev)%om_num = idb
                  exit
                end if
              end do
            end select

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
          
        exit
      end do   ! iwuse = 1, imax
    end do
    end if
    close(107)

    return
    end subroutine water_use_read
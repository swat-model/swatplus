      subroutine water_treatment_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      use conditional_module
      use recall_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iwtp = 0             !none       |number of water treatment plant
      integer :: itrt = 0             !none       |number of treatments
      integer :: iom = 0              !none       |counter
      integer :: idb = 0              !none       |number of flow control decision table
      
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
        db_mx%treat = imax
        if (eof < 0) exit
        
        !! allocate wtp and read the water treatment plant data
        allocate (wtp(imax))
        do iwtp = 1, imax
          read (107,*,iostat=eof) wtp(iwtp)%name, wtp(iwtp)%stor_mx,        &
              wtp(iwtp)%lag_days, wtp(iwtp)%loss_fr, wtp(iwtp)%num_treats
          
          allocate (wtp(iwtp)%conc(wtp(iwtp)%num_treats))

          do itrt = 1, wtp(iwtp)%num_treats
            read (107,*,iostat=eof) wtp(iwtp)%conc(itrt)%org_min_typ, wtp(iwtp)%conc(itrt)%org_min_name, &
              wtp(iwtp)%conc(itrt)%pests_typ, wtp(iwtp)%conc(itrt)%pests_name,                           &
              wtp(iwtp)%conc(itrt)%paths_typ, wtp(iwtp)%conc(itrt)%paths_name,                           &
              wtp(iwtp)%conc(itrt)%salts_typ, wtp(iwtp)%conc(itrt)%salts_name,                           &
              wtp(iwtp)%conc(itrt)%constit_typ, wtp(iwtp)%conc(itrt)%constit_name,                       &
              wtp(iwtp)%descrip
          
          if (eof < 0) exit
          
          !! crosswalk organic mineral with water treatment database (water_treat.wal)
              select case (wtp(iwtp)%conc(itrt)%org_min_typ)
              case ("const")
                !! om_num - number of the organic mineral in water treatment database (water_treat.wal)
                do iom = 1, db_mx%om_treat
                  if (om_treat_name(iom) == wtp(iwtp)%conc(itrt)%org_min_name) then
                    wtp(iwtp)%conc(itrt)%om_num = iom
                    exit
                  end if
                end do
                
              case ("dtbl")
              !! xwalk with flow control decision table
              do idb = 1, db_mx%dtbl_flo
                if (wtp(iwtp)%conc(itrt)%org_min_name == dtbl_flo(idb)%name) then
                  wtp(iwtp)%conc(itrt)%om_num = idb
                  exit
                end if
              end do
              
              case ("recall")
              !! xwalk with flow control decision table
              do idb = 1, db_mx%recallom_max
                if (wtp(iwtp)%conc(itrt)%org_min_name == recall_db(idb)%org_min) then
                  wtp(iwtp)%conc(itrt)%om_num = idb
                  exit
                end if
              end do
              end select
            
          !! decision table for adjustment of organic mineral concentrations
            if (wtp(iwtp)%conc(itrt)%org_min_name /= "null") then
              !! xwalk with con decision table
              do idb = 1, db_mx%dtbl_flo
                if (wtp(iwtp)%conc(itrt)%org_min_name == dtbl_flo(idb)%name) then
                  wtp(iwtp)%conc(itrt)%om_num = idb
                  exit
                end if
              end do
            end if
          
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
          
          end do   ! itrt = 1, wtp(iwtp)%num_treats
        end do    ! iwtp = 1, imax
        
      end do
      end if
      close(107)

      return
      end subroutine water_treatment_read
    subroutine water_osrc_read
      
      use input_file_module
      use water_allocation_module
      use recall_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      use sd_channel_module
      use conditional_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!         |title of file
      character (len=80) :: header = "" !         |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: isrc = 0             !none       |number of water treatment objects
      integer :: iom = 0              !none       |counter
      integer :: idb = 0              !none       |counter
      integer :: lev                  !none       |level for concentration - typically only release at 1 level on a day
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs
      inquire (file='outside_src.wal', exist=i_exist)
      if (.not. i_exist .or. 'outside_src.wal' == "null") then
        allocate (osrc(0:0))
      else
      do 
        open (107,file='outside_src.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%out_src = imax
        if (eof < 0) exit
        
        allocate (osrc(imax))

        do isrc = 1, imax
          lev = 1
          allocate (osrc(isrc)%conc(lev))
          read (107,*,iostat=eof) osrc(isrc)%name,                                          &
              osrc(isrc)%conc(lev)%org_min_typ, osrc(isrc)%conc(lev)%org_min_name,          &
              osrc(isrc)%conc(lev)%pests_typ, osrc(isrc)%conc(lev)%pests_name,              &
              osrc(isrc)%conc(lev)%paths_typ, osrc(isrc)%conc(lev)%paths_name,              &
              osrc(isrc)%conc(lev)%salts_typ, osrc(isrc)%conc(lev)%salts_name,              &
              osrc(isrc)%conc(lev)%constit_typ, osrc(isrc)%conc(lev)%constit_name,          &
              osrc(isrc)%descrip
          if (eof < 0) exit
                    
        !! determine water use concentration id number for organic mineral
          select case (osrc(isrc)%conc(lev)%org_min_typ)
          case ("const")
            !! om_num - number of the organic mineral in water treatment database (water_treat.wal)
            do iom = 1, db_mx%om_src
              if (om_osrc_name(iom) == osrc(isrc)%conc(lev)%org_min_name) then
                osrc(isrc)%conc(lev)%om_num = iom
                exit
              end if
            end do
                
          case ("dtbl")
            !! xwalk with flow control decision table
            do idb = 1, db_mx%dtbl_flo
              if (osrc(isrc)%conc(lev)%org_min_name == dtbl_flo(idb)%name) then
                osrc(isrc)%conc(lev)%om_num = idb
                exit
              end if
            end do
              
          case ("recall")
            !! xwalk with flow control decision table
            do idb = 1, db_mx%recallom_max
              if (osrc(isrc)%conc(lev)%org_min_name == recall_db(idb)%org_min) then
                osrc(isrc)%conc(lev)%om_num = idb
                exit
              end if
            end do
          end select
            
          !! read pseticide concentrations of treated water
          if (cs_db%num_pests > 0) then
            allocate (osrc_cs(isrc)%pest(cs_db%num_pests))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) osrc_cs(isrc)%pest
          end if
          
          !! read pathogen concentrations of treated water
          if (cs_db%num_paths > 0) then
            allocate (osrc_cs(isrc)%path(cs_db%num_paths))
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) osrc_cs(isrc)%path
          end if
          
        end do   ! isrc = 1, imax
      end do
      end if
      close(107)

      return
      end subroutine water_osrc_read
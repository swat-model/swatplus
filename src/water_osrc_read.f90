    subroutine water_osrc_read
      
      use input_file_module
      use water_allocation_module
      use recall_module
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
      integer :: isrc = 0             !none       |number of water treatment objects
      integer :: iom = 0              !none       |counter
      integer :: irec = 0             !none       |counter
      
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
        db_mx%outside_src = imax
        if (eof < 0) exit
        
        allocate (osrc(imax))

        do isrc = 1, imax
          read (107,*,iostat=eof) i, osrc(isrc)%name, osrc(isrc)%stor_mx,     &
                                     osrc(isrc)%lag_days, osrc(isrc)%loss_fr
          if (eof < 0) exit
        end do
          
        do isrc = 1, imax
          !! crosswalk organic mineral with 
          do irec = 1, db_mx%recalldb_max
            if (osrc(isrc)%name == recall_db(irec)%name) then
              do iom = 1, db_mx%recall_max
                if (recall_db(irec)%org_min%name == recall(iom)%filename) then
                  osrc(isrc)%iorg_min = iom
                  exit
                end if
              end do
            end if
          end do
            
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
          
        end do
      end do
      end if
      close(107)

      return
    end subroutine water_osrc_read

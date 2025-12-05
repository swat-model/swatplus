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
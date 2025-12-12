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
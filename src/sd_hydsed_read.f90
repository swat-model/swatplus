      subroutine sd_hydsed_read
      
      use input_file_module
      use sd_channel_module
      use channel_velocity_module
      use maximum_data_module
      use hydrograph_module
      use time_module
      
      implicit none      
      
      character (len=80) :: titldum   !             |title of file
      character (len=80) :: header    !             |header of file
      integer :: eof                  !             |end of file
      integer :: imax                 !none         |determine max number for array (imax) and total number in file
      logical :: i_exist              !none         |check to determine if file exists
      integer :: idb                  !             |
      integer :: ts_sed               !none         |time step for channel sediment routing
      
      eof = 0
      imax = 0
      maxint = 10
      
      ts_sed = Max (10, time%step)
      allocate (timeint(ts_sed))
      allocate (hyd_rad(ts_sed))
      allocate (trav_time(ts_sed))
      allocate (flo_dep(ts_sed))
      
      inquire (file=in_cha%hyd_sed, exist=i_exist)
      if (.not. i_exist .or. in_cha%hyd_sed == "null") then
        allocate (sd_chd(0:0))
      else
      do
        open (1,file=in_cha%hyd_sed)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (1,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do  
          
        db_mx%ch_lte = imax
           
        allocate (sd_chd(0:imax))
        
        !rtb floodplain
        !allocate(flood_freq(imax))
        !flood_freq = 0

        rewind (1)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
        
        do idb = 1, db_mx%ch_lte
          read (1,*,iostat=eof) sd_chd(idb)
          if (eof < 0) exit
        end do

        exit
      end do
      end if

      inquire (file="sed_nut.cha", exist=i_exist)
      if (.not. i_exist .or. "sed_nut.cha" == "null") then
        allocate (sd_chd1(1))
      else
      do
        open (1,file="sed_nut.cha")
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (1,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do  
          
        db_mx%ch_sednut = imax
           
        allocate (sd_chd1(0:imax))

        rewind (1)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
        
        do idb = 1, db_mx%ch_sednut
          read (1,*,iostat=eof) sd_chd1(idb)
          if (eof < 0) exit
        end do

        exit
      end do
      end if

      close (1)
      return
      end subroutine sd_hydsed_read
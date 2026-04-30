      subroutine water_canal_read
      
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
      integer :: ic = 0
      integer :: num_aqu = 0
      integer :: iaq = 0
      
      eof = 0
      imax = 0
      
      !! read water canal inputs

      inquire (file='water_canal.wal', exist=i_exist)
      if (.not. i_exist .or. 'water_canal.wal' == "null") then
        allocate (canal(0:0))
      else
      do 
        open (107,file='water_canal.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%canal = imax
        if (eof < 0) exit
        
        allocate (canal(imax))
        allocate (canal_om_stor(imax))
        allocate (canal_om_out(imax))
        allocate (canal_cs_stor(imax))

        do ic = 1, imax
          read (107,*,iostat=eof) i, canal(ic)%name, canal(ic)%w_sta, canal(ic)%init, canal(ic)%dtbl,       &
              canal(ic)%ddown_days, canal(ic)%w, canal(ic)%d, canal(ic)%s, canal(ic)%ss, canal(ic)%sat_con, &
              canal(ic)%loss_fr, canal(ic)%bed_thick, canal(ic)%div_id, canal(ic)%day_beg, canal(ic)%day_end, &
              num_aqu
          if (eof < 0) exit
          backspace (107)

          !! allocate and read aquifer loss data
          allocate (canal(ic)%aqu_loss(num_aqu))

          read (107,*,iostat=eof) i, canal(ic)%name, canal(ic)%w_sta, canal(ic)%init, canal(ic)%dtbl,       &
              canal(ic)%ddown_days, canal(ic)%w, canal(ic)%d, canal(ic)%s, canal(ic)%ss, canal(ic)%sat_con, &
              canal(ic)%loss_fr, canal(ic)%bed_thick, canal(ic)%div_id, canal(ic)%day_beg, canal(ic)%day_end, &
              canal(ic)%num_aqu, (canal(ic)%aqu_loss(iaq), iaq = 1, num_aqu)
          
          !! crosswalk with weather station
          
          !! crosswalk initial concentrations and decision table
        end do
        
      end do
      end if
      
      close(107)

      return
      end subroutine water_canal_read
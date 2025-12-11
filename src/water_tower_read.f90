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
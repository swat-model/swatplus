    subroutine om_osrc_read
      
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

      integer :: iom_osrc = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='om_osrc.wal', exist=i_exist)
      if (.not. i_exist .or. 'om_osrc.wal' == "null") then
        allocate (osrc_om(0:0))
        allocate (om_osrc_name(0:0))
      else
      do 
        open (107,file='om_osrc.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%om_treat = imax
        if (eof < 0) exit
        
        allocate (osrc_om(imax))
        allocate (om_osrc_name(imax))

        do iom_osrc = 1, imax
          read (107,*,iostat=eof) om_osrc_name(iom_osrc), osrc_om(iom_osrc)
        end do
      end do
      end if
      
      close(107)

      return
    end subroutine om_osrc_read

    subroutine water_orcv_read
      
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
      integer :: ircv = 0             !none       |number of water treatment objects
      integer :: iom = 0              !none       |counter
      integer :: irec = 0             !none       |counter
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='outside_rcv.wal', exist=i_exist)
      if (.not. i_exist .or. 'outside_rcv.wal' == "null") then
        allocate (orcv(0:0))
      else
      do 
        open (107,file='outside_rcv.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%out_rcv = imax
        if (eof < 0) exit
        
        allocate (orcv(imax))

        do ircv = 1, imax
          read (107,*,iostat=eof) i, orcv(ircv)%name, orcv(ircv)%filename
          if (eof < 0) exit
        end do

      end do
      end if
      close(107)

      return
    end subroutine water_orcv_read
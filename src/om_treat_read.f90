      subroutine om_treat_read
      
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
      integer :: iom_tr = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file='om_treat.wal', exist=i_exist)
      if (.not. i_exist .or. 'om_treat.wal' == "null") then
        allocate (wtp_om_treat(0:0))
        allocate (om_treat_name(0:0))
      else
      do 
        open (107,file='om_treat.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%om_treat = imax
        if (eof < 0) exit
        
        allocate (wtp_om_treat(imax))
        allocate (om_treat_name(imax))

        do iom_tr = 1, imax
          read (107,*,iostat=eof) om_treat_name(iom_tr), wtp_om_treat(iom_tr)
        end do
      end do
      end if
      
      close(107)

      return
    end subroutine om_treat_read
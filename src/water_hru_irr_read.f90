      subroutine water_hru_irr_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""  !           |title of file
      character (len=80) :: header = ""   !           |header of file
      integer :: eof = 0                  !           |end of file
      integer :: imax = 0                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist                  !none       |check to determine if file exists
      integer :: j = 0                    !none       |hru number for irrigation                
      integer :: irr = 0
      integer :: num_hru = 0
      
      eof = 0
      imax = 0
      
      !! read hru irrigation data inputs

      inquire (file='water_hru_irr.wal', exist=i_exist)
      if (i_exist) then
      do 
        open (107,file='water_hru_irr.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        read (107,*,iostat=eof) header
        db_mx%hru_irr = imax
        if (eof < 0) exit
        
        allocate (hruirr_db(imax))

        do irr = 1, imax
          read (107,*,iostat=eof) hruirr_db(irr)%name, hruirr_db(irr)%hrus
          num_hru = hruirr_db(irr)%hrus
          allocate (hruirr_db(irr)%hru_num(num_hru))
          allocate (hruirr_db(irr)%dtbl_lum(num_hru))
          do j = 1, num_hru
            read (107,*,iostat=eof) hruirr_db(irr)%hru_num(j), hruirr_db(irr)%dtbl_lum(j)
          end do
          if (eof < 0) exit
        end do
        
      end do
      end if
      
      close(107)

      return
      end subroutine water_hru_irr_read
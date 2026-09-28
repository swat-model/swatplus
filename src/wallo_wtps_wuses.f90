      subroutine wallo_wtps_wuses
      
      use input_file_module
      use water_allocation_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=80) :: titldum = ""!         |title of file
      integer :: eof = 0              !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iwtp_db = 0          !none       |water treatment plant database number
      integer :: iwuse_db = 0         !none       |water use database number
      integer :: iwtp = 0             !none       |water treatment plant number
      integer :: iwuse = 0            !none       |water use number
      character (len=25), dimension(:), allocatable :: wtp_name       !water treatment plant name
      character (len=25), dimension(:), allocatable :: wuse_name      !water use name
      
      eof = 0
      
      !! read water allocation inputs
      inquire (file='wtps_wuses.wal', exist=i_exist)
      if (i_exist .or. 'wtps_wuses.wal' /= "null") then
      do 
        open (107,file='wtps_wuses.wal')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        
        !! read the number of objects
        read (107,*,iostat=eof) wtps, wuses
         
        allocate (wtp_name(wtps))
        allocate (wuse_name(wuses))
        
        allocate (wtp_om_stor(wtps))
        allocate (wtp_cs_stor(wtps))
        allocate (wtp_om_out(wtps))
        allocate (wal_tr_omd(wtps))
        allocate (wal_tr_omm(wtps))
        allocate (wal_tr_omy(wtps))
        allocate (wal_tr_oma(wtps))
         
        allocate (wuse_om_stor(wuses))
        allocate (wuse_om_out(wuses))
        allocate (wuse_cs_stor(wuses))
        allocate (wal_use_omd(wuses))
        allocate (wal_use_omm(wuses))
        allocate (wal_use_omy(wuses))
        allocate (wal_use_oma(wuses))
        
        read (107,*,iostat=eof) titldum

        !! allocate and read the water treatment plant names
        read (107,*,iostat=eof) (wtp_name(iwtp), iwtp = 1, wtps)
        
        !! allocate and read the water use names
        read (107,*,iostat=eof) (wuse_name(iwuse), iwuse = 1, wuses)
        
        read (107,*,iostat=eof) titldum
        
        !! crosswalk water use name with water treatment plant data file
        do iwtp = 1, wtps
          do iwtp_db = 1, db_mx%treat
            if (wtp_name(iwtp) == wtp(iwtp_db)%name) then
              wtp(iwtp)%db_num = iwtp_db
              exit
            end if
          end do
        end do
        
        !! crosswalk water use name with water use data file
        do iwuse = 1, wuses
          do iwuse_db = 1, db_mx%uses
            if (wuse_name(iwuse) == wuse(iwuse_db)%name) then
              wuse(iwuse)%db_num = iwuse_db
              exit
            end if
          end do
        end do
        
        exit
      end do
      end if
      close(107)

      return
    end subroutine wallo_wtps_wuses
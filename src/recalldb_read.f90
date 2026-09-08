  subroutine recalldb_read

      use water_allocation_module
      use maximum_data_module
      use recall_module
      use hydrograph_module
      
      implicit none
      
      character (len=80) :: titldum = ""
      character (len=80) :: header = ""
      integer :: eof = 0
      integer :: imax = 0
      integer :: i = 0
      integer :: ii = 0
      integer :: k = 0
      integer :: iom = 0
      logical :: i_exist              !none       |check to determine if file exists
      
      !read all recall files
      inquire (file="recall_db.rec", exist=i_exist)
      if (i_exist .or. "recall_db.rec" /= "null") then
      do
        open (107,file="recall_db.rec")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i) 
          end do
          db_mx%recalldb_max = imax
          
      allocate (recall_db(0:imax))
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header
      if (eof < 0) exit
      
      do ii = 1, imax
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        read (107,*,iostat = eof) k, recall_db(i)%name, recall_db(i)%org_min,  &
                                     recall_db(i)%pest, recall_db(i)%path,     &
                                     recall_db(i)%hmet, recall_db(i)%salt,     &
                                     recall_db(i)%constit, recall_db(i)%descrip
        if (eof < 0) exit
                
      end do
      
    end do
    end if
    close (107)
      
    end subroutine recalldb_read
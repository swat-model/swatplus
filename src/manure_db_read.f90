      subroutine manure_db_read
      
      use input_file_module
      use maximum_data_module
      use fertilizer_data_module
      
      implicit none
   
      integer :: it = 0               !none       |counter
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      integer :: mfrt = 0             !           |
      logical :: i_exist              !none       |check to determine if file exists
      
      
      eof = 0
      imax = 0
      mfrt = 0
      
      inquire (file="manure_db.frt", exist=i_exist)
      if (.not. i_exist .or. "manure_db.frt" == "null") then
        allocate (manure_db(0:0))
      else
        do  
          open (107,file="manure_db.frt")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
            do while (eof == 0) 
              read (107,*,iostat=eof) titldum
              if (eof < 0) exit
              imax = imax + 1
            end do
           
            allocate (manure_db(0:imax))
        
            rewind (107)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
        
            do it = 1, imax
              read (107,*,iostat=eof) manure_db(it)%name, manure_db(it)%org_min, manure_db(it)%pests,   &
                manure_db(it)%paths, manure_db(it)%hmets, manure_db(it)%salts, manure_db(it)%constit,   &
                                                                               manure_db(it)%descrip
          
              !! xwalk org_min with manure_om_db to get iorg_min
              do mfrt = 1, db_mx%manure_om
                if (manure_db(it)%org_min == manure_om(mfrt)%name) then
                  manure_db(it)%iorg_min = mfrt
                  exit
                end if
              end do
          
              !! xwalk pests with fertdb to get ipests
              !do mfrt = 1, size(fertdb)
              !  if (manure_db(it)%pests == fertdb(mfrt)%fertnm) then
              !    manure_db(it)%ipests = mfrt
              !    exit
              !  end if
              !end do
            end do     
          exit
        end do
      end if
      
      db_mx%manureparm  = imax 
      
      close (107)
      return
      end subroutine manure_db_read
      subroutine sat_buff_read
      
      use input_file_module
      use maximum_data_module
      use hru_module
      use conditional_module
      
      implicit none

      
      
      
      external :: smp_buffer
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: msno = 0             !           |
      integer :: ibuff = 0            !none       |counter
      integer :: idb = 0              !none       |counter
      integer :: hru_src = 0          !none       |source hru for saturated buffer
      integer :: hru_rcv = 0          !none       |receiving hru (the saturated buffer hru(
      
      msno = 0
      eof = 0
      imax = 0
      
      !! read snow database data from snow.sno
      inquire (file="satbuffer.str", exist=i_exist)
      if(i_exist) then
          
      do 
        open (107,file="satbuffer.str")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        allocate (satbuff_db(0:imax))
    
        !! read saturated buffer inputs for all src/rcv hrus
        do ibuff = 1, imax
          read (107,*,iostat=eof) satbuff_db(ibuff)         
          if (eof < 0) exit
        end do

        !! set initial parameters
        do ibuff = 1, imax
          !! set saturated buffer parameters for source and receiving (buffer) hrus
          hru_src = satbuff_db(ibuff)%hru_src
          hru(hru_src)%sb%sb_db = satbuff_db(ibuff)
          hru_rcv = satbuff_db(ibuff)%hru_rcv
          hru(hru_rcv)%sb%sb_db = satbuff_db(ibuff)
          
          !! crosswalk to get number of flow control decision table
          do idb = 1, db_mx%dtbl_flo
            if (satbuff_db(ibuff)%flocon_dtbl == dtbl_flo(idb)%name) then
              hru(hru_src)%sb%dtbl = idb
            end if
          end do
                  
        end do

      exit
      enddo
      
      endif
      close (107)
      
      db_mx%sat_buff = imax
      
      return
      end subroutine sat_buff_read
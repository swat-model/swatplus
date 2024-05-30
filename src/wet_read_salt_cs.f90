      subroutine wet_read_salt_cs
      
      use maximum_data_module
      use reservoir_data_module
      use constituent_mass_module
      use reservoir_module
      use res_salt_module
      use res_cs_module
      
      implicit none

      integer :: i
      
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iwet                 !none       |counter 
      integer :: k                    !           |
      integer :: isalt                !none       |counter
      integer :: ics                  !none       |counter
      
      eof = 0
      imax = 0
            
      !read wetland.wet_cs
      inquire (file="wetland.wet_cs",exist=i_exist)
      if(i_exist) then
      
        !open file
        open(105,file="wetland.wet_cs")
        read(105,*) header
        read(105,*) header
        
        !allocate array
        allocate (wet_dat_c_cs(db_mx%wet_dat))

        !loop through the wetland entries
        do iwet = 1, db_mx%wet_dat
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) k, wet_dat_c_cs(iwet)
          if (eof < 0) exit
          
          !rtb salt
          do isalt = 1, db_mx%res_salt
            if (res_salt_data(isalt)%name == wet_dat_c_cs(iwet)%salt) then
              wet_dat(iwet)%salt = isalt
              exit
            endif
          enddo 
          
          !rtb cs
          do ics = 1, db_mx%res_cs
            if (res_cs_data(ics)%name == wet_dat_c_cs(iwet)%cs) then
              wet_dat(iwet)%cs = ics
              exit
            endif
          enddo 
          
        enddo !go to next reservoir entry
         
       close(105)
      endif
      
      return
      end subroutine wet_read_salt_cs     
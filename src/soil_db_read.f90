      subroutine soil_db_read
      
      use input_file_module
      use maximum_data_module
      use soil_data_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: j                    !none       |hru            
      integer :: nlyr                 !           |
      integer :: lyr                  !none       |counter
      integer :: mlyr                 !           |
      integer :: isol                 !none       |counter
 
      eof = 0
      imax = 0

      inquire (file=in_sol%soils_sol,exist=i_exist)
      if (.not. i_exist .or. in_sol%soils_sol == "null") then
        allocate (soildb(0:0))
        allocate (soildb(0)%ly(0:0))
      else
        do  
          open (107,file=in_sol%soils_sol)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          !! determine max number for array (imax) and total number in file
            do while (eof == 0)
              read (107,*,iostat=eof) titldum, nlyr 
              if (eof < 0) exit
                do lyr = 1, nlyr
                  read (107,*,iostat=eof) titldum
                  if (eof < 0) exit
                end do 
                imax = imax + 1
            end do
            
          db_mx%soil = imax 
          
          allocate (soildb(0:imax))
          
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
              
        do isol = 1, db_mx%soil
            
         read (107,*,iostat=eof) soildb(isol)%s%snam, soildb(isol)%s%nly
         if (eof < 0) exit
         mlyr = soildb(isol)%s%nly
      
         allocate (soildb(isol)%ly(mlyr)) 
       
         backspace 107
         read (107,*,iostat=eof) soildb(isol)%s%snam, soildb(isol)%s%nly,  &
          soildb(isol)%s%hydgrp, soildb(isol)%s%zmx,                       &           
          soildb(isol)%s%anion_excl, soildb(isol)%s%crk,                   &           
          soildb(isol)%s%texture
         if (eof < 0) exit
       do j = 1, mlyr
        read (107,*,iostat=eof) soildb(isol)%ly(j)%z, soildb(isol)%ly(j)%bd,             &
            soildb(isol)%ly(j)%awc, soildb(isol)%ly(j)%k, soildb(isol)%ly(j)%cbn,        &           
            soildb(isol)%ly(j)%clay, soildb(isol)%ly(j)%silt, soildb(isol)%ly(j)%sand,   &
            soildb(isol)%ly(j)%rock, soildb(isol)%ly(j)%alb, soildb(isol)%ly(j)%usle_k,  &           
            soildb(isol)%ly(j)%ec, soildb(isol)%ly(j)%cal, soildb(isol)%ly(j)%ph     
        if (eof < 0) exit
       end do
        end do
        exit
        enddo
        endif
      
        close (107)
        
      return
      end subroutine soil_db_read
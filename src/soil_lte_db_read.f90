        subroutine soil_lte_db_read
      
        use input_file_module
        use maximum_data_module
        use hru_lte_module
        use soil_data_module
        
        implicit none
        
        character (len=80) :: titldum   !           |title of file
        character (len=80) :: header    !           |header of file
        integer :: eof                  !           |end of file
        integer :: k                    !           |texture counter
        logical :: i_exist         !                |check to determine if file exists
        
    
        eof = 0
        
        !allocate (soil_lte(12))

       inquire (file=in_sol%lte_sol, exist=i_exist)
         if (.not. i_exist .or. in_sol%lte_sol == "null") then
            allocate (soil_lte(0:0))
          else
       do
          open (107,file=in_sol%lte_sol)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
        allocate (soil_lte(12))
        
          do k = 1, 12          
            read (107,*,iostat=eof) soil_lte(k)
            if (eof < 0) exit
          end do
        end do
     
        close (107)
      end if 
      return
      end subroutine soil_lte_db_read
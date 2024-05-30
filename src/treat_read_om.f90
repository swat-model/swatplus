      subroutine treat_read_om

      use water_allocation_module
      use dr_module
      use constituent_mass_module
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use maximum_data_module

      implicit none
      
      character (len=80) :: titldum, header
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii

      eof = 0
      imax = 0
      
      !read delivery ratio organic-mineral data
      inquire (file="treatment.trt", exist=i_exist)
      if (i_exist .or. "treatment.trt" /= "null") then
        do
          open (107,file="treatment.trt")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) imax
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          db_mx%trt_om = imax
          
          allocate (trt(0:imax))
          allocate (trt_om_name(0:imax))
      
          !read all delivery ratio data
          do ii = 1, db_mx%trt_om
            read (107,*,iostat=eof) trt_om_name(ii), trt(ii)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      return

      end subroutine treat_read_om
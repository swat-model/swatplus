      subroutine cs_fert_read !rtb cs
       
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads constituent fertilizer loading (kg/ha) for various fertilizer types
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use cs_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=30) :: fert_name
      integer :: icsi
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists

      eof = 0
      
      !read constituent fertilizer loading (kg/ha)
      inquire (file="fertilizer.frt_cs", exist=i_exist)
      if (i_exist) then
        open (107,file="fertilizer.frt_cs")
        read (107,*,iostat=eof) titldum
        read (107,*,iostat=eof) header
        
        !allocate fertilizer array
        allocate (fert_cs(db_mx%fertparm))
        
        !set flag
        fert_cs_flag = 1
        
        !read in the constituent fertilizer rates (kg/ha) for each fertilizer type
        do icsi=1,db_mx%fertparm
          read (107,*) fert_cs(icsi)  
        enddo
        close (107)
      end if
      
      return
      end subroutine cs_fert_read
      !read in cs uptake parameters (rtb cs)
      subroutine cs_uptake_read
      
      use basin_module
      use input_file_module
      use climate_module
      use time_module
      use maximum_data_module
      use constituent_mass_module
      use hydrograph_module
      use cs_module
      use maximum_data_module
      
      implicit none
      
      character (len=80) :: file      !           |filename
      character (len=80) :: header    !           |header of file
      character (len=30) :: name      !           |header of file
      character (len=30) :: plnt_typ  !           |header of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: eof                  !           |end of file
      integer :: i,j                  !           |counters
      
      
      eof = 0

      !only proceed if there are constituents in the simulation
      if(cs_db%num_cs > 0) then
      
      cs_uptake_on = 0
      inquire (file='cs_uptake',exist=i_exist)
      if(i_exist) then
        
        !turn on constituent uptake feature
        cs_uptake_on = 1
        
        !open and read first lines
        open(5054,file='cs_uptake')
        read(5054,*) header
        read(5054,*) header
        read(5054,*) header
        
        !allocate array
        allocate(cs_uptake_kg(db_mx%plantparm,cs_db%num_cs))
        cs_uptake_kg = 0.
        
        !loop through the plant communities
        do i=1,db_mx%plantparm
          read(5054,*) name,(cs_uptake_kg(i,j),j=1,cs_db%num_cs)
        enddo
        
        close(5054) 
      endif
      
      endif
      
            
      return
      end subroutine cs_uptake_read
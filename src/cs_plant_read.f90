      subroutine cs_plant_read !rtb cs
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use cs_data_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=12) :: plant_name
      integer :: iplant
      logical :: i_exist              !none       |check to determine if file exists


      !read plant boron tolerance data
      inquire (file="cs_plants_boron", exist=i_exist)
      if (i_exist) then
        
        !open the file and read first line
        open(107,file="cs_plants_boron")
        read(107,*) titldum

        !read plant boron tolerance parameters (a,b parameters from relative yield equations)
        read(107,*)
        read(107,*) bor_tol_sim !flag to simulate boron effect on plant growth 
        read(107,*) header
        read(107,*) header
        read(107,*) header
        read(107,*) header
        allocate(bor_stress_a(db_mx%plantparm))
        allocate(bor_stress_b(db_mx%plantparm))
        do iplant=1,db_mx%plantparm
          read(107,*) plant_name,bor_stress_a(iplant),bor_stress_b(iplant)
        enddo

        !close the file
        close(107)

      endif

      
      return
      end subroutine cs_plant_read
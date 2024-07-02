      subroutine salt_plant_read !rtb salt
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use salt_data_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=12) :: plant_name
      integer :: isalt,iplant
      logical :: i_exist              !none       |check to determine if file exists


      !read plant salt tolerance data
      inquire (file="salt_plants", exist=i_exist)
      if (i_exist) then
        
        !open the file and read first line
        open(107,file="salt_plants")
        read(107,*) titldum
        
        !read TDS-->EC conversion factor
        read(107,*) header
        read(107,*) salt_tds_ec

        !read plant salinity tolerance parameters (a,b parameters from relative yield equations)
        read(107,*)
        read(107,*) salt_tol_sim !flag to simulate salt effect on plant growth 
        read(107,*) salt_soil_type !soil type (1 = CaSO4 soils; 2 = NaCl soils)
        read(107,*) salt_effect !method for applying salt stress: 1 = applied after other stresses; 2 = included with other stresses (min)
        read(107,*)
        read(107,*)
        read(107,*)
        read(107,*)
        read(107,*) header
        allocate(salt_stress_a(db_mx%plantparm))
        allocate(salt_stress_b(db_mx%plantparm))
        do iplant=1,db_mx%plantparm
          read(107,*) plant_name,salt_stress_a(iplant),salt_stress_b(iplant)
        enddo

        !close the file
        close(107)

      endif
 
      return
      end subroutine salt_plant_read
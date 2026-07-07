      subroutine aqu_read_init
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use aquifer_module
      use aqu_pesticide_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none      
      
      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: eof                    !             |end of file
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      logical :: i_exist                !none         |check to determine if file exists
      integer :: i                      !none         |counter
      integer :: iaqu                   !none         |counter
      integer :: isp_ini                !             |
      integer :: ics                    !             |
      integer :: idat                   !             |
      integer :: init, iaq, iob, idb, ini, ipest, ipath, isalt, init_aqu
      eof = 0
      imax = 0
      
      !read init
      inquire (file=in_aqu%init, exist=i_exist)
      if (.not. i_exist .or. in_aqu%init == "null") then
        allocate (aqu_init(0:0))
      else   
      do
       open (105,file=in_aqu%init)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do

      allocate (aqu_init(0:imax))
      allocate (aqu_init_dat_c(0:imax))

      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
           
       do iaqu = 1, imax
         read (105,*,iostat=eof) aqu_init_dat_c(iaqu)
         if (eof < 0) exit
       end do
       
       end do
       close (105)

      end if

      !! initialize organics and constituents
      do iaq = 1, sp_ob%aqu
        iob = sp_ob1%aqu + iaq - 1
        idat = ob(iob)%props

        do idb = 1, db_mx%aqudb
          do init_aqu = 1, imax
            if (aqudb(idb)%aqu_ini == aqu_init_dat_c(init_aqu)%name) then
              ini = init_aqu
              exit
            end if
          end do

            !! initial organic mineral
            do ics = 1, db_mx%om_water_init
              !! initializing organics in aqu_initial - do it here later
              if (aqu_init(ini)%org_min == 0) write (9001,*) om_init_name(ics), " not found"
            end do
            
            !! initial pesticides
            do ics = 1, db_mx%pest_ini
              if (aqu_init_dat_c(ini)%pest == pest_soil_ini(ics)%name) then
                !! initialize pesticides in aquifer water and benthic from input data
                do ipest = 1, cs_db%num_pests
                  !! kg/ha = mg/kg (ppm) * t/m3  * m * 10000.m2/ha * 1000kg/t * kg/1000000 mg
                  !! assume bulk density of 2.0 t/m3
                  cs_aqu(iaq)%pest(ipest) = pest_soil_ini(ics)%soil(ipest) * 2.0 * aqudb(idb)%dep_bot * 10.
                end do
                exit
              end if
              if (aqu_init(ini)%pest == 0) write (9001,*) pest_init_name(ics), " not found" 
            end do
            
            !! initial pathogens
            do ics = 1, db_mx%pathw_ini
              if (aqu_init_dat_c(ini)%path == path_init_name(ics)) then
                !! initialize pathogens in aquifer water and benthic from input data
                do ipath = 1, cs_db%num_paths
                  cs_aqu(iaq)%path(ipath) = path_soil_ini(ics)%soil(ipath)
                end do
                exit
              end if
              if (aqu_init(ini)%path == 0) write (9001,*) path_init_name(ics), " not found"
            end do
            
            !! initial heavy metals
            !! initial salts
            do ics = 1, db_mx%saltw_ini
              if (aqu_init_dat_c(ini)%path == path_init_name(ics)) then
                !! initialize salts in aquifer water and benthic from input data
                do isalt = 1, cs_db%num_salts
                  cs_aqu(iaq)%salt(isalt) = salt_soil_ini(ics)%soil(isalt)
                end do
                exit
              end if
              if (aqu_init(ini)%salt == 0) write (9001,*) salt_init_name(ics), " not found"
            end do
            
        end do

      end do

      return
      end subroutine aqu_read_init
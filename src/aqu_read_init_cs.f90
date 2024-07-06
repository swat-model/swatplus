      subroutine aqu_read_init_cs !rtb salt/cs
      
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
      integer :: ictr
      integer :: isp_ini                !             |                    !             |
      integer :: idat                   !             |
      integer :: init, iaq, iob, idb, ini, ipest, ipath, isalt, init_aqu, ics, iaqdb
      real :: gw_volume,aqu_volume,aqu_bd,aqu_mass,mass_sorbed
      eof = 0
      imax = 0
            
      !read initial.aqu_cs
      inquire (file="initial.aqu_cs",exist=i_exist)
      if(i_exist) then
       
        do
          open (105,file="initial.aqu_cs")
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          read (105,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

          !allocate array
          allocate (aqu_init_dat_c_cs(imax))

          !read initialization names
          rewind (105)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          read (105,*,iostat=eof) header
          if (eof < 0) exit
          do iaqu = 1, imax
            read (105,*,iostat=eof) aqu_init_dat_c_cs(iaqu)
            if (eof < 0) exit
          enddo
       
        enddo
        close (105)
      
        !! initialize organics and constituents for each aquifer object
        do iaq = 1, sp_ob%aqu
          iob = sp_ob1%aqu + iaq - 1
          idat = ob(iob)%props
          gw_volume = (aqu_d(iaq)%stor/1000.)*(ob(iob)%area_ha*10000.) !m3 of groundwater
          iaqdb = ob(iob)%props !get database of the aquifer object
          aqu_volume = (ob(iob)%area_ha*10000.) * aqudb(iaqdb)%dep_bot * (1-aqu_dat(iaq)%spyld) !m3 of aquifer material
          aqu_bd = 2000. !kg/m3 bulk density (assumed)
          aqu_mass = aqu_volume * aqu_bd !m3 * kg/m3 --> kg
          
          !! initial pesticides
          do isp_ini = 1, imax
            if (aqu_init_dat_c_cs(isp_ini)%name == aqudb(iaq)%aqu_ini) then
              do ics = 1, db_mx%pestw_ini
                if (aqu_init_dat_c_cs(isp_ini)%pest == pest_init_name(ics)) then
                  !! initialize pesticides in aquifer water and benthic from input data
                  do ipest = 1, cs_db%num_pests
                    !! kg/ha = mg/kg (ppm) * t/m3  * m * 10000.m2/ha * 1000kg/t * kg/1000000 mg
                    !! assume bulk density of 2.0 t/m3
                    cs_aqu(iaq)%pest(ipest) = pest_water_ini(ics)%water(ipest) * 2.0 * aqudb(iaq)%dep_bot * 10.
                  end do
                  exit
                end if
              end do  
            endif
          enddo
           
          !! initial pathogens
          do isp_ini = 1, imax
            if (aqu_init_dat_c_cs(isp_ini)%name == aqudb(iaq)%aqu_ini) then
              do ics = 1, db_mx%pathw_ini
                if (aqu_init_dat_c_cs(isp_ini)%path == path_init_name(ics)) then
                  !! initialize pathogens in aquifer water and benthic from input data
                  do ipath = 1, cs_db%num_paths
                    cs_aqu(iaq)%path(ipath) = path_soil_ini(ics)%soil(ipath)
                  end do
                  exit
                end if
              end do  
            endif
          enddo
            
          !! initial salts !rtb salt
          if(cs_db%num_salts > 0) then
          do ics = 1, db_mx%salt_gw_ini
            if (aqu_init_dat_c_cs(iaq)%salt == salt_aqu_ini(ics)%name) then
              !loop for salt ions
              do isalt = 1,cs_db%num_salts
                cs_aqu(iaq)%saltc(isalt) = salt_aqu_ini(ics)%conc(isalt) !g/m3 (mg/L)
                cs_aqu(iaq)%salt(isalt) = (salt_aqu_ini(ics)%conc(isalt)*gw_volume) / 1000. !g/m3 --> kg
              enddo
              ! loop for salt mineral fractions
              do isalt = 1,5
                cs_aqu(iaq)%salt_min(isalt) = salt_aqu_ini(ics)%frac(isalt)
              end do
              exit
            end if
          end do
          endif
            
          !! initial constituents !rtb cs
          if(cs_db%num_cs > 0) then
          do ictr = 1, db_mx%cs_ini
            if (aqu_init_dat_c_cs(iaq)%cs == cs_aqu_ini(ictr)%name) then
              !loop for constituents
              do ics = 1, cs_db%num_cs
                cs_aqu(iaq)%csc(ics) = cs_aqu_ini(ictr)%aqu(ics) !g/m3 (mg/L)
                cs_aqu(iaq)%cs(ics) = (cs_aqu_ini(ictr)%aqu(ics)*gw_volume) / 1000. !g/m3 --> kg
                cs_aqu(iaq)%csc_sorb(ics) = cs_aqu_ini(ictr)%aqu(ics+cs_db%num_cs) !mg/kg
                mass_sorbed = (cs_aqu(iaq)%csc_sorb(ics)*aqu_mass)/1.e6 !mg/kg * kg / 1.e6 = kg
                cs_aqu(iaq)%cs_sorb(ics) = mass_sorbed / ob(iob)%area_ha !kg/ha
              enddo
              exit
            end if
          end do
          endif

        end do
      
      endif !check for file
      
      
      return
      end subroutine aqu_read_init_cs
      
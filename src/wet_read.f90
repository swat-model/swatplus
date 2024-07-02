      subroutine wet_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use conditional_module
      use reservoir_module
      use hydrograph_module
      use constituent_mass_module
      use pesticide_data_module
      use res_salt_module
      use res_cs_module
      use hru_module, only : hru
      
      implicit none

      character (len=80) :: titldum      !           |title of file
      character (len=80) :: header       !           |header of file
      integer :: eof                     !           |end of file
      integer :: imax                    !none       |determine max number for array (imax) and total number in file
      logical :: i_exist                 !none       |check to determine if file exists
      integer :: i                       !none       |counter
      integer :: ires                    !none       |counter 
      integer :: ihyd                    !none       |counter 
      integer :: k                       !           |
      integer :: irel                    !none       |counter
      integer :: ised                    !none       |counter
      integer :: inut                    !none       |counter
      integer :: isp_ini                 !none       |counter
      integer :: ics                     !none       |counter
      integer :: isstor                  !none       |counter
      
      eof = 0
      imax = 0
            
      !read wetland.wet
      imax = 0
      inquire (file=in_res%wet, exist=i_exist)
      if (.not. i_exist .or. in_res%wet == "null") then
        allocate (wet_dat_c(0:0))
        allocate (wet_dat(0:0))
      else   
        do
          open (105,file=in_res%wet)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          read (105,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            imax = imax + 1
          end do
        
          db_mx%wet_dat = imax
       
          allocate (wet_dat_c(imax))
          allocate (wet_dat(imax))
      
          rewind (105)
          read (105,*,iostat = eof) titldum
          if (eof < 0) exit
          read (105,*,iostat=eof) header
          if (eof < 0) exit
      
          do ires = 1, db_mx%wet_dat
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            backspace (105)
            read (105,*,iostat=eof) k, wet_dat_c(ires)
            if (eof < 0) exit
          end do
      
          db_mx%wet_dat = imax
            
          do i = 1, sp_ob%hru
            if (hru(i)%dbsc%surf_stor /= "null") then
              do isstor = 1, db_mx%wet_dat
                if (hru(i)%dbsc%surf_stor == wet_dat_c(isstor)%name) then
                  hru(i)%dbs%surf_stor = isstor
                  hru(i)%wet_db = isstor
                  
                  !! initialize orgaincs and minerals in water
                  do isp_ini = 1, db_mx%res_init
                    if (wet_dat_c(isstor)%init == res_init_dat_c(isp_ini)%init) then
                      wet_dat(isstor)%init = isp_ini
                      !! initial organic mineral
                      do ics = 1, db_mx%om_water_init
                        if (res_init_dat_c(isp_ini)%org_min == om_init_name(ics)) then
                          wet_init(isp_ini)%org_min = ics
                          exit
                        end if
                      end do
                      !! initial pesticides
                      do ics = 1, db_mx%pestw_ini
                        if (res_init_dat_c(isp_ini)%pest == pest_init_name(ics)) then
                          wet_init(isp_ini)%pest = ics
                          exit
                        end if
                      end do
                      !! initial pathogens
                      do ics = 1, db_mx%pathw_ini
                        if (res_init_dat_c(isp_ini)%path == path_init_name(ics)) then
                          wet_init(isp_ini)%path = ics
                          exit
                        end if
                    end do
                    !! initial heavy metals
                    !! initial salts
                  end if
                end do

                !! xwalk hydrology inputs
                do ihyd = 1, db_mx%wet_hyd
                  if (wet_hyddb(ihyd)%name == wet_dat_c(isstor)%hyd) then
                    wet_hyd(i) = wet_hyddb(ihyd)
                    wet_dat(isstor)%hyd = ihyd
                    exit
                  end if
                end do
       
                !! xwalk release decision table
                do irel = 1, db_mx%dtbl_res
                  if (dtbl_res(irel)%name == wet_dat_c(isstor)%release) then
                    wet_dat(isstor)%release = irel
                    exit
                  end if
                end do      
   
               !! xwalk with sediment inputs
               do ised = 1, db_mx%res_sed
                 if (res_sed(ised)%name == wet_dat_c(isstor)%sed) then
                   wet_prm(i)%sed = res_sed(ised)
                   wet_dat(isstor)%sed = ised
                   exit
                 end if
               end do      
   
               !! xwalk with nutrient inputs
               do inut = 1, db_mx%res_nut
                 if (res_nut(inut)%name == wet_dat_c(isstor)%nut) then
                   wet_prm(i)%nut = res_nut(inut)
                   wet_dat(isstor)%nut = inut
                   exit
                 end if
               end do   
        
               if (wet_dat(isstor)%init == 0) write (9001,*) wet_dat_c(isstor)%init, " not found (wet-init)"
               if (wet_dat(isstor)%hyd == 0) write (9001,*) wet_dat_c(isstor)%hyd, " not found (wet-hyd)"
               if (wet_dat(isstor)%release == 0) write (9001,*) wet_dat_c(isstor)%release, " not found (wet-release)"
               if (wet_dat(isstor)%sed == 0) write (9001,*) wet_dat_c(isstor)%sed, " not found (wet-sed)"
               if (wet_dat(isstor)%nut == 0) write (9001,*) wet_dat_c(isstor)%nut, " not found (wet-nut)"

               exit
             end if
           end do
         end if
         if (hru(i)%dbs%surf_stor == 0 .and. hru(i)%dbsc%surf_stor /= 'null') & 
           write (9001,*) hru(i)%dbsc%surf_stor,"not found (wetland.wet)"
         end do
       
         close (105)
         exit
       end do
      end if
      
      return
      end subroutine wet_read
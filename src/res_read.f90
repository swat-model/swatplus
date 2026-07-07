      subroutine res_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use conditional_module
      use hydrograph_module
      use constituent_mass_module
      use reservoir_module
      use pesticide_data_module
      
      implicit none

      integer :: mon
      integer :: i
      real :: lnvol
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ires                 !none       |counter 
      integer :: k                    !           |
      integer :: iinit                !none       |counter 
      integer :: ihyd                 !none       |counter
      integer :: irel                 !none       |counter 
      integer :: ised                 !none       |counter
      integer :: inut                 !none       |counter
      integer :: ipst                 !none       |counter
      integer :: isp_ini              !          |
      integer :: ics                  !none      |counter
      integer :: iob                  !none      |counter
      
      eof = 0
      imax = 0
            
      !read reservoir.res
      imax = 0
      inquire (file=in_res%res, exist=i_exist)
      if (.not. i_exist .or. in_res%res == "null") then
        allocate (res_dat_c(0:0))
        allocate (res_dat(0:0))
      else   
      do
       open (105,file=in_res%res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%res_dat = imax
       
      allocate (res_dat_c(0:imax))
      allocate (res_dat(0:imax))
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
      
       do i = 1, db_mx%res_dat
         read (105,*,iostat=eof) ires
         if (eof < 0) exit
         backspace (105)
         read (105,*,iostat=eof) k, res_dat_c(ires)
         if (eof < 0) exit

        !! initialize organics and minerals in water
        do isp_ini = 1, db_mx%res_init
          if (res_dat_c(ires)%init == res_init_dat_c(isp_ini)%init) then
            res_dat(ires)%init = isp_ini
            if (res_dat(ires)%init == 0) write (9001,*) res_init_dat_c(isp_ini)%init, " not found (res-init)"
            !! initial organic mineral
            do ics = 1, db_mx%om_water_init
              if (res_init_dat_c(isp_ini)%org_min == om_init_name(ics)) then
                res_init(isp_ini)%org_min = ics
                exit
              end if
            if (res_init(isp_ini)%org_min == 0) write (9001,*) om_init_name(ics), " not found"
            end do
            !! initial pesticides
            do ics = 1, db_mx%pestw_ini
              if (res_init_dat_c(isp_ini)%pest == pest_init_name(ics)) then
                res_init(isp_ini)%pest = ics
                exit
              end if
            if (res_init(isp_ini)%pest == 0) write (9001,*) pest_init_name(ics), " not found" 
            end do
            !! initial pathogens
            do ics = 1, db_mx%pathw_ini
              if (res_init_dat_c(isp_ini)%path == path_init_name(ics)) then
                res_init(isp_ini)%path = ics
                exit
              end if
            if (res_init(isp_ini)%path == 0) write (9001,*) path_init_name(ics), " not found"
            end do
            !! initial heavy metals
            !! initial salts
          end if
        end do

         do ihyd = 1, db_mx%res_hyd
           if (res_hyd(ihyd)%name == res_dat_c(ires)%hyd) then
             res_dat(ires)%hyd = ihyd
             exit
           end if
         end do
       
          do irel = 1, db_mx%dtbl_res
            if (dtbl_res(irel)%name == res_dat_c(ires)%release) then
             res_dat(ires)%release = irel
             exit
            end if
          end do      
 
         do ised = 1, db_mx%res_sed
           if (res_sed(ised)%name == res_dat_c(ires)%sed) then
             res_dat(ires)%sed = ised
             exit
           end if
         end do      

         do inut = 1, db_mx%res_nut
           if (res_nut(inut)%name == res_dat_c(ires)%nut) then
             res_dat(ires)%nut = inut
             exit
           end if
         end do   

       if (res_dat(ires)%hyd == 0) write (9001,*) res_dat_c(ires)%hyd, " not found (res-hyd)"
       if (res_dat(ires)%release == 0) write (9001,*) res_dat_c(ires)%release, " not found (res-release)"         
       if (res_dat(ires)%sed == 0) write (9001,*) res_dat_c(ires)%sed, " not found (res-sed)"
       if (res_dat(ires)%nut == 0) write (9001,*) res_dat_c(ires)%nut, " not found (res-nut)"
       end do

      close (105)
      exit
      enddo
      endif
      
      return
      end subroutine res_read
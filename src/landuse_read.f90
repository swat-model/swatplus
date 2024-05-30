      subroutine landuse_read
      
      use input_file_module
      use maximum_data_module
      use septic_data_module
      use plant_data_module
      use hru_module, only : sdr
      use landuse_data_module
      use mgt_operations_module
      
      implicit none

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: mlu                  !           |
      integer :: ilu                  !           |landuse type 
      integer :: ipcom                !none       |counter
      integer :: isched               !           |
      integer :: ipr                  !none       |counter
       
      mlu = 0
      eof = 0
      imax = 0
      
      !! read all landuse data from landuse.dat
      inquire (file=in_lum%landuse_lum, exist=i_exist)
      if (.not. i_exist .or. in_lum%landuse_lum == "null") then
        allocate (lum(0:0))
        allocate (lum_str(0:0))
      else
      do
       open (107,file=in_lum%landuse_lum)
       read (107,*,iostat=eof) titldum
       if (eof < 0) exit
       read (107,*,iostat=eof) header
       if (eof < 0) exit
       do while (eof == 0)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         imax = imax + 1
       end do
       
       allocate (lum(0:imax))
       allocate (lum_str(0:imax))
      
       rewind (107)
       read (107,*,iostat=eof) titldum
       if (eof < 0) exit
       read (107,*,iostat=eof) header
       if (eof < 0) exit
      
       do ilu = 1, imax   
         read (107,*,iostat=eof) lum(ilu)
         if (eof < 0) exit
       end do
       
       do ilu = 1, imax    
         if (lum(ilu)%plant_cov /= "null") then
           do ipcom = 1, db_mx%plantcom
              if (lum(ilu)%plant_cov == pcomdb(ipcom)%name) then
                 lum_str(ilu)%plant_cov = ipcom
                 exit
              end if
           end do
         end if
         
         if (lum(ilu)%mgt_ops /= "null") then 
           do isched = 1, db_mx%mgt_ops
             if (lum(ilu)%mgt_ops == sched(isched)%name) then
               lum_str(ilu)%mgt_ops = isched
               exit
             end if
           end do
         end if
         
         if (lum(ilu)%cn_lu /= "null") then
           do ipr = 1, db_mx%cn_lu
             if (lum(ilu)%cn_lu == cn(ipr)%name) then
               lum_str(ilu)%cn_lu = ipr
               exit
             end if
           end do
         end if
                  
         if (lum(ilu)%cons_prac /= "null") then
           do ipr = 1, db_mx%cons_prac
             if (lum(ilu)%cons_prac == cons_prac(ipr)%name) then
               lum_str(ilu)%cons_prac = ipr
               exit
             end if
           end do
         end if
         
         if (lum(ilu)%tiledrain /= "null") then
           do ipr = 1, db_mx%sdr
             if (lum(ilu)%tiledrain == sdr(ipr)%name) then
               lum_str(ilu)%tiledrain = ipr
               exit
             end if
           end do
         end if
             
         if (lum(ilu)%septic /= "null") then
           do ipr = 1, db_mx%septic
             if (lum(ilu)%septic == sep(ipr)%name) then
               lum_str(ilu)%septic = ipr
               exit
             end if
           end do
         end if
         
         if (lum(ilu)%fstrip /= "null") then
           do ipr = 1, db_mx%filtop_db
             if (lum(ilu)%fstrip == filtstrip_db(ipr)%name) then
               lum_str(ilu)%fstrip = ipr
               exit
             end if
           end do
         end if
                  
         if (lum(ilu)%grassww /= "null") then
           do ipr = 1, db_mx%grassop_db
             if (lum(ilu)%grassww == grwaterway_db(ipr)%name) then
               lum_str(ilu)%grassww = ipr
               exit
             end if
           end do
         end if
                             
         if (lum(ilu)%bmpuser /= "null") then
           do ipr = 1, db_mx%bmpuserop_db
             if (lum(ilu)%bmpuser == bmpuser_db(ipr)%name) then
               lum_str(ilu)%bmpuser = ipr
               exit
             end if
           end do
         
         end if
         
           if (lum_str(ilu)%plant_cov == 0 .and. lum(ilu)%plant_cov /= "null") then 
               write (9001,*) lum(ilu)%name, lum(ilu)%plant_cov, " not found in plant.ini" 
           end if
           if (lum_str(ilu)%mgt_ops == 0 .and. lum(ilu)%mgt_ops /= "null") then
              write (9001,*) lum(ilu)%name, lum(ilu)%mgt_ops, " not found in management.sch"
           end if
           if (lum_str(ilu)%cn_lu == 0 .and. lum(ilu)%cn_lu /= "null") then 
              write (9001,*) lum(ilu)%name, lum(ilu)%cn_lu, " not found in cntable.lum"
           end if
           if (lum_str(ilu)%cons_prac == 0 .and. lum(ilu)%cons_prac /= "null") then 
              write (9001,*) lum(ilu)%name, lum(ilu)%cons_prac, " not found (landuse-consprac)"
           end if
           if (lum_str(ilu)%tiledrain == 0 .and. lum(ilu)%tiledrain /= "null") then
              write (9001,*) lum(ilu)%name, lum(ilu)%tiledrain, " not found (landuse-tiledr)"
           end if
           if (lum_str(ilu)%septic == 0 .and. lum(ilu)%septic /= "null") then 
              write (9001,*) lum(ilu)%name, lum(ilu)%septic, " not found (landuse-septic)"
           end if
           if (lum_str(ilu)%fstrip == 0 .and. lum(ilu)%fstrip /= "null") then
              write (9001,*) lum(ilu)%name, lum(ilu)%fstrip, " not found (landuse-fstrip)"
           end if
           if (lum_str(ilu)%grassww == 0 .and. lum(ilu)%grassww /= "null") then
              write (9001,*) lum(ilu)%name, lum(ilu)%grassww, " not found (landuse-grassww)"
           end if
           if (lum_str(ilu)%bmpuser == 0 .and. lum(ilu)%bmpuser /= "null") then 
              write (9001,*) lum(ilu)%name, lum(ilu)%bmpuser, " not found (landuse-bmpuser)"
           end if           
       end do
       exit
      end do
      endif
      
      close(107)
      
      db_mx%landuse = imax
      
      return  
      end subroutine landuse_read
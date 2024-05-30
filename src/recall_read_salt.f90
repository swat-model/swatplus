      subroutine recall_read_salt

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module
      use time_module
      use exco_module
      
      implicit none      
 
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character(len=16) :: ob_name
      character(len=8) :: ob_typ
      integer :: imax                 !none       |end of loop
      integer :: iyr                  !           |
      integer :: jday                 !           |
      integer :: mo                   !           |
      integer :: day_mo               !           |
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: nbyr                 !none       !number of years the land use occurred 
      integer :: k                    !           |
      integer :: iyrs                 !           | 
      integer :: iyr_prev             !none       |previous year
      integer :: istep                !           | 
      integer :: ipc                  !none       |counter
      integer :: ii                   !none       |counter
      integer :: i                    !           |
      integer :: iexco_om
      integer :: ifirst               !           |
      integer :: iexo_allo = 0
      integer :: isalt,jj,kk,dum
      
      eof = 0
      imax = 0

      !read all recall files
      inquire (file="salt_recall.rec", exist=i_exist)
      if (i_exist .or. "salt_recall.rec" /= "null") then
      do
        open (107,file="salt_recall.rec")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        !count the number of point source files
        imax = 0
        do while (eof == 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i) 
        end do
        
        !allocate recall_salt array
        allocate (rec_salt(0:imax))
        
        !allocate salt balance arrays
        !point sources originating from within the watershed
        allocate (recsaltb_d(imax))
        allocate (recsaltb_m(imax))
        allocate (recsaltb_y(imax))
        allocate (recsaltb_a(imax))
        do ii=1,imax
          allocate (recsaltb_d(ii)%salt(cs_db%num_salts))
          allocate (recsaltb_m(ii)%salt(cs_db%num_salts))
          allocate (recsaltb_y(ii)%salt(cs_db%num_salts))
          allocate (recsaltb_a(ii)%salt(cs_db%num_salts))
          do isalt=1,cs_db%num_salts
            recsaltb_d(ii)%salt(isalt) = 0.
            recsaltb_m(ii)%salt(isalt) = 0.
            recsaltb_y(ii)%salt(isalt) = 0.
            recsaltb_a(ii)%salt(isalt) = 0.
          enddo
        end do
        !point sources originating from outside the watershed
        allocate (recoutsaltb_d(imax))
        allocate (recoutsaltb_m(imax))
        allocate (recoutsaltb_y(imax))
        allocate (recoutsaltb_a(imax))
        do ii=1,imax
          allocate (recoutsaltb_d(ii)%salt(cs_db%num_salts))
          allocate (recoutsaltb_m(ii)%salt(cs_db%num_salts))
          allocate (recoutsaltb_y(ii)%salt(cs_db%num_salts))
          allocate (recoutsaltb_a(ii)%salt(cs_db%num_salts))
          do isalt=1,cs_db%num_salts
            recoutsaltb_d(ii)%salt(isalt) = 0.
            recoutsaltb_m(ii)%salt(isalt) = 0.
            recoutsaltb_y(ii)%salt(isalt) = 0.
            recoutsaltb_a(ii)%salt(isalt) = 0.
          enddo
        enddo
        
        !go back through the file, reading in the types and filenames
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        !loop through the point source files
        do ii = 1, imax
          
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat = eof) k, rec_salt(i)%name, rec_salt(i)%typ, rec_salt(i)%filename
          if (eof < 0) exit
        
          !open and read file contents
          if (rec_salt(i)%typ /= 4) then
            open (108,file = rec_salt(i)%filename)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            read (108,*,iostat=eof) nbyr
            if (eof < 0) exit
            read (108,*,iostat=eof) header
            if (eof < 0) exit 
        
            !record the type of point source (1 = from within the watershed; 2 = originating from outside the watershed)
            if(titldum == 'Incoming') then
              rec_salt(i)%pts_type = 2
            else
              rec_salt(i)%pts_type = 1
            endif
            
            select case (rec_salt(i)%typ)
              case (1) !! daily
                allocate (rec_salt(i)%hd_salt(366,nbyr))
                do jj=1,nbyr
                  do kk=1,366  
                    allocate (rec_salt(i)%hd_salt(kk,jj)%salt(cs_db%num_salts))
                    rec_salt(i)%hd_salt(kk,jj)%salt = 0.
                  enddo
                enddo
              case (2) !! monthly
                allocate (rec_salt(i)%hd_salt(12,nbyr))
                do jj=1,nbyr
                  do kk=1,12  
                    allocate (rec_salt(i)%hd_salt(kk,jj)%salt(cs_db%num_salts))
                    rec_salt(i)%hd_salt(kk,jj)%salt = 0.
                  enddo
                enddo
              case (3) !! annual
                allocate (rec_salt(i)%hd_salt(1,nbyr))
                do jj=1,nbyr
                  allocate (rec_salt(i)%hd_salt(1,jj)%salt(cs_db%num_salts))
                  rec_salt(i)%hd_salt(1,jj)%salt = 0.
                enddo
            end select 
           
            !! find data end time
            do 
              read (108,*,iostat=eof) jday, mo, day_mo, iyr
              if (eof < 0) exit
            end do
            rec_salt(i)%end_yr = iyr
            rewind (108)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            read (108,*,iostat=eof) nbyr
            if (eof < 0) exit
            read (108,*,iostat=eof) header
            if (eof < 0) exit 
       
            !! find data at start of simulation
            if (rec_salt(i)%typ == 0) then
              iyrs = 1
              iyr_prev = jday
            else
              do 
                read (108,*,iostat=eof) jday, mo, day_mo, iyr
                if (eof < 0) exit
                if (iyr == time%yrc) then
                  rec_salt(i)%start_yr = iyr
                  select case (rec_salt(i)%typ)
                    case (1) !! daily
                      istep = jday
                    case (2) !! monthly
                      istep = mo
                    case (3) !! annual
                      istep = 1
                  end select
                  exit
                if (eof < 0) exit
                end if
              end do
              backspace (108)
              iyr_prev = iyr
              iyrs = 1
            end if
       
            do
              iyr_prev = iyr
              if (rec_salt(i)%typ == 0) then
                !nothing
              else
                read (108,*,iostat=eof) jday, mo, day_mo, iyr, ob_typ, ob_name,         &
                   (rec_salt(i)%hd_salt(istep,iyrs)%salt(isalt),isalt=1,cs_db%num_salts)
              end if
              if (eof < 0) exit
              select case (rec_salt(i)%typ)
                case (1) !! daily
                  istep = istep + 1
                  if (jday == 365 .or. jday == 366) then
                    read (108,*,iostat=eof) jday, mo, day_mo, iyr
                    if (eof < 0) exit
                    backspace (108)
                    if (iyr /= iyr_prev) then
                      iyr_prev = iyr
                      iyrs = iyrs + 1
                      istep = 1
                    end if
                 end if
                 
                case (2) !! monthly
                  istep = istep + 1
                  if (mo == 12) then
                    iyrs = iyrs + 1
                    istep = 1
                  end if
            
                case (3) !! annual
                  iyrs = iyrs + 1

             end select
           
            end do   
            close (108)
          else
          
            if (rec_salt(i)%typ == 4) then
              !! xwalk with exco file to get sequential number
            end if
     
          end if
      
      end do
      
      close (107)
      exit
      enddo
      endif
      
      return
      end subroutine recall_read_salt
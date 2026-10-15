      !read point source data for constituents !rtb cs
      subroutine recall_read_cs

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module
      use time_module
      use exco_module
      use gwflow_module, only : div_conc_cs
      
      implicit none      
 
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      character(len=16) :: ob_name = ""
      character(len=8) :: ob_typ = ""
      integer :: imax = 0             !none       |end of loop
      integer :: iyr = 0              !           |
      integer :: jday = 0             !           |
      integer :: mo = 0               !           |
      integer :: day_mo = 0           !           |
      integer :: eof = 0              !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: nbyr = 0             !none       !number of years the land use occurred 
      integer :: k = 0                !           |
      integer :: iyrs = 0             !           | 
      integer :: iyr_prev = 0         !none       |previous year
      integer :: istep = 0            !           | 
      integer :: ii = 0               !none       |counter
      integer :: i = 0                !           |
      integer :: ics = 0
      integer :: jj = 0
      integer :: kk = 0
      
      eof = 0
      imax = 0
      if (cs_db%num_cs == 0) return

      !read all recall files
      inquire (file="cs_recall.rec", exist=i_exist)
      if (i_exist) then
      do
        open (107,file="cs_recall.rec")
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
        
        !allocate recall_cs and the cs balance arrays for every recall object in the model, and for every index in the file
        call alloc_rec_cs (Max(imax, sp_ob%recall))

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
          read (107,*,iostat = eof) k, rec_cs(i)%name, rec_cs(i)%typ, rec_cs(i)%filename
          if (eof < 0) exit
        
          !open and read file contents
          if (rec_cs(i)%typ /= 4) then
            open (108,file = rec_cs(i)%filename)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            read (108,*,iostat=eof) nbyr
            if (eof < 0) exit
            read (108,*,iostat=eof) header
            if (eof < 0) exit 
        
            !record the type of point source (1 = from within the watershed; 2 = originating from outside the watershed)
            if(titldum == 'Incoming') then
              rec_cs(i)%pts_type = 2
            else
              rec_cs(i)%pts_type = 1
            endif
            
            select case (rec_cs(i)%typ)
              case (1) !! daily
                allocate (rec_cs(i)%hd_cs(366,nbyr))
                do jj=1,nbyr
                  do kk=1,366  
                    allocate (rec_cs(i)%hd_cs(kk,jj)%cs(cs_db%num_cs), source = 0.)
                    rec_cs(i)%hd_cs(kk,jj)%cs = 0.
                  enddo
                enddo
              case (2) !! monthly
                allocate (rec_cs(i)%hd_cs(12,nbyr))
                do jj=1,nbyr
                  do kk=1,12  
                    allocate (rec_cs(i)%hd_cs(kk,jj)%cs(cs_db%num_cs), source = 0.)
                    rec_cs(i)%hd_cs(kk,jj)%cs = 0.
                  enddo
                enddo
              case (3) !! annual
                allocate (rec_cs(i)%hd_cs(1,nbyr))
                do jj=1,nbyr
                  allocate (rec_cs(i)%hd_cs(1,jj)%cs(cs_db%num_cs), source = 0.)
                  rec_cs(i)%hd_cs(1,jj)%cs = 0.
                enddo
            end select 
           
            !! find data end time
            do 
              read (108,*,iostat=eof) jday, mo, day_mo, iyr
              if (eof < 0) exit
            end do
            rec_cs(i)%end_yr = iyr
            rewind (108)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            read (108,*,iostat=eof) nbyr
            if (eof < 0) exit
            read (108,*,iostat=eof) header
            if (eof < 0) exit 
       
            !! find data at start of simulation
            if (rec_cs(i)%typ == 0) then
              iyrs = 1
              iyr_prev = jday
            else
              do 
                read (108,*,iostat=eof) jday, mo, day_mo, iyr
                if (eof < 0) exit
                if (iyr == time%yrc) then
                  rec_cs(i)%start_yr = iyr
                  select case (rec_cs(i)%typ)
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
              if (rec_cs(i)%typ == 0) then
                !nothing
              else
                read (108,*,iostat=eof) jday, mo, day_mo, iyr, ob_typ, ob_name,    &
                     (rec_cs(i)%hd_cs(istep,iyrs)%cs(ics),ics=1,cs_db%num_cs)
              end if
              if (eof < 0) exit
              select case (rec_cs(i)%typ)
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
          
            if (rec_cs(i)%typ == 4) then
              !! xwalk with exco file to get sequential number
            end if
     
          end if
      
      end do
      
      close (107)
      exit
      enddo
      endif

      !without cs_recall.rec, every recall object still gets zero constituent loads
      if (.not. allocated(rec_cs)) call alloc_rec_cs (sp_ob%recall)

      return

      contains

      subroutine alloc_rec_cs (n)
        integer, intent (in) :: n
        integer :: irec
        allocate (rec_cs(0:n))
        !point sources originating from within the watershed
        allocate (reccsb_d(n))
        allocate (reccsb_m(n))
        allocate (reccsb_y(n))
        allocate (reccsb_a(n))
        !point sources originating from outside the watershed
        allocate (recoutcsb_d(n))
        allocate (recoutcsb_m(n))
        allocate (recoutcsb_y(n))
        allocate (recoutcsb_a(n))
        do irec = 1, n
          allocate (reccsb_d(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (reccsb_m(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (reccsb_y(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (reccsb_a(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (recoutcsb_d(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (recoutcsb_m(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (recoutcsb_y(irec)%cs(cs_db%num_cs), source = 0.)
          allocate (recoutcsb_a(irec)%cs(cs_db%num_cs), source = 0.)
        end do
        !recall_cs also zeroes the diversion concentrations, which gwflow_read allocates only when gwflow runs with solutes
        if (.not. allocated(div_conc_cs)) allocate (div_conc_cs(cs_db%num_cs,sp_ob%recall), source = 0.)
      end subroutine alloc_rec_cs

      end subroutine recall_read_cs
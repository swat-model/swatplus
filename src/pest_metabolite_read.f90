      subroutine pest_metabolite_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use pesticide_data_module
      use constituent_mass_module
      
      implicit none

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: parent_name
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ip                   !none       |pestdb counter
      integer :: ipb                  !none       |basin sequential pesticide counter
      integer :: imeta                !none       |metabolite counter
      integer :: iparent              !none       |parent counter
      integer :: num_metab            !none       |number of matabolites for each parent
      
      eof = 0
      imax = 0
      
      inquire (file="pest_metabolite.pes", exist=i_exist)
      if (i_exist ) then
      do
        open (106,file="pest_metabolite.pes")
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (106,*,iostat=eof) titldum, num_metab
            do ip = 1, num_metab
              read (106,*,iostat=eof) titldum
            end do
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        rewind (106)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
        
        do iparent = 1, imax
          read (106,*,iostat=eof) parent_name, num_metab
          if (eof < 0) exit
           
          !! xwalk with pestdb
          do ip = 1, db_mx%pestparm
            if (parent_name == pestdb(ip)%name) then
              allocate (pestcp(ip)%daughter(num_metab))
              pestcp(ip)%num_metab = num_metab
              
              do imeta = 1, num_metab
                read (106,*,iostat=eof) pestcp(ip)%daughter(imeta)%name,        &
                                        pestcp(ip)%daughter(imeta)%foliar_fr,   &
                                        pestcp(ip)%daughter(imeta)%soil_fr,     &
                                        pestcp(ip)%daughter(imeta)%aq_fr,       &
                                        pestcp(ip)%daughter(imeta)%ben_fr
                
                !! xwalk with constituent data for basin
                do ipb = 1, cs_db%num_pests
                  if (pestcp(ip)%daughter(imeta)%name == cs_db%pests(ipb)) then
                    pestcp(ip)%daughter(imeta)%num = ipb
                  end if
                end do
              end do

            end if
          end do
        end do
        
      end do
      end if
      
      close (106)
      return
      end subroutine pest_metabolite_read
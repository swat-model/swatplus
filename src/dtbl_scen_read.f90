      subroutine dtbl_scen_read
      
      use maximum_data_module
      use reservoir_data_module
      use landuse_data_module
      use mgt_operations_module
      use tillage_data_module
      use fertilizer_data_module
      use input_file_module
      use conditional_module
      
      implicit none
                  
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: i                    !none       |counter 
      integer :: mdtbl                !none       |ending of loop
      integer :: ic                   !none       |counter 
      integer :: ial                  !none       |counter 
      integer :: iac                  !none       !counter 
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ilum                 !none       |counter      
      
      mdtbl = 0
      eof = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_cond%dtbl_scen, exist=i_exist)
      if (.not. i_exist .or. in_cond%dtbl_scen == "null") then
        allocate (dtbl_scen(0:0)) 
      else
        do
          open (107,file=in_cond%dtbl_scen)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mdtbl
          if (eof < 0) exit
          read (107,*,iostat=eof)
          if (eof < 0) exit
          allocate (dtbl_scen(0:mdtbl))

          do i = 1, mdtbl
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) dtbl_scen(i)%name, dtbl_scen(i)%conds, dtbl_scen(i)%alts, dtbl_scen(i)%acts
            if (eof < 0) exit
            allocate (dtbl_scen(i)%cond(dtbl_scen(i)%conds))
            allocate (dtbl_scen(i)%alt(dtbl_scen(i)%conds,dtbl_scen(i)%alts))
            allocate (dtbl_scen(i)%act(dtbl_scen(i)%acts))
            allocate (dtbl_scen(i)%act_hit(dtbl_scen(i)%alts))
            allocate (dtbl_scen(i)%act_typ(dtbl_scen(i)%acts))
            allocate (dtbl_scen(i)%act_app(dtbl_scen(i)%acts))
            allocate (dtbl_scen(i)%act_outcomes(dtbl_scen(i)%acts,dtbl_scen(i)%alts))
            
            !read conditions and condition alternatives
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do ic = 1, dtbl_scen(i)%conds
              read (107,*,iostat=eof) dtbl_scen(i)%cond(ic), (dtbl_scen(i)%alt(ic,ial), ial = 1, dtbl_scen(i)%alts)
              if (eof < 0) exit
            end do
                        
            !read actions and action outcomes
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do iac = 1, dtbl_scen(i)%acts
              read (107,*,iostat=eof) dtbl_scen(i)%act(iac), (dtbl_scen(i)%act_outcomes(iac,ial), ial = 1, dtbl_scen(i)%alts)
              if (eof < 0) exit
            end do
            read (107,*,iostat=eof)
            if (eof < 0) exit
            
            !cross walk characters to get array numbers
            do iac = 1, dtbl_scen(i)%acts
                select case (dtbl_scen(i)%act(iac)%typ)
                case ("lu_change")
                  do ilum = 1, db_mx%landuse
                    if (dtbl_scen(i)%act(iac)%file_pointer == lum(ilum)%name) then
                      dtbl_scen(i)%act_typ(iac) = ilum
                      exit
                    end if
                  end do
                end select
                
            end do
            
          end do
          db_mx%dtbl_scen = mdtbl
          exit
        enddo
      endif
      close (107)
      
      return  
      end subroutine dtbl_scen_read
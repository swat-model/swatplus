      subroutine dtbl_flocon_read
      
      use maximum_data_module
      use hydrograph_module
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
      integer :: idb                  !none       |counter
      integer :: iob                  !none       |counter

      mdtbl = 0
      eof = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_cond%dtbl_flo, exist=i_exist)
      if (.not. i_exist .or. in_cond%dtbl_flo == "null") then
        allocate (dtbl_flo(0:0)) 
      else
        do
          open (107,file=in_cond%dtbl_flo)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mdtbl
          if (eof < 0) exit
          read (107,*,iostat=eof)
          if (eof < 0) exit
          allocate (dtbl_flo(0:mdtbl))

          do i = 1, mdtbl
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) dtbl_flo(i)%name, dtbl_flo(i)%conds, dtbl_flo(i)%alts, dtbl_flo(i)%acts
            if (eof < 0) exit
            allocate (dtbl_flo(i)%cond(dtbl_flo(i)%conds))
            allocate (dtbl_flo(i)%alt(dtbl_flo(i)%conds,dtbl_flo(i)%alts))
            allocate (dtbl_flo(i)%act(dtbl_flo(i)%acts))
            allocate (dtbl_flo(i)%act_hit(dtbl_flo(i)%alts))
            allocate (dtbl_flo(i)%act_typ(dtbl_flo(i)%acts))
            allocate (dtbl_flo(i)%act_app(dtbl_flo(i)%acts))
            allocate (dtbl_flo(i)%act_outcomes(dtbl_flo(i)%acts,dtbl_flo(i)%alts))
            
            !read conditions and condition alternatives
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do ic = 1, dtbl_flo(i)%conds
              read (107,*,iostat=eof) dtbl_flo(i)%cond(ic), (dtbl_flo(i)%alt(ic,ial), ial = 1, dtbl_flo(i)%alts)
              if (eof < 0) exit
            end do
                        
            !read actions and action outcomes
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do iac = 1, dtbl_flo(i)%acts
              read (107,*,iostat=eof) dtbl_flo(i)%act(iac), (dtbl_flo(i)%act_outcomes(iac,ial), ial = 1, dtbl_flo(i)%alts)
              if (eof < 0) exit
              
              !! if divert action, xwalk fp with flo_con decision table
              if (dtbl_flo(i)%act(iac)%typ == "divert" .and. dtbl_flo(i)%act(iac)%option == "recall") then
                do idb = 1, db_mx%recall_max
                  if (dtbl_flo(i)%act(iac)%file_pointer == recall(idb)%filename) then
                    dtbl_flo(i)%act_typ(iac) = idb
                    exit
                  end if
                end do
              end if
                
            end do
            read (107,*,iostat=eof)
            if (eof < 0) exit

          end do
          db_mx%dtbl_flo = mdtbl
          exit
        enddo
      endif
      
      !cross walk dtbl name with connect file ruleset
      do iob = 1, sp_ob%objs
        if (ob(iob)%ruleset /= "null") then
          do idb = 1, db_mx%dtbl_flo
            if (dtbl_flo(idb)%name == ob(iob)%ruleset) then
              ob(iob)%flo_dtbl = idb
              exit
            end if
          end do
        end if
      end do
      
      close (107)
      
      return  
      end subroutine dtbl_flocon_read
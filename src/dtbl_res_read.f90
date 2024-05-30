      subroutine dtbl_res_read
      
      use maximum_data_module
      use reservoir_data_module
      use landuse_data_module
      use mgt_operations_module
      use tillage_data_module
      use fertilizer_data_module
      use input_file_module
      use conditional_module
      use hydrograph_module, only : recall
      
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
       
      mdtbl = 0
      eof = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_cond%dtbl_res, exist=i_exist)
      if (.not. i_exist .or. in_cond%dtbl_res == "null") then
        allocate (dtbl_res(0:0)) 
      else
        do
          open (107,file=in_cond%dtbl_res)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mdtbl
          if (eof < 0) exit
          read (107,*,iostat=eof)
          if (eof < 0) exit
          allocate (dtbl_res(0:mdtbl))

          do i = 1, mdtbl
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) dtbl_res(i)%name, dtbl_res(i)%conds, dtbl_res(i)%alts, dtbl_res(i)%acts
            if (eof < 0) exit
            allocate (dtbl_res(i)%cond(dtbl_res(i)%conds))
            allocate (dtbl_res(i)%alt(dtbl_res(i)%conds,dtbl_res(i)%alts))
            allocate (dtbl_res(i)%act(dtbl_res(i)%acts))
            allocate (dtbl_res(i)%act_hit(dtbl_res(i)%alts))
            allocate (dtbl_res(i)%act_typ(dtbl_res(i)%acts))
            allocate (dtbl_res(i)%act_app(dtbl_res(i)%acts))
            allocate (dtbl_res(i)%act_outcomes(dtbl_res(i)%acts,dtbl_res(i)%alts))
            
            !read conditions and condition alternatives
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do ic = 1, dtbl_res(i)%conds
              read (107,*,iostat=eof) dtbl_res(i)%cond(ic), (dtbl_res(i)%alt(ic,ial), ial = 1, dtbl_res(i)%alts)
              if (eof < 0) exit
            end do
                        
            !read actions and action outcomes
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do iac = 1, dtbl_res(i)%acts
              read (107,*,iostat=eof) dtbl_res(i)%act(iac), (dtbl_res(i)%act_outcomes(iac,ial), ial = 1, dtbl_res(i)%alts)
              if (eof < 0) exit
            end do
            read (107,*,iostat=eof)
            if (eof < 0) exit
            
            !cross walk characters to get array numbers
            do iac = 1, dtbl_res(i)%acts
                select case (dtbl_res(i)%act(iac)%typ)
                                  
                case ("release")
                  select case (dtbl_res(i)%act(iac)%option)
                    case ("weir")
                    do idb = 1, db_mx%res_weir
                      if (dtbl_res(i)%act(iac)%file_pointer == res_weir(idb)%name) then
                        dtbl_res(i)%act_typ(iac) = idb
                        exit
                      end if
                    end do
            
                    case ("meas")
                    do idb = 1, db_mx%recall_max
                      if (dtbl_res(i)%act(iac)%file_pointer == recall(idb)%name) then
                        dtbl_res(i)%act_typ(iac) = idb
                        exit
                      end if
                    end do
                    end select
                    
                 end select
                
            end do
            
          end do
          db_mx%dtbl_res = mdtbl
          exit
        enddo
      endif
      close (107)
      
      return  
      end subroutine dtbl_res_read
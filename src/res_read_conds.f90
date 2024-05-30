      subroutine res_read_conds
    
      use reservoir_conditions_module
      use maximum_data_module

      implicit none
      
      character (len=80) :: title
      integer :: max_table, tnum_conds, ii, ictbl, isub_con, icc, imod
      integer ::  eof
      logical :: i_exist         !                |check to determine if file exists
   
      inquire (file="res_conds.dat", exist=i_exist)
      if (.not. i_exist) return
      open (100,file="res_conds.dat")
      read (100,*,iostat=eof) title
      if (eof < 0) return
      read (100,*,iostat=eof) max_table
      if (eof < 0 .or. max_table < 1) return
      allocate (ctbl(max_table))
      
      db_mx%ctbl_res = max_table
      
      !! read data for each condition table
      do ictbl = 1, max_table
        read (100,*) ctbl(ictbl)%name, ctbl(ictbl)%num_conds, ctbl(ictbl)%num_modules
        allocate (ctbl(ictbl)%conds(ctbl(ictbl)%num_conds)) 
        allocate (ctbl(ictbl)%mods(ctbl(ictbl)%num_modules)) 
      
        !! loop through all conditions
        do ii = 1, ctbl(ictbl)%num_conds 
          !! read total number of sub conditions
          read (100,*) isub_con
          backspace (100)
          allocate (ctbl(ictbl)%conds(ii)%scon(isub_con)) 
          read (100,*) ctbl(ictbl)%conds(ii)%num_conds, (ctbl(ictbl)%conds(ii)%scon(icc), icc = 1, isub_con),    &
              ctbl(ictbl)%conds(ii)%action
        end do
      
        !! read each module
        do imod = 1, ctbl(ictbl)%num_modules
          read (100,*) tnum_conds
          allocate (ctbl(ictbl)%mods(imod)%con(tnum_conds))
          ctbl(ictbl)%mods(imod)%num_conds = tnum_conds
      
          !! read all subconditions
          do ii = 1, tnum_conds
            read (100,*) isub_con
            backspace (100)
            allocate (ctbl(ictbl)%mods(imod)%con(ii)%scon(isub_con)) 
            read (100,*) ctbl(ictbl)%mods(imod)%con(ii)%num_conds, (ctbl(ictbl)%mods(imod)%con(ii)%scon(icc), icc = 1, isub_con),  &
                ctbl(ictbl)%mods(imod)%con(ii)%action
          end do
        end do
      end do
    
	  return      
      end subroutine res_read_conds
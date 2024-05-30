      subroutine hyd_connect_out
    
      use basin_module
      use hydrograph_module
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  
      
      implicit none
      
      integer :: ii             !              |
      integer :: i              !              | 

      ii = 0
      icmd = sp_ob1%objs

      if (pco%hydcon == "y") then
        do while (icmd /= 0)
          ii = ii + 1
          write (7000,*) ii, ob(icmd)%name, ob(icmd)%typ,               &  
           ob(icmd)%props, ob(icmd)%props2, ob(icmd)%src_tot,           &
           ob(icmd)%rcv_tot, (ob(icmd)%obj_out,ob(icmd)%obtyp_out(i),   &
           ob(icmd)%obtypno_out(i), ob(icmd)%htyp_out(i), i = 1,        &
           ob(icmd)%src_tot)
          if (pco%csvout == "y") then 
            write (7001,'(*(G0.3,:","))') ii, ob(icmd)%name, ob(icmd)%typ,  &  
            ob(icmd)%props, ob(icmd)%props2, ob(icmd)%src_tot,              &
             ob(icmd)%rcv_tot, (ob(icmd)%obj_out,ob(icmd)%obtyp_out(i),     &
             ob(icmd)%obtypno_out(i), ob(icmd)%htyp_out(i), i = 1,          &
             ob(icmd)%src_tot) 
          end if
         
         icmd = ob(icmd)%cmd_next
        end do
      endif
!100   format (i4,1x,a,11i6)

      close(172)
      
      return  
      end subroutine hyd_connect_out
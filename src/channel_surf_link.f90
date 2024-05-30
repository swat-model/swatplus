      subroutine channel_surf_link 
                 
      use hydrograph_module
      use channel_module
      use ru_module
      use maximum_data_module
      use hru_module, only : hru
      
      implicit none
      
      character (len=3) :: iobtyp   !none          |object type
      integer :: ics                !none          |counter 
      integer :: ii                 !none          |counter 
      integer :: i                  !units         |description  
      integer :: iob                !units         |description   
      integer :: ihru               !none          |counter 
      integer :: ichan
      real :: tot_ha                !units         |description 
      integer :: iobtypno

      tot_ha = 0
      
      do ics = 1, db_mx%ch_surf
        
        do ii = 1, ch_sur(ics)%num

          ichan =  ch_sur(ics)%chnum
          iobtyp = ch_sur(ics)%obtyp(ii)     !object type
          iobtypno = ch_sur(ics)%obtypno(ii)
          select case (iobtyp)
          case ("hru")   !hru
            ob(ics)%obj_out(ii) = sp_ob1%hru + ob(ics)%obtypno_out(ii) - 1
            iob = ob(ics)%obj_out(ii)
            ob(iob)%flood_ch_lnk = ichan   !pointer back to channel-hru link
            ob(iob)%flood_ch_elem = ics   !pointer to landscape element - 1 nearest to channel
            
            ihru = ch_sur(ics)%obtypno(ii)
            hru(ihru)%wet_fp = "y"
            tot_ha = tot_ha + ob(iob)%area_ha
            
          case ("hlt")   !hru_lte
            ob(i)%obj_out(ii) = sp_ob1%hru_lte + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            ob(iob)%flood_ch_lnk = ics   !pointer back to channel-hru link
            ob(iob)%flood_ch_elem = ii   !pointer to landscape element - 1 nearest to channel
            
            ihru = ch_sur(ics)%obtypno(ii)
            tot_ha = tot_ha + ob(iob)%area_ha
            
          case ("ru")   !routing unit
            iru = ch_sur(ics)%obtypno(ii)

            !set flood plain link and landscape element (1==closest to river)
            do ihru = 1, ru_def(iru)%num_tot
              iob = ru_def(iru)%num(ihru)
              ob(iob)%flood_ch_lnk = ichan   !pointer back to channel
              ob(iob)%flood_ch_elem = ics   !pointer to link
              tot_ha = tot_ha + ob(iob)%area_ha
              hru(iob)%wet_fp = "y"
            end do
            
          case ("cha")   !channel
            !
          case ("sdc")   !swat-deg channel
            !
          end select
        end do

        !! loop again to get fractions
!        do ii = 1, ch_sur(ics)%num
!          iobtyp = ch_sur(ics)%obtyp(ii)     !object type
!          select case (iobtyp)
!          case ("hru")   !hru
!            ob(i)%obj_out(ii) = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
!            iob = ob(i)%obj_out(ii)
!            ob(iob)%flood_frac = ob(iob)%area_ha / tot_ha

!          case ("hlt")   !hru_lte
!            ob(i)%obj_out(ii) = sp_ob1%hru_lte + ob(i)%obtypno_out(ii) - 1
!            iob = ob(i)%obj_out(ii)
!            ob(iob)%flood_frac = ob(iob)%area_ha / tot_ha

!          case ("ru")   !routing unit
!            ob(i)%obj_out(ii) = sp_ob1%ru + ob(i)%obtypno_out(ii) - 1
!            iob = ob(i)%obj_out(ii)
!            iru = ch_sur(ics)%obtypno(ii)
 !           ith = ru(iru)%dbs%toposub_db

            !set flood plain link and landscape element (1==closest to river)
 !           do iele = 1, ru_def(iru)%num_tot
 !             iob =  ru_def(iru)%num(ihru)
 !             ob(iob)%flood_frac = (ru_elem(iele)%frac * ob(iob)%area_ha) / tot_ha
 !           end do
            
 !         case ("cha")   !channel
            !
 !        case ("sdc")   !swat-deg channel
            !
 !         end select
 !       end do
        
      end do    ! ics = 1, db_mx%ch_surf
        
        return

      end subroutine channel_surf_link
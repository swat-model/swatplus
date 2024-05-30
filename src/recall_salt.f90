      !rtb salt
      !include salt mass from point sources
      subroutine recall_salt(irec)
      
      
      use basin_module
      use hydrograph_module
      use time_module
      use constituent_mass_module
      use ch_salt_module
      
      implicit none 

      !incoming variables
      integer :: irec
      
      !local variables
      integer :: isalt				!            |salt ion counter
      integer :: ichan        !            |id of source channel
      real :: salt_conc       !g/m3        |concentration of salt ion in source channel
      real :: div_mass        !kg          |mass of salt ion in diversion water
      
      
      !depending on the point source type, add/remove salt mass to object
      obcs(icmd)%hd(1)%salt = 0.
      if (cs_db%num_salts > 0) then
        select case (rec_salt(irec)%typ)  
          case (1)    !daily
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do isalt=1,cs_db%num_salts
                if(recall(irec)%hd(time%day,time%yrs)%flo < 0) then
                  !diversion: remove mass    
                  ichan = ob(icmd)%obtypno_out(1) !channel from which water is diverted
                  if(ch_stor(ichan)%flo > 10.) then !only proceed if channel has water
                    salt_conc = (ch_water(ichan)%salt(isalt)*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
                    div_mass = (salt_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day  
                    if((div_mass*(-1)) > ch_water(ichan)%salt(isalt)) then
                      div_mass = ch_water(ichan)%salt(isalt) * (-1) !only take what is there
                    endif
                    ch_water(ichan)%salt(isalt) = ch_water(ichan)%salt(isalt) + div_mass
                    chsalt_d(ichan)%salt(isalt)%div = div_mass
                  endif
                else
                  !source: add mass
                  obcs(icmd)%hd(1)%salt(isalt) = rec_salt(irec)%hd_salt(time%day,time%yrs)%salt(isalt)
								endif
              enddo
              if(rec_salt(irec)%pts_type.eq.1) then
                do isalt=1,cs_db%num_salts
                  recsaltb_d(irec)%salt(isalt) = obcs(icmd)%hd(1)%salt(isalt)
                enddo
              else
                do isalt=1,cs_db%num_salts
                  recoutsaltb_d(irec)%salt(isalt) = obcs(icmd)%hd(1)%salt(isalt)
                enddo
              endif
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (2)    !monthly
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do isalt=1,cs_db%num_salts
                obcs(icmd)%hd(1)%salt(isalt) = rec_salt(irec)%hd_salt(time%mo,time%yrs)%salt(isalt)
              enddo
              if(rec_salt(irec)%pts_type.eq.1) then
                do isalt=1,cs_db%num_salts
                  recsaltb_d(irec)%salt(isalt) = rec_salt(irec)%hd_salt(time%mo,time%yrs)%salt(isalt)
                enddo
              else
                do isalt=1,cs_db%num_salts
                  recoutsaltb_d(irec)%salt(isalt) = rec_salt(irec)%hd_salt(time%mo,time%yrs)%salt(isalt)
                enddo
              endif
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (3)    !annual
            if (time%yrc >= rec_salt(irec)%start_yr .or. time%yrc <= rec_salt(irec)%end_yr) then
              do isalt=1,cs_db%num_salts
                obcs(icmd)%hd(1)%salt(isalt) = rec_salt(irec)%hd_salt(1,time%yrs)%salt(isalt)
              enddo
              if(rec_salt(irec)%pts_type.eq.1) then
                do isalt=1,cs_db%num_salts
								  recsaltb_d(irec)%salt(isalt) = rec_salt(irec)%hd_salt(1,time%yrs)%salt(isalt)
                enddo
              else
                do isalt=1,cs_db%num_salts
                  recoutsaltb_d(irec)%salt(isalt) = rec_salt(irec)%hd_salt(1,time%yrs)%salt(isalt)
                enddo
              endif
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (4)    !average annual
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do isalt=1,cs_db%num_salts
                obcs(icmd)%hd(1)%salt(isalt) = rec_salt(irec)%hd_salt(1,1)%salt(isalt)
              enddo
              if(rec_salt(irec)%pts_type.eq.1) then
                do isalt=1,cs_db%num_salts
                  recsaltb_d(irec)%salt(isalt) = rec_salt(irec)%hd_salt(1,1)%salt(isalt)
                enddo
              else
                do isalt=1,cs_db%num_salts
                  recoutsaltb_d(irec)%salt(isalt) = rec_salt(irec)%hd_salt(1,1)%salt(isalt)
                enddo
              endif
            endif  
          end select
      endif
    
      return
      end subroutine recall_salt
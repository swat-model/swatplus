      !rtb cs
      !include constituent mass from point sources
      subroutine recall_cs(irec)
      
      
      use basin_module
      use hydrograph_module
      use time_module
      use constituent_mass_module
      use ch_cs_module
      
      implicit none 

      !incoming variables
      integer :: irec
      
      !local variables
      integer :: ics					!            |constituent counter
      integer :: ichan        !            |id of source channel
      real :: cs_conc         !g/m3        |concentration of constituent in source channel
      real :: div_mass        !kg          |mass of constituent in diversion water
      
      
      !depending on the point source type, add/remove constituent mass to object
      obcs(icmd)%hd(1)%cs = 0.
      if (cs_db%num_cs > 0) then
        select case (rec_cs(irec)%typ)  
          case (1)    !daily
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do ics=1,cs_db%num_cs
                if(recall(irec)%hd(time%day,time%yrs)%flo < 0) then
                  !diversion: remove mass from channel directly
                  ichan = ob(icmd)%obtypno_out(1) !channel from which water is diverted
                  if(ch_stor(ichan)%flo > 10.) then !only proceed if channel has water
                    cs_conc = (ch_water(ichan)%cs(ics)*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
                    div_mass = (cs_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day 
                    if((div_mass*(-1)) > ch_water(ichan)%cs(ics)) then
                      div_mass = ch_water(ichan)%cs(ics) * (-1) !only take what is there
                    endif
                    ch_water(ichan)%cs(ics) = ch_water(ichan)%cs(ics) + div_mass
                    chcs_d(ichan)%cs(ics)%div = div_mass
                  endif
                else
                  !source: add mass
                  obcs(icmd)%hd(1)%cs(ics) = rec_cs(irec)%hd_cs(time%day,time%yrs)%cs(ics)
								endif
              enddo
              if(rec_cs(irec)%pts_type.eq.1) then
                reccsb_d(irec)%cs = obcs(icmd)%hd(1)%cs
              else
                recoutcsb_d(irec)%cs = obcs(icmd)%hd(1)%cs
              endif
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (2)    !monthly
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do ics=1,cs_db%num_cs
                obcs(icmd)%hd(1)%cs(ics) = rec_cs(irec)%hd_cs(time%mo,time%yrs)%cs(ics)
              enddo
              if(rec_cs(irec)%pts_type.eq.1) then
                reccsb_d(irec)%cs = rec_cs(irec)%hd_cs(time%mo,time%yrs)%cs
              else
                recoutcsb_d(irec)%cs = rec_cs(irec)%hd_cs(time%mo,time%yrs)%cs
              endif
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (3)    !annual
            if (time%yrc >= rec_cs(irec)%start_yr .or. time%yrc <= rec_cs(irec)%end_yr) then
              do ics=1,cs_db%num_cs
                obcs(icmd)%hd(1)%cs(ics) = rec_cs(irec)%hd_cs(1,time%yrs)%cs(ics)
              enddo
              if(rec_cs(irec)%pts_type.eq.1) then
								reccsb_d(irec)%cs = rec_cs(irec)%hd_cs(1,time%yrs)%cs
              else
                recoutcsb_d(irec)%cs = rec_cs(irec)%hd_cs(1,time%yrs)%cs
              endif
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (4)    !average annual
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do ics=1,cs_db%num_cs
                obcs(icmd)%hd(1)%cs(ics) = rec_cs(irec)%hd_cs(1,1)%cs(ics)
              enddo
              if(rec_cs(irec)%pts_type.eq.1) then
                reccsb_d(irec)%cs = rec_cs(irec)%hd_cs(1,1)%cs
              else
                recoutcsb_d(irec)%cs = rec_cs(irec)%hd_cs(1,1)%cs
              endif
            endif  
          end select
      endif

      
      return
      end subroutine recall_cs
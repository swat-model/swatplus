      subroutine cli_atmodep_time_control

      use climate_module
      use time_module
      
      implicit none
      
      if (atmodep_cont%num_sta > 0) then
        if (atmodep_cont%first == 1) then
          if (atmodep_cont%timestep == "yr") then
            if (atmodep_cont%yr_init == time%yrc) then
              atmodep_cont%ts = 1
              atmodep_cont%first = 0
            end if
          end if 
          if (atmodep_cont%timestep =="mo") then
            if (atmodep_cont%yr_init == time%yrc .and. atmodep_cont%mo_init == time%mo) then
              atmodep_cont%ts = 1
              atmodep_cont%first = 0
            end if
          end if         
        else
          if (atmodep_cont%timestep == "yr") then
            if (time%end_yr == 1) then
              atmodep_cont%ts = atmodep_cont%ts + 1
              !atmodep_cont%ts = amin0(atmodep_cont%ts, atmodep_cont%num)
            end if 
          end if
          if (atmodep_cont%timestep == "mo") then
            if (time%end_mo == 1) then
              atmodep_cont%ts = atmodep_cont%ts + 1
              !atmodep_cont%ts = amin0(atmodep_cont%ts, atmodep_cont%num)
            end if
          end if
        end if
      end if
                
      end subroutine cli_atmodep_time_control
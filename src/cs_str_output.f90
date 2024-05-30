      subroutine cs_str_output !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine prints out daily constituent data for specified channels
      
      use hydrograph_module
      use constituent_mass_module
      use ch_cs_module
      use time_module
      
      implicit none 
       
      integer :: i                   !           |counter
      integer :: chan_id             !           |id of channel
      integer :: elem_count          !           |counter
      real :: line_array(2000)       !           |array to hold all daily values (flow, conc, load)
      
      
      !print out daily conc and loads for specified channels (in cs_streamobs file)
      if(cs_obs_file == 1) then
      
        !prepare daily data
        line_array = 0.
        elem_count = 0
        !streamflow (m3/sec)
        do i=1,cs_str_nobs
          elem_count = elem_count + 1
          chan_id = cs_str_obs(i)
          line_array(elem_count) = ch_out_d(chan_id)%flo
        enddo
        !seo4 concentration (g/m3)
        do i=1,cs_str_nobs
          elem_count = elem_count + 1
          chan_id = cs_str_obs(i)
          line_array(elem_count) = chcs_d(chan_id)%cs(1)%conc
        enddo
        !seo3 concentration (g/m3)
        do i=1,cs_str_nobs
          elem_count = elem_count + 1
          chan_id = cs_str_obs(i)
          line_array(elem_count) = chcs_d(chan_id)%cs(2)%conc
        enddo
        !seo4 load (kg/day)
        do i=1,cs_str_nobs
          elem_count = elem_count + 1
          chan_id = cs_str_obs(i)
          line_array(elem_count) = chcs_d(chan_id)%cs(1)%tot_out
        enddo
        !seo3 load (kg/day)
        do i=1,cs_str_nobs
          elem_count = elem_count + 1
          chan_id = cs_str_obs(i)
          line_array(elem_count) = chcs_d(chan_id)%cs(2)%tot_out
        enddo
        !no3 load (kg/day)
        do i=1,cs_str_nobs
          elem_count = elem_count + 1
          chan_id = cs_str_obs(i)
          line_array(elem_count) = ch_out_d(chan_id)%no3
        enddo
        
        !write out to file
        write(8200,100) time%yrc,time%day,(line_array(i),i=1,elem_count)
        
      endif !check if daily output is active


100   format(i8,i8,2000e14.6)
      
      return
      end !cs_str_output

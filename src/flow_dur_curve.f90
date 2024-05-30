      subroutine flow_dur_curve

      use time_module
      use hydrograph_module
      
      implicit none  

      real :: sum                         !             |
      integer :: iyr                      !none         |number of years
      integer :: next                     !             |
      integer :: npts                     !             |
      integer :: ipts                     !             |
      integer :: iprv                     !             |
      integer :: mle                      !             |
      integer :: nprob                    !             |
      integer :: iday                     !             |
      integer :: mfe                      !             |
      integer :: iyr_ch                   !             | 

        !set linked list for daily flow duration curves
        ob(icmd)%fdc_ll(time%day)%val = ob(icmd)%hd(1)%flo
        next = ob(icmd)%fdc%mfe
        npts = time%day - 1
        do ipts = 1, npts
          if (ob(icmd)%fdc_ll(time%day)%val >= ob(icmd)%fdc_ll(next)%val) then
            ob(icmd)%fdc_ll(time%day)%next = next
            if (ipts == 1) then
              ob(icmd)%fdc%mfe = time%day
            else
              ob(icmd)%fdc_ll(iprv)%next = time%day
            end if
            exit
          end if
          iprv = next
          next = ob(icmd)%fdc_ll(next)%next
        end do
        if (npts > 0 .and. ipts == npts + 1) then
          mle = ob(icmd)%fdc%mle
          ob(icmd)%fdc_ll(mle)%next = time%day
          ob(icmd)%fdc%mle = time%day
        end if
        !set linked list for daily flow duration curves
        
        !save flow duration curve probabilities for the year
        if (time%end_yr == 1) then
          sum = 0.
          nprob = 1
          next = ob(icmd)%fdc%mfe
          do iday = 1, time%day
            if (iday == fdc_days(nprob)) then
              ob(icmd)%fdc%yr(time%yrs)%p(nprob) = ob(icmd)%fdc_ll(next)%val
              nprob = nprob + 1
              if (nprob > fdc_npts) exit
            end if
            next = ob(icmd)%fdc_ll(next)%next
          end do
          !sum all values to get mean
          do iday = 1, time%day
            sum = sum + ob(icmd)%fdc_ll(iday)%val
          end do
          ob(icmd)%fdc%yr(time%yrs)%mean = sum / float(iday)
          mfe = ob(icmd)%fdc%mfe
          mle = ob(icmd)%fdc%mle
          ob(icmd)%fdc%yr(time%yrs)%max = ob(icmd)%fdc_ll(mfe)%val
          ob(icmd)%fdc%yr(time%yrs)%min = ob(icmd)%fdc_ll(mle)%val
          !write ob(icmd)%fdc%p before we reinitialize
          ob(icmd)%fdc%mfe = 1
          ob(icmd)%fdc%mle = 1
        end if
        
        !save flow duration curve probabilities for the year
        
        !master duration curve from annual curves (median)
        if (time%end_sim == 1) then
          do nprob = 1, fdc_npts
            ob(icmd)%fdc%mfe = 1
            ob(icmd)%fdc%mle = 1

            !set linked list for each year
            do iyr = 1, time%nbyr
              ob(icmd)%fdc_lla(iyr)%val = ob(icmd)%fdc%yr(iyr)%p(nprob)
              next = ob(icmd)%fdc%mfe
              npts = iyr - 1
              do ipts = 1, npts
                if (ob(icmd)%fdc_lla(iyr)%val <= ob(icmd)%fdc_lla(next)%val) then
                  ob(icmd)%fdc_lla(iyr)%next = next
                  if (ipts == 1) then
                    ob(icmd)%fdc%mfe = iyr
                  else
                    ob(icmd)%fdc_lla(iprv)%next = iyr
                  end if
                  exit
                end if
                iprv = next
                next = ob(icmd)%fdc_lla(next)%next
              end do  !ipts
              if (npts > 0 .and. ipts == npts + 1) then
                mle = ob(icmd)%fdc%mle
                ob(icmd)%fdc_lla(mle)%next = iyr
                ob(icmd)%fdc%mle = iyr
              end if
            end do    !iyr
          
          !calc mean, abs max and min
          sum = 0.
          do iyr = 1, time%nbyr
            sum = sum + ob(icmd)%fdc%yr(iyr)%mean
            ob(icmd)%fdc%p_md%max = Max (ob(icmd)%fdc%p_md%max, ob(icmd)%fdc%yr(iyr)%max)
            ob(icmd)%fdc%p_md%min = amin1 (ob(icmd)%fdc%p_md%min, ob(icmd)%fdc%yr(iyr)%min)
          end do
          ob(icmd)%fdc%p_md%mean = sum / time%nbyr
          
          !calc probabilities
          next = ob(icmd)%fdc%mfe
          do iyr = 1, time%nbyr
            if (iyr > time%nbyr / 2) then
                iyr_ch = (time%nbyr + 1.1) / 2
                if (iyr <= iyr_ch) then
                  !if odd number of years, take the mid year
                  ob(icmd)%fdc%p_md%p(nprob) = ob(icmd)%fdc%yr(next)%p(nprob)
                  exit
                else
                  !if even number of years, take average of 2 mid years
                  ob(icmd)%fdc%p_md%p(nprob) = (ob(icmd)%fdc%yr(next)%p(nprob) + ob(icmd)%fdc%yr(iprv)%p(nprob)) / 2.
                  exit
                end if
            end if
            iprv = next
            next = ob(icmd)%fdc_lla(next)%next
          end do    !iyr
          
          end do    !nprob
          
          !output flows in m3/s
          ob(icmd)%fdc%p_md%mean = ob(icmd)%fdc%p_md%mean / 86400.
          ob(icmd)%fdc%p_md%p = ob(icmd)%fdc%p_md%p / 86400.
          ob(icmd)%fdc%p_md%max = ob(icmd)%fdc%p_md%max / 86400.
          ob(icmd)%fdc%p_md%min = ob(icmd)%fdc%p_md%min / 86400.
          
          !compute flashiness index
          if (ob(icmd)%flash_idx%sum_q > 1.e-6) then
            ob(icmd)%flash_idx%index = ob(icmd)%flash_idx%sum_q_q1 / ob(icmd)%flash_idx%sum_q
          end if
          
          write (6000,*) ob(icmd)%typ, ob(icmd)%props, ob(icmd)%area_ha, ob(icmd)%flash_idx%index,  &
            ob(icmd)%fdc%p_md%mean, ob(icmd)%fdc%p_md%max, ob(icmd)%fdc%p_md%p, ob(icmd)%fdc%p_md%min
          
          !convert to mm -- m3 * 1 / ha * ha/ 10,000 m3 * 1,000 mm/m = mm
          !normalize by dividing by mean flow in mm
          !ob(icmd)%fdc%p_md%mean = ob(icmd)%fdc%p_md%mean / ob(icmd)%area_ha / 10.
          !ob(icmd)%fdc%p_md%p = ob(icmd)%fdc%p_md%p / ob(icmd)%area_ha / ob(icmd)%fdc%p_md%mean / 10.
          !ob(icmd)%fdc%p_md%max = ob(icmd)%fdc%p_md%max / ob(icmd)%area_ha / ob(icmd)%fdc%p_md%mean / 10.
          !ob(icmd)%fdc%p_md%min = ob(icmd)%fdc%p_md%min / ob(icmd)%area_ha / ob(icmd)%fdc%p_md%mean / 10.
          !write (6000,*) ob(icmd)%typ, ob(icmd)%props, ob(icmd)%area_ha, ob(icmd)%fdc%p_md%mean,        &
          !    ob(icmd)%fdc%p_md%max, ob(icmd)%fdc%p_md%p, ob(icmd)%fdc%p_md%min
        end if
               
        !master duration curve from annual curves (median)
   
      return
      end subroutine flow_dur_curve
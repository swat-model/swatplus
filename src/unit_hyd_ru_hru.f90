      subroutine unit_hyd_ru_hru
  
      !!  compute unit hydrographs for all hru and ru

      use hru_module, only : tconc
      use ru_module, only : ru_tc
      use hydrograph_module
      use time_module
      
      implicit none
      
      integer :: iihru          !       |hru counter
      integer :: iiru           !       |routing unit counter
      integer :: iob            !       |object counter
      integer :: ihyd           !       |receiving hyd counter
      real :: tc                !       |time of concentration for incoming hru and ru

      if (time%step > 1) then
        do iihru = 1, sp_ob%hru
          iob = sp_ob1%hru + iihru - 1
          call unit_hyd (tconc(iihru), ob(iob)%uh)
        end do
        do iiru = 1, sp_ob%ru
          iob = sp_ob1%ru + iiru - 1
          call unit_hyd (ru_tc(iiru), ob(iob)%uh)
        end do
        !! compute uh for all incoming hru and ru with fraction < 1
        do iob = 1, sp_ob%objs
          do ihyd = 1, ob(iob)%rcv_tot
            if (ob(iob)%frac_in(ihyd) < .999) then
              if (ob(iob)%obtyp_in(ihyd) == "hru") then
                iihru = ob(iob)%obtypno_in(ihyd)
                tc = tconc(iihru)
              end if
              if (ob(iob)%obtyp_in(ihyd) == "ru") then
                iiru = ob(iob)%obtypno_in(ihyd)
                tc = ru_tc(iiru)
              end if
              !! assume tc = sqrt(da_km) --> delta tc = sqrt(small_area)/sqrt(entire_area) = sqrt(frac_area)
              tc = sqrt(ob(iob)%frac_in(ihyd)) * tc
              call unit_hyd (tc, ob(iob)%hin_uh(ihyd)%uh)
            end if
          end do
        end do
                
      end if
      
      return
      end subroutine unit_hyd_ru_hru
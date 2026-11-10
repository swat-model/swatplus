      subroutine wallo_pou_output (ipou)
    
      use time_module
      use water_allocation_module
      use hydrograph_module
      
      implicit none
      
      integer, intent (in) :: ipou      !place of use number
      integer :: ipod                   !point of diversion number
      integer :: ipor                   !point of return number
      
      !! sum daily POD and POR organic-mineral for monthly
      poum_met(ipou)%duty_tot%duty = poum_met(ipou)%duty_tot%duty + poud_met(ipou)%duty_tot%duty
      poum_met(ipou)%duty_tot%deliv = poum_met(ipou)%duty_tot%deliv + poud_met(ipou)%duty_tot%deliv
      do ipod = 1, pou(ipou)%pods
        poum_om(ipou)%pod(ipod) = poum_om(ipou)%pod(ipod) + poud_om(ipou)%pod(ipod)
        poum_met(ipou)%pod(ipod)%duty = poum_met(ipou)%pod(ipod)%duty +      &
            poud_met(ipou)%pod(ipod)%duty
        poum_met(ipou)%pod(ipod)%deliv = poum_met(ipou)%pod(ipod)%deliv +            &
            poud_met(ipou)%pod(ipod)%deliv
      end do 
      do ipor = 1, pou(ipou)%pors
        poum_om(ipou)%por(ipor) = poum_om(ipou)%por(ipor) + poud_om(ipou)%por(ipor)
      end do 
      
      !! daily print      
      if (pco%water_allo%d == "y") then
        write (3410,*) time%day, time%mo, time%day_mo, time%yrc, ipou,                  &
            pou(ipou)%name, poud_met(ipou)%duty_tot%duty, poud_met(ipou)%duty_tot%deliv
        do ipod = 1, pou(ipou)%pods
          write (3418,*) time%day, time%mo, time%day_mo, time%yrc, ipod, poud_met(ipou)%pod(ipod)%duty, &
              poud_met(ipou)%pod(ipod)%deliv, poud_om(ipou)%pod(ipod)
        end do
        do ipor = 1, pou(ipou)%pors
          write (3426,*) time%day, time%mo, time%day_mo, time%yrc, ipor, poud_om(ipou)%por(ipor)
        end do

        if (pco%csvout == "y") then
          write (3414,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, ipou,          &
            time%yrc, ipou, pou(ipou)%name, poud_met(ipou)%duty_tot%duty,              &
            poud_met(ipou)%duty_tot%deliv
          do ipod = 1, pou(ipou)%pods
            write (3422,'(*(G0.6,:","))')  ipod, poud_met(ipou)%pod(ipod)%duty,        &
              poud_met(ipou)%pod(ipod)%deliv, poud_om(ipou)%pod(ipod)
          end do
          do ipor = 1, pou(ipou)%pors
            write (3430,'(*(G0.6,:","))') ipor, poud_om(ipou)%por(ipor)
          end do
        end if
        
        !! zero daily POD and POR organic-mineral
        poud_met(ipou)%duty_tot%duty = 0.
        poud_met(ipou)%duty_tot%deliv = 0.
        poud_om(ipou)%pods = hz
        poud_om(ipou)%pors = hz
        do ipod = 1, pou(ipou)%pods
          poud_om(ipou)%pod(ipod) = hz
          poud_met(ipou)%pod(ipod)%duty = 0.
          poud_met(ipou)%pod(ipod)%deliv = 0.
        end do
        do ipor = 1, pou(ipou)%pors
          poud_om(ipou)%por(ipor) = hz
        end do 
      
      end if    !! daily print

      !! monthly print
      !! sum monthly POD and POR organic-mineral for yearly
      pouy_met(ipou)%duty_tot%duty = pouy_met(ipou)%duty_tot%duty + poum_met(ipou)%duty_tot%duty
      pouy_met(ipou)%duty_tot%deliv = pouy_met(ipou)%duty_tot%deliv + poum_met(ipou)%duty_tot%deliv
      do ipod = 1, pou(ipou)%pods
        pouy_om(ipou)%pod(ipod) = pouy_om(ipou)%pod(ipod) + poum_om(ipou)%pod(ipod)
        pouy_met(ipou)%pod(ipod)%duty = pouy_met(ipou)%pod(ipod)%duty +      &
            poum_met(ipou)%pod(ipod)%duty
        pouy_met(ipou)%pod(ipod)%deliv = pouy_met(ipou)%pod(ipod)%deliv +            &
            poum_met(ipou)%pod(ipod)%deliv
      end do 
      do ipor = 1, pou(ipou)%pors
        pouy_om(ipou)%por(ipor) = pouy_om(ipou)%por(ipor) + poum_om(ipou)%por(ipor)
      end do 
      
      if (time%end_mo == 1) then
      if (pco%water_allo%m == "y") then
        write (3411,*)time%day, time%mo, time%day_mo, time%yrc, ipou,          &
            pou(ipou)%name, poum_met(ipou)%duty_tot%duty, poum_met(ipou)%duty_tot%deliv
        do ipod = 1, pou(ipou)%pods
          write (3419,*) time%day, time%mo, time%day_mo, time%yrc, ipod, poum_met(ipou)%pod(ipod)%duty, &
              poum_met(ipou)%pod(ipod)%deliv, poum_om(ipou)%pod(ipod)
        end do
        do ipor = 1, pou(ipou)%pors
          write (3427,*) time%day, time%mo, time%day_mo, time%yrc, ipor, poum_om(ipou)%por(ipor)
        end do

        if (pco%csvout == "y") then
          write (3415,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, ipou,          &
            time%yrc, ipou, pou(ipou)%name, poum_met(ipou)%duty_tot%duty,              &
            poum_met(ipou)%duty_tot%deliv
          do ipod = 1, pou(ipou)%pods
            write (3423,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ipod, poum_met(ipou)%pod(ipod)%duty, &
              poum_met(ipou)%pod(ipod)%deliv, poum_om(ipou)%pod(ipod)
          end do
          do ipor = 1, pou(ipou)%pors
            write (3431,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ipor, poum_om(ipou)%por(ipor)
          end do
        end if
      end if
        
        !! zero monthly POD and POR organic-mineral
        poum_met(ipou)%duty_tot%duty = 0.
        poum_met(ipou)%duty_tot%deliv = 0.
        poum_om(ipou)%pods = hz
        poum_om(ipou)%pors = hz
        do ipod = 1, pou(ipou)%pods
          poum_om(ipou)%pod(ipod) = hz
          poum_met(ipou)%pod(ipod)%duty = 0.
          poum_met(ipou)%pod(ipod)%deliv = 0.
        end do 
        do ipor = 1, pou(ipou)%pors
          poum_om(ipou)%por(ipor) = hz
        end do 
      
      end if     !! monthly print

      !! yearly print
      if (time%end_yr == 1) then
          
      poua_met(ipou)%duty_tot%duty = poua_met(ipou)%duty_tot%duty + pouy_met(ipou)%duty_tot%duty
      poua_met(ipou)%duty_tot%deliv = poua_met(ipou)%duty_tot%deliv + pouy_met(ipou)%duty_tot%deliv
      !! sum yearly POD and POR organic-mineral for average annual
      do ipod = 1, pou(ipou)%pods
        poua_om(ipou)%pod(ipod) = poua_om(ipou)%pod(ipod) + pouy_om(ipou)%pod(ipod)
        poua_met(ipou)%pod(ipod)%duty = poua_met(ipou)%pod(ipod)%duty +      &
            pouy_met(ipou)%pod(ipod)%duty
        poua_met(ipou)%pod(ipod)%deliv = poua_met(ipou)%pod(ipod)%deliv +            &
            pouy_met(ipou)%pod(ipod)%deliv
      end do 
      do ipor = 1, pou(ipou)%pors
        poua_om(ipou)%por(ipor) = poua_om(ipou)%por(ipor) + pouy_om(ipou)%por(ipor)
      end do 
      
      if (pco%water_allo%y == "y") then
        write (3412,*) time%day, time%mo, time%day_mo, time%yrc, ipou,         &
            pou(ipou)%name, pouy_met(ipou)%duty_tot%duty, pouy_met(ipou)%duty_tot%deliv
        do ipod = 1, pou(ipou)%pods
          write (3420,*) time%day, time%mo, time%day_mo, time%yrc, ipod, pouy_met(ipou)%pod(ipod)%duty, &
              pouy_met(ipou)%pod(ipod)%deliv, pouy_om(ipou)%pod(ipod)
        end do
        do ipor = 1, pou(ipou)%pors
          write (3428,*) time%day, time%mo, time%day_mo, time%yrc, ipor, pouy_om(ipou)%por(ipor)
        end do

        if (pco%csvout == "y") then
          write (3416,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, ipou,  &
            time%yrc, ipou, pou(ipou)%name, pouy_met(ipou)%duty_tot%duty,              &
            pouy_met(ipou)%duty_tot%deliv
          do ipod = 1, pou(ipou)%pods
            write (3424,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ipod, pouy_met(ipou)%pod(ipod)%duty, &
              pouy_met(ipou)%pod(ipod)%deliv, pouy_om(ipou)%pod(ipod)
          end do
          do ipor = 1, pou(ipou)%pors
            write (3432,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ipor, pouy_om(ipou)%por(ipor)
          end do
        end if
        
        !! zero yearly POD and POR organic-mineral
        pouy_met(ipou)%duty_tot%duty = 0.
        pouy_met(ipou)%duty_tot%deliv = 0.
        pouy_om(ipou)%pods = hz
        pouy_om(ipou)%pors = hz
        do ipod = 1, pou(ipou)%pods
          pouy_om(ipou)%pod(ipod) = hz
          pouy_met(ipou)%pod(ipod)%duty = 0.
          poum_met(ipou)%pod(ipod)%deliv = 0.
        end do 
        do ipor = 1, pou(ipou)%pors
          pouy_om(ipou)%por(ipor) = hz
        end do 
      end if
      end if     !! yearly print

      !! average annual print
      if (time%end_sim == 1 .and. time%yrs_prt > 0) then
          
      !! average annual POD and POR organic-mineral
      poua_met(ipou)%duty_tot%duty = poua_met(ipou)%duty_tot%duty / real(time%yrs_prt)
      poua_met(ipou)%duty_tot%deliv = poua_met(ipou)%duty_tot%deliv / real(time%yrs_prt)
      do ipod = 1, pou(ipou)%pods
        poua_om(ipou)%pod(ipod) = poua_om(ipou)%pod(ipod) / real(time%yrs_prt)
        poua_met(ipou)%pod(ipod)%duty = poua_met(ipou)%pod(ipod)%duty / real(time%yrs_prt)
        poua_met(ipou)%pod(ipod)%deliv = poua_met(ipou)%pod(ipod)%deliv / real(time%yrs_prt)
      end do 
      do ipor = 1, pou(ipou)%pors
        poua_om(ipou)%por(ipor) = poua_om(ipou)%por(ipor) / real(time%yrs_prt)
      end do 
      
      if (pco%water_allo%a == "y") then
        write (3413,*) time%day, time%mo, time%day_mo, time%yrc, ipou,         &
            pou(ipou)%name, poua_met(ipou)%duty_tot%duty, poua_met(ipou)%duty_tot%deliv
        do ipod = 1, pou(ipou)%pods
          write (3421,*) time%day, time%mo, time%day_mo, time%yrc, ipod, poua_met(ipou)%pod(ipod)%duty, &
              poua_met(ipou)%pod(ipod)%deliv, poua_om(ipou)%pod(ipod)
        end do
        do ipor = 1, pou(ipou)%pors
          write (3429,*) time%day, time%mo, time%day_mo, time%yrc, ipor, poua_om(ipou)%por(ipor)
        end do

        if (pco%csvout == "y") then
          write (3417,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, ipou,  &
            time%yrc, ipou, pou(ipou)%name, poua_met(ipou)%duty_tot%duty,              &
            poua_met(ipou)%duty_tot%deliv
          do ipod = 1, pou(ipou)%pods
            write (3425,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ipod, poua_met(ipou)%pod(ipod)%duty, &
              poua_met(ipou)%pod(ipod)%deliv, poua_om(ipou)%pod(ipod)
          end do
          do ipor = 1, pou(ipou)%pors
            write (3433,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ipor, poua_om(ipou)%por(ipor)
          end do
        end if
      end if
      end if    !! average annual print
      
      return

      end subroutine wallo_pou_output
      subroutine hru_urb_bmp
!!    ~ ~ ~ PURPOSE ~ ~ ~

    use hru_module, only : hru, ihru, sedyld, sedorgn, sedorgp, sedminpa, sedminps, sed_con, soln_con, solp_con,  &
       surqno3, latno3, surqsolp, qdr, orgn_con, orgp_con 
    
    implicit none
    
    integer :: j          !none          |HRU number 
    integer :: xx         !              |
    real :: sedppm        !              |
    real :: solnppm       !              |
    real :: solpppm       !              | 
    real :: sednppm       !              |
    real :: sedpppm       !              |

	j = 0
	j = ihru

!! convert to ppm -> (kg/ha)*100./mm = ppm
      if (qdr(j) > 0.1) then
	  xx = 100. / qdr(j)
        sedppm = 1000. * xx * sedyld(j) / hru(j)%area_ha
        solnppm = xx * (surqno3(j) + latno3(j))
        solpppm = xx * surqsolp(j)
        sednppm = xx * sedorgn(j)
        sedpppm = xx * (sedorgp(j) + sedminpa(j) + sedminps(j))
      
        if (sedppm > sed_con (j)) then
          sedyld(j) = sed_con(j) * hru(j)%area_ha / xx / 1000.
        endif

        if (solnppm > soln_con(j)) then
	    surqno3(j) = soln_con(j) / xx
          latno3(j) = soln_con(j) / xx
        endif

        if (solpppm > solp_con(j)) then
          surqsolp(j) = solp_con(j) / xx
        endif

        if (sednppm > orgn_con(j)) then
          sedorgn(j) = orgn_con(j) / xx
        endif

        if (sedpppm > orgp_con(j)) then 
	    sedorgn(j)= orgp_con(j) / xx
          sedminpa(j)= orgp_con(j) / xx
	    sedminps(j)= orgp_con(j) / xx
        endif

	endif

      return
      end subroutine hru_urb_bmp
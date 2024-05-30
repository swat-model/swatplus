      subroutine res_sediment

      use reservoir_data_module
      use reservoir_module
      use conditional_module
      use climate_module
      use time_module
      use hydrograph_module
      use water_body_module
      
      implicit none

      real :: trapres                       !              |
      real :: velofl                        !              |  
      real :: sed_ppm, sil_ppm, cla_ppm 

      if (wbody%flo < 1.e-6) then
        ! reservoir is empty
        wbody = hz
      else

        !! compute new sediment concentration in reservoir
	    if (ht1%sed < 1.e-6) ht1%sed = 0.0      
        !! velsetl = 1.35 for clay particle m/d
	    if (wbody_wb%area_ha > 1.e-6) then
          velofl = (ht2%flo / wbody_wb%area_ha) / 10000.  ! m3/d / ha * 10000. = m/d
          if (velofl > 1.e-6) then
	        trapres = wbody_prm%sed%velsetlr / velofl
          else
            trapres = 1.
          end if
	      if (trapres > 1.) trapres = 1.
	    else
	      trapres = 1.
        end if
        wbody%sed = wbody%sed - (ht1%sed * trapres)
        wbody%sil = wbody%sil - (ht1%sil * trapres)
        wbody%cla = wbody%cla - (ht1%cla * trapres)

        !! compute concentrations
	    if (wbody%flo > 0.) then
          sed_ppm = 1000000. * wbody%sed / wbody%flo
          sed_ppm = Max(1.e-6, sed_ppm)
          sil_ppm = 1000000. * wbody%sil / wbody%flo
          sil_ppm = Max(1.e-6, sil_ppm)
          cla_ppm = 1000000. * wbody%cla / wbody%flo
          cla_ppm = Max(1.e-6, cla_ppm)
	    else
          sed_ppm = 1.e-6
          sil_ppm = 1.e-6
          cla_ppm = 1.e-6
	    endif
        
        !! compute change in sediment concentration due to settling 
        if (sed_ppm > wbody_prm%sed%nsed) then
          sed_ppm = (sed_ppm - wbody_prm%sed%nsed) * wbody_prm%sed%sed_stlr + wbody_prm%sed%nsed
          wbody%sed = sed_ppm * wbody%flo / 1000000.      ! ppm -> t
          
          sil_ppm = (sil_ppm - wbody_prm%sed%nsed) * wbody_prm%sed%sed_stlr + wbody_prm%sed%nsed
          wbody%sil = sil_ppm * wbody%flo / 1000000.      ! ppm -> t
          
          cla_ppm = (cla_ppm - wbody_prm%sed%nsed) * wbody_prm%sed%sed_stlr + wbody_prm%sed%nsed
          wbody%cla = cla_ppm * wbody%flo / 1000000.      ! ppm -> t

          !! assume all sand aggregates and gravel settles
          wbody%san = 0.
          wbody%sag = 0.
          wbody%lag = 0.
          wbody%grv = 0.
        end if

        !! compute sediment leaving reservoir - ppm -> t
        ht2%sed = sed_ppm * ht2%flo / 1000000.
        wbody%sed = wbody%sed - ht2%sed
        ht2%sil = sil_ppm * ht2%flo / 1000000.
        wbody%sil = wbody%sil - ht2%sil
        ht2%cla = cla_ppm * ht2%flo / 1000000.
        wbody%cla = wbody%cla - ht2%cla

      end if

      return
      end subroutine res_sediment
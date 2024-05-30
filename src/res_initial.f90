      subroutine res_initial
      
      use reservoir_module
      use maximum_data_module
      use reservoir_data_module
      use hydrograph_module
      use constituent_mass_module
      use pesticide_data_module
      use water_body_module
      use res_salt_module
      use res_cs_module
      
      implicit none
      
      integer :: ires        !none          |counter
      integer :: lnvol       !              |
      real :: resdif         !              |
      integer :: i           !none          |counter
      integer :: idat        !none          |counter
      integer :: icon        !none          |
      integer :: init        !              | 
      integer :: ipest       !none          |counter
      integer :: ipath       !              |
      integer :: isalt       !              |counter for salt ions !rtb salt
      integer :: ipest_db    !none      |counter

      do ires = 1, sp_ob%res
        !! set initial volumes for res and hru types and convert units
        res_ob(ires)%ob  = sp_ob1%res + ires - 1
        res_ob(ires)%evol = res_hyd(ires)%evol * 10000.       !! ha-m => m**3
        res_ob(ires)%pvol = res_hyd(ires)%pvol * 10000.       !! ha-m => m**3
        res_ob(ires)%esa = res_hyd(ires)%esa
        res_ob(ires)%psa = res_hyd(ires)%psa
        !! set initial weir height to principal depth - m
        res_ob(ires)%weir_hgt = res_ob(ires)%pvol / (res_ob(ires)%psa * 10000.)
        
        !! use br1 as lag - then compute actual br1 (no option to input actual br1)
        res_ob(ires)%lag_up = res_hyd(ires)%br1
        res_ob(ires)%lag_down = res_hyd(ires)%br2
        
        !! calculate shape parameters for surface area equation
        resdif = res_hyd(ires)%evol - res_hyd(ires)%pvol
        if ((res_hyd(ires)%esa - res_hyd(ires)%psa) > 0. .and. resdif > 0.) then
          lnvol = Log10(res_ob(ires)%evol) - Log10(res_ob(ires)%pvol)
          if (lnvol > 1.e-4) then
            res_ob(ires)%br2 = (Log10(res_ob(ires)%esa) - Log10(res_ob(ires)%psa)) / lnvol
          else  
            res_ob(ires)%br2 = (Log10(res_ob(ires)%esa) - Log10(res_ob(ires)%psa)) / 0.001
          end if
          if (res_ob(ires)%br2 > 0.9) then
            res_ob(ires)%br2 = 0.9
            res_ob(ires)%br1 = (res_ob(ires)%psa / res_ob(ires)%pvol) ** 0.9
          else
            res_ob(ires)%br1 = (res_ob(ires)%esa / res_ob(ires)%evol) ** res_ob(ires)%br2
          end if  
        else
          res_ob(ires)%br2 = 0.9
          if (res_ob(ires)%pvol > 1.e-6) then
            res_ob(ires)%br1 = (res_ob(ires)%psa / res_ob(ires)%pvol) ** 0.9
          else
            res_ob(ires)%br1 = .1
          end if
        end if
        
      end do
      
      do ires = 1, sp_ob%res
        !! only initialize volume and constituents if operational at start of simulation
        !! if not, assume zero volume when dam is built
        if (time%yrc > res_hyd(ires)%iyres .or. (time%mo >= res_hyd(ires)%mores   &
                                   .and. time%yrc == res_hyd(ires)%iyres)) then
          idat = res_ob(ires)%props
          i = res_dat(idat)%init
        
          !! initialize org-min in reservoir
          init = res_init(i)%org_min
          res(ires) = om_init_water(init)
          call res_convert_mass (res(ires), res_ob(ires)%pvol)
        
          !! set initial reservoir org-min to reset for soft calibration
          res_om_init(ires) = res(ires)

          !! initialize pesticides in reservoir water and benthic from input data
          init = res_init(i)%pest
          do ipest = 1, cs_db%num_pests
            ipest_db = cs_db%pest_num(ipest)
            res_water(ires)%pest(ipest) = pest_water_ini(init)%water(ipest)
            res_benthic(ires)%pest(ipest) = pest_water_ini(init)%benthic(ipest)
            !! calculate mixing velocity using molecular weight and porosity
            res_ob(ires)%aq_mix(ipest) = pestdb(ipest_db)%mol_wt * (1. - res_sed(ires)%bd / 2.65)
          end do
                  
          !! initialize pathogens in reservoir water and benthic from input data
          init = res_init(i)%path
          do ipath = 1, cs_db%num_paths
            res_water(ires)%path(ipath) = path_water_ini(init)%water(ipath)
            res_benthic(ires)%path(ipath) = path_water_ini(init)%benthic(ipath)
          end do
                        
          !! calculate initial surface area       
          res_wat_d(ires)%area_ha = res_ob(ires)%br1 * res(ires)%flo ** res_ob(ires)%br2

          !! initialize salts in reservoir water, from database file (salt.res)
          !rtb salt
          if(cs_db%num_salts > 0) then
            idat = res_ob(ires)%props
            icon = res_dat(idat)%salt !database to use (in salt_res file)
            if(icon > 0) then
              do isalt = 1, cs_db%num_salts
                res_water(ires)%saltc(isalt) = res_salt_data(icon)%c_init(isalt) !concentration (g/m3)
                res_water(ires)%salt(isalt) = res_water(ires)%saltc(isalt) * res(ires)%flo / 1000. !g/m3 * m3 / 1000. = kg
              enddo
						else
              do isalt = 1, cs_db%num_salts
                res_water(ires)%saltc(isalt) = 0. !concentration (g/m3)
                res_water(ires)%salt(isalt) = 0. !kg
              enddo 
            endif
          endif
          
          !! initialize constituents in reservoir water, from database file (cs_res)
          !rtb cs
          if(cs_db%num_cs > 0) then
            idat = res_ob(ires)%props
            icon = res_dat(idat)%cs !database to use (in cs.res file)
            if(icon > 0) then
              !seo4
              res_water(ires)%csc(1) = res_cs_data(icon)%c_seo4 !concentration (g/m3)
              res_water(ires)%cs(1) = res_water(ires)%csc(1) * res(ires)%flo / 1000. !g/m3 * m3 / 1000. = kg
              !seo3
              res_water(ires)%csc(2) = res_cs_data(icon)%c_seo3 !concentration (g/m3)
              res_water(ires)%cs(2) = res_water(ires)%csc(2) * res(ires)%flo / 1000. !g/m3 * m3 / 1000. = kg
              !boron
              res_water(ires)%csc(3) = res_cs_data(icon)%c_born !concentration (g/m3)
              res_water(ires)%cs(3) = res_water(ires)%csc(3) * res(ires)%flo / 1000. !g/m3 * m3 / 1000. = kg
            else
              !seo4
              res_water(ires)%csc(1) = 0.
              res_water(ires)%cs(1) = 0.
              !seo3
              res_water(ires)%csc(2) = 0.
              res_water(ires)%cs(2) = 0.
              !boron
              res_water(ires)%csc(3) = 0.
              res_water(ires)%cs(3) = 0.
            endif
          endif
          
        endif
        
      end do
      close(105)

      return
      end subroutine res_initial
      subroutine pest_pl_up

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the root uptake of pesticide from the soil
!!    into the plant, following the transpiration stream.

!!    ~ ~ ~ FLAGS ~ ~ ~
!!    pest_uptake_lamshoeft (module param) : .true.  = use exact Lamshoeft PUF log-ratio formulation
!!                                           .false. = use original linear TSCF * C * V (default)
!!    pest_uptake_skip_ly1  (module param) : .true.  = skip the 10mm surface layer in root uptake
!!                                           .false. = include all layers including layer 1 (default)
!!    pestdb()%harvest_sink (per-pesticide) : 1 = plant is a 100% sink; uptaken pesticide is
!!                                                permanently lost from the system (SWAT-equiv, default)
!!                                            0 = pesticide in plant is returned to soil on harvest/kill
!!    pestdb()%resistance_n (per-pesticide) : 0.0 = use actual soil water for concentration (default)
!!                                            1.0 = use field capacity for concentration
!!                                            0-1 = linear blend: st_eff = st + n * max(0, fc - st)

      use pesticide_data_module
      use output_ls_pesticide_module
      use hru_module, only :  ihru
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use plant_module
      
      implicit none       
      
      integer :: j = 0        !none          |HRU number
      integer :: k = 0        !none          |pesticide number
      integer :: ly = 0       !none          |soil layer number
      integer :: ipl = 0      !none          |plant number
      integer :: ipest_db = 0 !none          |pesticide number from pest.dat
      real :: pest_up = 0.    !kg/ha         |pesticide taken up from soil layer
      real :: kd = 0.         !(mg/kg)/(mg/L)|soil-water partition coefficient
      real :: sorb_cap = 0.   !mm            |sorption capacity = kd * bd * thick
      real :: w_before = 0.   !mm            |soil water + sorption before extraction
      real :: w_after = 0.    !mm            |soil water + sorption after extraction
      real :: tscf = 0.       !none          |plant uptake factor from pesticide database
      real :: st_eff = 0.     !mm            |effective soil water (resistance-blended)

      j = ihru

      if (cs_db%num_pests == 0) return

      do k = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(k)
        hpestb_d(j)%pest(k)%pl_uptake = 0.
        ! calculate root uptake of pesticide from soil
        do ipl = 1, pcom(j)%npl
          ! skip plants that are not growing (gro /= 'y') to prevent phantom pesticide uptake
          if (pcom(j)%plcur(ipl)%gro /= 'y') cycle
          if (ipest_db > 0) then
            tscf = pestdb(ipest_db)%pl_uptake
            do ly = 1, soil(j)%nly
              !! skip the 10mm surface layer if flag is set (layer 1 is dominated by
              !! evaporation, not transpiration, and has artificially high concentrations)
              if (pest_uptake_skip_ly1 .and. ly == 1) cycle
              !! guard: only compute uptake if there is pesticide and water in the layer
              if (cs_soil(j)%ly(ly)%pest(k) >= 1.e-9 .and. soil(j)%phys(ly)%st > 1.e-6  &
                  .and. pcom(j)%plcur(ipl)%uptake(ly) > 1.e-6) then

                !! compute sorption capacity of the layer (kd * bd * thick)
                !! kd = Koc * foc;  units: (mL/g)*(g/g) = mL/g = L/kg = m3/Mg
                !! sorb_cap = kd [m3/Mg] * bd [Mg/m3] * thick [mm] = [mm]
                kd = pestdb(ipest_db)%koc * soil1(ihru)%cbn(ly) / 100.
                sorb_cap = kd * soil(j)%phys(ly)%bd * soil(j)%phys(ly)%thick

                !! resistance blending: st_eff = st + n * max(0, fc - st)
                !! n=0 -> actual st (default); n=1 -> field capacity
                st_eff = soil(j)%phys(ly)%st                                   &
                         + pestdb(ipest_db)%resistance_n                        &
                         * max(0., soil(j)%phys(ly)%fc - soil(j)%phys(ly)%st)

                if (pest_uptake_lamshoeft) then
                  !! ---------------------------------------------------------
                  !! Lamshoeft exact PUF formulation (log-ratio)
                  !! m_end = m_start * ((st_after + S) / (st_before + S))^TSCF
                  !! pest_up = m_start * [1 - ((st_after+S)/(st_before+S))^TSCF]
                  !! ---------------------------------------------------------
                  !! Note: pl_waterup has already reduced st, so current st = st_after.
                  !! st_before = st + uptake(ly)
                  w_after  = st_eff + sorb_cap
                  w_before = w_after + pcom(j)%plcur(ipl)%uptake(ly)
                  pest_up = cs_soil(j)%ly(ly)%pest(k)                                 &
                            * (1. - (w_after / w_before) ** tscf)
                else
                  !! ---------------------------------------------------------
                  !! Original linear formulation with sorption correction
                  !! C_dissolved = pest_mass / (st_before + sorb_cap)
                  !! pest_up = TSCF * uptake * C_dissolved
                  !! ---------------------------------------------------------
                  !! st_before = current st_eff + uptake (add back the extracted water)
                  w_before = st_eff + pcom(j)%plcur(ipl)%uptake(ly) + sorb_cap
                  pest_up = tscf * pcom(j)%plcur(ipl)%uptake(ly)                      &
                            / w_before                                                 &
                            * cs_soil(j)%ly(ly)%pest(k)
                end if

                !! cap: cannot remove more than available
                if (pest_up > cs_soil(j)%ly(ly)%pest(k)) pest_up = cs_soil(j)%ly(ly)%pest(k)
                if (pest_up < 0.) pest_up = 0.

                !! remove from soil, add to plant internal pool and daily accumulator
                cs_soil(j)%ly(ly)%pest(k) = cs_soil(j)%ly(ly)%pest(k) - pest_up
                if (pestdb(ipest_db)%harvest_sink < 0.5) then
                  !! pest enters the plant pool and will be returned to soil on harvest/kill
                  cs_pl(j)%pl_in(ipl)%pest(k) = cs_pl(j)%pl_in(ipl)%pest(k) + pest_up
                else
                  !! sink behavior by design: do not store in pl_in; treat uptake as
                  !! terminal at uptake step and track it through pl_uptake output.
                end if
                !! always track the daily uptake for output purposes
                hpestb_d(j)%pest(k)%pl_uptake = hpestb_d(j)%pest(k)%pl_uptake + pest_up
              end if
            end do
          end if 
        end do
      end do

      return
      end subroutine pest_pl_up
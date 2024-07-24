      subroutine pest_lch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates pesticides leached through each layer,
!!    pesticide transported with lateral subsurface flow, and pesticide
!!    transported with surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru         |none          |HRU number
!!    surfq(:)     |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lat_pst(:)   |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    zdb(:,:)     |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use pesticide_data_module
      use basin_module
      use hru_module, only : hru, surfq, qtile, ihru
      use soil_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use organic_mineral_mass_module
      
      implicit none        
      
      integer :: j         !none          |HRU number
      integer :: k         !none          |counter
      integer :: ipest_db  !none          |pesticide number from pest.dat
      integer :: ly        !none          |counter (soil layers)
      real :: kd                !(mg/kg)/(mg/L) |koc * carbon
      real :: yy           !              |
      real :: vf           !              |
      real :: xx           !kg/ha         |amount of pesticide removed from soil layer
      real :: zdb1         !              |
      real :: co           !kg/mm-ha      |concentration of pesticide in water
      real :: csurf        !kg/mm-ha      |concentration of pesticide in surq and latq            | 

      j = ihru

      if (cs_db%num_pests /= 0) then
        do k = 1, cs_db%num_pests
          hpestb_d(j)%pest(k)%perc = 0.
          hpestb_d(j)%pest(k)%surq = 0.
          hpestb_d(j)%pest(k)%latq = 0.
        end do

        do ly = 1, soil(j)%nly

          do k = 1, cs_db%num_pests
            ipest_db = cs_db%pest_num(k)

            kd = pestdb(ipest_db)%koc * soil1(ihru)%cbn(ly) / 100.
            zdb1 = soil(j)%phys(ly)%ul + kd * soil(j)%phys(1)%bd * soil(j)%phys(1)%thick
            !! units: mm + (m^3/ton)*(ton/m^3)*mm = mm

            !! compute volume of flow through the layer
            vf = soil(j)%ly(ly)%prk + soil(j)%ly(ly)%flat
            if (ly == 1) vf = vf + surfq(j)
            if (ly == hru(j)%lumv%ldrain) vf = vf + qtile
              
            !! compute concentration in the flow
            if (cs_soil(j)%ly(ly)%pest(k) >= 0.0001 .and. vf > 0.) then
              xx =  cs_soil(j)%ly(ly)%pest(k) * (1. - Exp(-vf / (zdb1 + 1.e-6)))
              co = xx / vf
              co = Min(pestdb(ipest_db)%solub / 100., co)
              csurf = bsn_prm%percop * co

              !! calculate pesticide lost in surface runoff
              if (surfq(j) > 0. .and. ly == 1) then
                yy = csurf * surfq(j)
                if (yy >  cs_soil(j)%ly(ly)%pest(k)) yy = cs_soil(j)%ly(ly)%pest(k)
                cs_soil(j)%ly(ly)%pest(k) =  cs_soil(j)%ly(ly)%pest(k) - yy
                hpestb_d(j)%pest(k)%surq = yy 
              endif

              !! calculate pesticide lost in tile flow
              if (qtile > 0. .and. ly == hru(j)%lumv%ldrain) then
                yy = co * qtile
                if (yy > cs_soil(j)%ly(ly)%pest(k)) yy = cs_soil(j)%ly(ly)%pest(k)
                cs_soil(j)%ly(ly)%pest(k) = cs_soil(j)%ly(ly)%pest(k) - yy
                hpestb_d(j)%pest(k)%tileq = yy
              end if

              !! calculate pesticide lost in lateral flow - use surface runoff conc for first layer
              if (ly == 1) then
                yy = csurf * soil(j)%ly(ly)%flat
              else
                yy = co * soil(j)%ly(ly)%flat
              end if
              if (yy > cs_soil(j)%ly(ly)%pest(k)) yy = cs_soil(j)%ly(ly)%pest(k)
              cs_soil(j)%ly(ly)%pest(k) = cs_soil(j)%ly(ly)%pest(k) - yy
              hpestb_d(j)%pest(k)%latq = hpestb_d(j)%pest(k)%latq + yy
                
              !! calculate pesticide leaching
              xx = co * soil(j)%ly(ly)%prk
              if (xx > cs_soil(j)%ly(ly)%pest(k)) xx = cs_soil(j)%ly(ly)%pest(k)
              cs_soil(j)%ly(ly)%pest(k) = cs_soil(j)%ly(ly)%pest(k) - xx
              if (ly < soil(j)%nly) then
                cs_soil(j)%ly(ly+1)%pest(k) = cs_soil(j)%ly(ly+1)%pest(k) + xx
              else
                hpestb_d(j)%pest(k)%perc = xx
              end if
                
            end if  ! flow volume > 0. 
              
          end do    ! number of pests
        end do      ! number of layers

      end if        ! number of pests > 0

      return
      end subroutine pest_lch
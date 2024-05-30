      subroutine sd_channel_surf_link
                 
      use hydrograph_module
      use sd_channel_module
      use ru_module
      use hru_module, only : hru, ihru 
      use topography_data_module
      
      implicit none 
      
      integer :: ics                  !none          |counter
      character (len=3) :: iobtyp     !none          |object type
      integer :: ii                   !none          |counter 
      integer :: iihru                !none          |hru counter 
      integer :: ihru_tot             !none          |total number of hru in the flood plain   

      do ics = 1, sp_ob%chandeg
        if (sd_ch(ics)%fp%obj_tot > 0) then
        !! determine number of hru's
        ihru_tot = 0
        do ii = 1, sd_ch(ics)%fp%obj_tot
          iobtyp = sd_ch(ics)%fp%obtyp(ii)     !object type
          select case (iobtyp)
          case ("hru")   !hru
            ihru_tot = ihru_tot + 1
          case ("ru")   !flood plain routing unit
            iru = sd_ch(ics)%fp%obtypno(ii)
            ihru_tot = ihru_tot + ru_def(iru)%num_tot
          end select
        end do
        
        allocate (sd_ch(ics)%fp%hru(ihru_tot))
        allocate (sd_ch(ics)%fp%hru_fr(ihru_tot))
   
        !! calculate total flood plain area and set hru numbers
        ihru_tot = 0
        sd_ch(ics)%fp%ha = 0.
        do ii = 1, sd_ch(ics)%fp%obj_tot
          iobtyp = sd_ch(ics)%fp%obtyp(ii)     !object type
          select case (iobtyp)
          case ("hru")   !hru
            ihru_tot = ihru_tot + 1
            ihru = sd_ch(ics)%fp%obtypno(ii)
            sd_ch(ics)%fp%hru(ihru_tot) = ihru
            sd_ch(ics)%fp%ha = sd_ch(ics)%fp%ha + hru(ihru)%area_ha
            hru(ihru)%wet_fp = "y"
            
          case ("ru")   !flood plain routing unit
            iru = sd_ch(ics)%fp%obtypno(ii)

            !set flood plain link and landscape element (1==closest to river)
            do iihru = 1, ru_def(iru)%num_tot
              ihru_tot = ihru_tot + 1
              ihru = ru_def(iru)%num(iihru)
              sd_ch(ics)%fp%hru(ihru_tot) = ihru
              sd_ch(ics)%fp%ha = sd_ch(ics)%fp%ha + hru(ihru)%area_ha
              hru(ihru)%wet_fp = "y"
            end do
      
          end select
        end do      ! ii = 1, sd_ch(ics)%fp%obj_tot
   
        !set hru flood plain area fractions
        sd_ch(ics)%fp%hru_tot = ihru_tot
        do ihru = 1, sd_ch(ics)%fp%hru_tot
          iihru = sd_ch(ics)%fp%hru(ihru)
          sd_ch(ics)%fp%hru_fr(ihru) = hru(iihru)%area_ha / sd_ch(ics)%fp%ha
        end do
            
      end if    ! sd_ch(ics)%fp%obj_tot > 0
      end do    ! ics = 1, sp_ob%chandeg
        
      return

      end subroutine sd_channel_surf_link
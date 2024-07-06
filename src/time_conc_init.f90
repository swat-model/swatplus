      subroutine time_conc_init 
    
      use ru_module
      use hru_module, only : brt, hru, ihru, t_ov, tconc
      use hydrograph_module, only : sp_ob, ru_def, ru_elem, sp_ob1, ob
      use topography_data_module
      use time_module
      use basin_module
      
      implicit none 
      
      integer :: ii                !none         |counter
      integer :: ielem             !none         |counter
      integer :: iob               !             | 
      integer :: ith               !             | 
      integer :: ifld              !             |
      real :: tov                  !             |
      real :: ch_slope             !             |
      real :: ch_n                 !             |
      real :: ch_l                 !             | 
      real :: t_ch                 !hr           |time for flow entering the farthest upstream 
                                   !             |channel to reach the subbasin outlet

     ! compute weighted Mannings n for each subbasin
      do iru = 1, sp_ob%ru
        ru_n(iru) = 0.
        do ii = 1, ru_def(iru)%num_tot
          ielem = ru_def(iru)%num(ii)
          if (ru_elem(ielem)%obtyp == "hru") then
            ihru = ru_elem(ielem)%obtypno 
            ru_n(iru) = ru_n(iru) + hru(ihru)%luse%ovn * hru(ihru)%km
          else
            ru_n(iru) = 0.1
          end if
        end do
      end do
            
      do iru = 1, sp_ob%ru
        iob = sp_ob1%ru + iru - 1
        ru(iru)%da_km2 = ob(iob)%area_ha / 100.
        ru_n(iru) = ru_n(iru) / ru(iru)%da_km2
        ith = ru(iru)%dbs%toposub_db
        !if (ith > 0 .and. ichan > 0) then                  
        ! compute tc for the subbasin
          tov = .0556 * (topo_db(ith)%slope_len * ru_n(iru)) ** .6 /     &
                                              (topo_db(ith)%slope + .001) ** .3
          ch_slope = .5 * (topo_db(ith)%slope + .001)
          ch_n = ru_n(iru)
          ch_l = ru(iru)%field%length / 1000.
          t_ch = .62 * ch_l * ch_n**.75 / (ru(iru)%da_km2**.125 * ch_slope**.375)
          ru_tc(iru) = tov + t_ch
        !end if                                             
      end do
      
      !!compute time of concentration (sum of overland and channel times)
      do ihru = 1, sp_ob%hru
        ith = hru(ihru)%dbs%topo
        ifld = hru(ihru)%dbs%field
        t_ov(ihru) = .0556 * (hru(ihru)%topo%slope_len *                    &
           hru(ihru)%luse%ovn) ** .6 / (hru(ihru)%topo%slope + .0001) ** .3
        ch_slope = .5 * topo_db(ith)%slope
        ch_n = hru(ihru)%luse%ovn
        !! assume length to width (l/w) ratio of 2 --> A=l*w - A=l*l/2 - l=sqrt(A/2)
        !! assume channel begins at 1/2 of distance
        ch_l = 0.5 * sqrt(hru(ihru)%area_ha / 2.)
        !ch_l = hru(ihru)%field%length / 1000.
        t_ch = .31 * ch_l * ch_n**.75 / (hru(ihru)%km**.125 * (ch_slope + .001)**.375)
        tconc(ihru) = t_ov(ihru) + t_ch
        !! compute fraction of surface runoff that is reaching the main channel
        if (time%step > 1) then
          brt(ihru) = 1.-Exp(-bsn_prm%surlag / (tconc(ihru) /               &
              (time%dtm / 60.)))	!! urban modeling by J.Jeong
        else
          brt(ihru) = 1. - Exp(-bsn_prm%surlag / tconc(ihru))
        endif
      end do
      
     return
     end subroutine time_conc_init
      subroutine calsoft_hyd_bfr_pet
      use hru_module, only : hru, hru_init
      use soil_module
      use plant_module
      use hydrograph_module
      use ru_module
      use aquifer_module
      use channel_module
      use hru_lte_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use conditional_module
      use reservoir_module
      use organic_mineral_mass_module
      use time_module
      implicit none
      integer :: isim = 0          !          |
      integer :: ireg = 0          !none      |counter
      integer :: ilum = 0          !none      |counter
      integer :: iihru = 0         !none      |counter
      integer :: ihru_s = 0        !none      |counter
      integer :: iter_ind = 0      !          |end of loop
      integer :: ietco = 0         !none      |counter
      real :: rmeas = 0.            !          |
      real :: denom = 0.            !          |
      real :: soft = 0.             !          |
      real :: diff = 0.             !          |
      real :: chg_val = 0.          !          |   
      real :: pred = 0.
      ! calibrate esco and pet for water yield
        iter_ind = 1
        ! first calibrate potential et
        do ietco = 1, 2     !iter_ind
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%precip_aa
            pred = lscal(ireg)%lum(ilum)%aa%wyr
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - pred) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6) then
            isim = 1
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                if (soft < pred) then
                  chg_val = 1. + abs((soft - pred) / soft)
                else
                  chg_val = 1. - abs((pred - soft) / pred)
                end if
                lscal(ireg)%lum(ilum)%prm_prev%petco = lscal(ireg)%lum(ilum)%prm%petco
                if (ietco == 1) then
                  lscal(ireg)%lum(ilum)%prm%petco = chg_val
                else
                  lscal(ireg)%lum(ilum)%prm%petco = lscal(ireg)%lum(ilum)%prm%petco * chg_val
                end if
                lscal(ireg)%lum(ilum)%prm_prev%petco = pred
                if (lscal(ireg)%lum(ilum)%prm%petco >= ls_prms(4)%pos) then
                  chg_val = ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm%petco = ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%petco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%petco <= ls_prms(4)%neg) then
                  chg_val = ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm%petco = ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%petco = 1.
                end if
            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c .or. lscal(ireg)%lum(ilum)%meas%name == "basin") then
                !set parms for pet adjustment
                hru(iihru)%hyd%pet_co = hru(iihru)%hyd%pet_co * chg_val
                hru(iihru)%hyd%pet_co = amin1 (hru(iihru)%hyd%pet_co, ls_prms(4)%up)
                hru(iihru)%hyd%pet_co = Max (hru(iihru)%hyd%pet_co, ls_prms(4)%lo)
                hru_init(iihru)%hyd%pet_co = hru(iihru)%hyd%pet_co
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        !! re-initialize all objects
        call re_initialize
        ! 1st cover adjustment 
        if (isim > 0) then
          cal_sim =  " first pet adj "
          cal_adj = chg_val
          call time_control
        end if
      end do    ! petco iterations
      return
      end subroutine calsoft_hyd_bfr_pet
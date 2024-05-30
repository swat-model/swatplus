      subroutine swr_substor
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine stores and lags lateral soil flow and nitrate

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bss(1,:)      |mm H2O       |amount of lateral flow lagged
!!    bss(2,:)      |kg N/ha      |amount of nitrate in lateral flow lagged

!!    bss(3,:)      |mm           |amount of tile flow lagged
!!    bss(4,:)      |kg N/ha      |amount of nitrate in tile flow lagged
!!    ihru          |none         |HRU number
!!    lat_pst(:)    |kg pst/ha    |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    lat_ttime(:)  |none         |Exponential of the lateral flow travel time
!!    latq(:)       |mm H2O       |amount of water in lateral flow in HRU for
!!                                |the day
!!    qtile(:)      |mm H2O       |amount of water in tile flow in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bss(1,:)      |mm H2O        |amount of lateral flow lagged
!!    bss(2,:)      |kg N/ha       |amount of nitrate in lateral flow lagged
!!    bss(3,:)      |mm            |amount of tile flow lagged
!!    bss(4,:)      |kg N/ha       |amount of nitrate in tile flow lagged
!!    lat_pst(:)    |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                 |for the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use pesticide_data_module
      use hru_module, only : hru, ihru, latq, latno3, qtile, bss, tileno3
      use constituent_mass_module
      use output_ls_pesticide_module
      
      implicit none      
      
      integer :: j           !none          |HRU number  

      j = ihru

      bss(1,j) = bss(1,j) + latq(j)
      bss(2,j) = bss(2,j) + latno3(j)
      bss(3,j) = bss(3,j) + qtile
      bss(4,j) = bss(4,j) + tileno3(j)
      if (bss(1,j) < 1.e-6) bss(1,j) = 0.0
      if (bss(2,j) < 1.e-6) bss(2,j) = 0.0
      if (bss(3,j) < 1.e-6) bss(3,j) = 0.0
      if (bss(4,j) < 1.e-6) bss(4,j) = 0.0

      latq(j) = bss(1,j) * hru(j)%hyd%lat_ttime
      latno3(j) = bss(2,j) * hru(j)%hyd%lat_ttime
      qtile = bss(3,j) * hru(j)%lumv%tile_ttime
      tileno3(j) = bss(4,j) * hru(j)%lumv%tile_ttime
      if (latq(j) < 1.e-6) latq(j) = 0.
      if (latno3(j) < 1.e-6) latno3(j) = 0.
      if (qtile < 1.e-6) qtile = 0.
      if (tileno3(j) < 1.e-6) tileno3(j) = 0.

      bss(1,j) = bss(1,j) - latq(j)
      bss(2,j) = bss(2,j) - latno3(j)
      bss(3,j) = bss(3,j) - qtile
      bss(4,j) = bss(4,j) - tileno3(j)

      return
      end subroutine swr_substor
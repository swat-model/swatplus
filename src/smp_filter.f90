      subroutine smp_filter
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the reduction of pollutants in surface runoff
!!    due to an edge of field filter or buffer strip

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    use basin_module
    use hru_module, only : hru, surfq, ihru, sagyld, silyld, clayld, lagyld, sedorgn, surqno3, sedminpa,  &
      sedminps, sedorgp, surqsolp, sedyld, sanyld
    use soil_module
    use constituent_mass_module
    use time_module
    use output_ls_pesticide_module
      
    implicit none

    integer :: i              !                |
    integer :: j              !none            |hru number
    integer :: k              !none            |counter
    real :: drain_vfs1        !ha	           |drainage area of vfs section 1
    real :: drain_vfs2        !ha	           |drainage area of vfs section 2
    real :: drain_vfs3        !	           |
    real :: area_vfs1         !ha	           |Area of vfs section 1
    real :: area_vfs2         !ha	           |Area of vfs section 2
    real :: vfs_depth1        !mm	           |Runoff Loading for vfs section 1
    real :: vfs_depth2        !mm	           |Runoff Loading for vfs section 2
    real :: vfs_sed1          !kg/m^2          |sediment loading for vfs section 1
    real :: vfs_sed2          !kg/m^2          |sediment loading for vfs section 2
    real :: surq_remove1      !%               |Surface runoff removal for for vfs section 1
    real :: surq_remove2      !%               |Surface runoff removal for for vfs section 2
    real :: surq_remove       !%               |Average surface runoff removal for for entire vfs
    real :: sed_remove1       !%               |sediment removal for for vfs section 1
    real :: sed_remove2       !%               |sediment removal for for vfs section 2
    real :: sed_remove        !%               |Average sediment removal for for entire vfs 
    real :: remove1           !%               |Generic removal for for vfs section 1 
                              !                |(recycled for constituants)
    real :: remove2           !%               |Generic removal for for vfs section 2 
                              !                |(recycled for constituants)
    real :: sedtrap           !                | 
    real :: xrem              !                ! 
    real :: vfs_ratio1        !                !
    real :: vfs_ratio2        !                !
    real :: orgn_remove       !%	           |Average organic N removal from surface 
                              !                |runoff for for entire vfs
    real :: surqno3_remove    !%	           |Average nitrate removal from surface 
                              !                |runoff for for entire vfs
    real :: partp_remove      !%	           |Average particulate P removal from surface
                              !                | runoff for for entire vfs
    real :: solP_remove       !%	           |Average soluble P removal from surface 
                              !                |runoff for for entire vfs	
    real :: remove21          !                |
   
    j = ihru

	if (i == 100) then 
	remove2 = 0.
	end if


    !! Filter only if there is some surface runoff
	if (surfq(j) > .0001) then

      !! vfs comnposed of two sections one with more concentrated flow than the other

      !! Calculate drainage area of vfs 1 2 3 in ha
	  drain_vfs1 = (1. - hru(j)%lumv%vfscon)* hru(j)%area_ha
	  drain_vfs2 = ((1. - hru(j)%lumv%vfsch) * hru(j)%lumv%vfscon)* hru(j)%area_ha
	  drain_vfs3 = hru(j)%lumv%vfscon * hru(j)%lumv%vfsch * hru(j)%area_ha

      !! Calculate area of vfs 1 and 2 in ha
	  area_vfs1 = hru(j)%area_ha * 0.9 / hru(j)%lumv%vfsratio
	  area_vfs2 = hru(j)%area_ha * 0.1 / hru(j)%lumv%vfsratio

      !!	Calculate drainage area to vfs area ratio (unitless)
	  vfs_ratio1 = drain_vfs1/area_vfs1
	  vfs_ratio2 = drain_vfs2/area_vfs2

      !! calculate runoff depth over buffer area in mm
	  vfs_depth1 = vfs_ratio1 * surfq(j)
	  vfs_depth2 = vfs_ratio2 * surfq(j)

      !! calculate sediment loading over buffer area in kg/m^2
	  vfs_sed1 = (sedyld(j) / hru(j)%area_ha * 1000. * drain_vfs1) / (area_vfs1 * 10000.)
	  vfs_sed2 = (sedyld(j) / hru(j)%area_ha * 1000. * drain_vfs2) / (area_vfs2 * 10000.)

      !! calculate Runoff Removal by vfs (used for nutrient removal estimation only) based on runoff depth and ksat
      !! Based on vfsmod simulations

      surq_remove1 = 75.8-10.8 * Log(vfs_depth1) + 25.9 * Log(soil(j)%phys(1)%k)
	  if (surq_remove1 > 100.) surq_remove1 = 100.
	  if (surq_remove1 < 0.) surq_remove1 = 0.

      surq_remove2 = 75.8-10.8 * Log(vfs_depth2) + 25.9 * Log(soil(j)%phys(1)%k)
	  if (surq_remove2 > 100.) surq_remove2 = 100.
	  if (surq_remove2 < 0.) surq_remove2 = 0.

	  surq_remove = (surq_remove1 * drain_vfs1 + surq_remove2 * drain_vfs2)/hru(j)%area_ha

      !! calculate sediment Removal - Based on measured data from literature

	  sed_remove1 = 79.0 - 1.04 * vfs_sed1 + 0.213 * surq_remove1 
	  if (sed_remove1 > 100.) sed_remove1 = 100.
	  if (sed_remove1 < 0.) sed_remove1 = 0.

	  sed_remove2 = 79.0 - 1.04 * vfs_sed2 + 0.213 * surq_remove1 
	  if (sed_remove2 > 100.) sed_remove2 = 100.
	  if (sed_remove2 < 0.) sed_remove2 = 0.

	  sed_remove = (sed_remove1 * drain_vfs1 + sed_remove2 * drain_vfs2) / hru(j)%area_ha	
	
	  sedyld(j) = sedyld(j) * (1. - sed_remove / 100.)
      sedyld(j) = Max(0., sedyld(j))

	  sedtrap = sedyld(j) * sed_remove / 100.
	  xrem = 0.

      if (sedtrap <= lagyld(j)) then
        lagyld(j) = lagyld(j) - sedtrap
      else
        xrem = sedtrap - lagyld(j)
        lagyld(j) = 0.
        if (xrem <= sanyld(j)) then
          sanyld(j) = sanyld(j) - xrem
        else
          xrem = xrem - sanyld(j)
          sanyld(j) = 0.
          if (xrem <= sagyld(j)) then
            sagyld(j) = sagyld(j) - xrem
          else
            xrem = xrem - sagyld(j)
            sagyld(j) = 0.
            if (xrem <= silyld(j)) then
              silyld(j) = silyld(j) - xrem
            else
              xrem = xrem - silyld(j)
              silyld(j) = 0.
              if (xrem <= clayld(j)) then
                clayld(j) = clayld(j) - xrem
              else
                xrem = xrem - clayld(j)
                clayld(j) = 0.
              end if
            end if
          end if
        end if
      end if
      sanyld(j) = Max(0., sanyld(j))
      silyld(j) = Max(0., silyld(j))
      clayld(j) = Max(0., clayld(j))
      sagyld(j) = Max(0., sagyld(j))
      lagyld(j) = Max(0., lagyld(j))

      !! Calculate Organic Nitrogen Removal
      !! Based on measured data from literature

	  remove1 = 0.036 * sed_remove1 ** 1.69
	  if (remove1 > 100.) remove1 = 100.
	  if (remove1 < 0.) remove1 = 0.

	  remove2 = 0.036 * sed_remove2 ** 1.69
	  if (remove2 > 100.) remove2 = 100.
	  if (remove2 < 0.) remove2 = 0.
	
	  orgn_remove = (remove1 * drain_vfs1 + remove2 * drain_vfs2)/hru(j)%area_ha
	  sedorgn(j) = sedorgn(j) * (1. - orgn_remove / 100.)

      !! calculate Nitrate removal from surface runoff
      !! Based on measured data from literature
	
	  remove1 = 39.4 + 0.584 * surq_remove1
	  if (remove1 > 100.) remove1 = 100.
	  if (remove1 < 0.) remove1 = 0.

	  remove2 = 39.4 + 0.584 * surq_remove2
	  if (remove2 > 100.) remove2 = 100.
      if (remove2 < 0.) remove2 = 0.

      surqno3_remove = (remove1 * drain_vfs1 + remove2 * drain_vfs2)/hru(j)%area_ha
	  surqno3(j) = surqno3(j) * (1. - surqno3_remove / 100.)

      !! calculate Particulate P removal from surface runoff
      !!Based on measured data from literature

	  remove1 = 0.903 * sed_remove1
	  if (remove1 > 100.) remove1 = 100.
	  if (remove1 < 0.) remove1 = 0.
	
	  remove2 = 0.903 * sed_remove2
	  if (remove2 > 100.) remove2 = 100.
	  if (remove2 < 0.) remove2 = 0.

	  partP_remove = (remove1 * drain_vfs1 + remove2 * drain_vfs2)/hru(j)%area_ha
	  sedminpa(j) = sedminpa(j) * (1. - partP_remove / 100.)
	  sedminps(j) = sedminps(j) * (1. - partP_remove / 100.)
	  sedorgp(j) = sedorgp(j) * (1. - partP_remove / 100.)

      !! Calculate Soluble P removal from surface runoff
      !!  DP% = - 6.14 + 1.13 Runoff%
	  remove1 = 29.3 + 0.51 * surq_remove1
	  if (remove1 > 100.) remove1 = 100.
	  if (remove1 < 0.) remove1 = 0.
	
	  remove21 = 29.3 + 0.51 * surq_remove2
	  if (remove2 > 100.) remove2 = 100.
	  if (remove2 < 0.) remove2 = 0.

	  solp_remove = (remove1 * drain_vfs1 + remove2 * drain_vfs2)/hru(j)%area_ha
	  surqsolp(j) = surqsolp(j) * (1. - solp_remove / 100.)

      !! Calculate pesticide removal 
      !! based on the sediment and runoff removal only
      do k = 1, cs_db%num_pests
        hpestb_d(j)%pest(k)%surq = hpestb_d(j)%pest(k)%surq * (1. - surq_remove / 100.)
        hpestb_d(j)%pest(k)%sed = hpestb_d(j)%pest(k)%sed * (1. - sed_remove / 100.)
      end do

	end if  ! if (surfq(j) > .0001)

    return
    end subroutine smp_filter
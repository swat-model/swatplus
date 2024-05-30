      subroutine ch_initial (idat, irch)

      use channel_data_module
      use channel_module
      
      implicit none
      
      integer :: irch     !none      |counter
      integer :: ised     !none      |counter
      integer :: idat     !units     |description
      real :: bnksize     !units     |description
      real :: bedsize     !units     |description
      real :: sc          !units     |description    
      
      ised = ch_dat(idat)%sed
      bnksize = ch_sed(ised)%bnk_d50 / 1000.  !! Units conversion Micrometer to Millimeters
!!    Channel sediment particle size distribution
!!    Clayey bank
	if (bnksize <= 0.005) then
	  ch(irch)%bnk_cla = 0.65
        ch(irch)%bnk_sil = 0.15
	  ch(irch)%bnk_san = 0.15
	  ch(irch)%bnk_gra = 0.05
	end if

!!    Silty bank
	if (bnksize > 0.005 .and. bnksize <= 0.05) then
        ch(irch)%bnk_sil = 0.65
	  ch(irch)%bnk_cla = 0.15
	  ch(irch)%bnk_san = 0.15
	  ch(irch)%bnk_gra = 0.05
	end if

!!    Sandy bank
	if (bnksize > 0.05 .and. bnksize <= 2.) then
	  ch(irch)%bnk_san = 0.65
        ch(irch)%bnk_sil = 0.15
	  ch(irch)%bnk_cla = 0.15
	  ch(irch)%bnk_gra = 0.05
	end if
      
!!    Gravel bank
	if (bnksize > 2.) then
	  ch(irch)%bnk_gra = 0.65
	  ch(irch)%bnk_san = 0.15
        ch(irch)%bnk_sil = 0.15
	  ch(irch)%bnk_cla = 0.05
	end if

!!    Channel sediment particle size distribution
!!    Clayey bed
      bedsize = ch_sed(ised)%bed_d50 / 1000.  !! Units conversion Micrometer to Millimeters
	if (bedsize <= 0.005) then
	  ch(irch)%bed_cla = 0.65
        ch(irch)%bed_sil = 0.15
	  ch(irch)%bed_san = 0.15
	  ch(irch)%bed_gra = 0.05
	end if

!!    Silty bed
	if (bedsize > 0.005 .and. bedsize <= 0.05) then
        ch(irch)%bed_sil = 0.65
	  ch(irch)%bed_cla = 0.15
	  ch(irch)%bed_san = 0.15
	  ch(irch)%bed_gra = 0.05
	end if

!!    Sandy bed
	if (bedsize > 0.05 .and. bedsize <= 2.) then
	  ch(irch)%bed_san = 0.65
        ch(irch)%bed_sil = 0.15
	  ch(irch)%bed_cla = 0.15
	  ch(irch)%bed_gra = 0.05
	end if
      
!!    Gravel bed
	if (bedsize > 2.) then
	  ch(irch)%bed_gra = 0.65
	  ch(irch)%bed_san = 0.15
        ch(irch)%bed_sil = 0.15
	  ch(irch)%bed_cla = 0.05
      end if
      
!!    An estimate of Critical shear stress if it is not given (N/m^2)
!!	Critical shear stress based on silt and clay %
!!	Critical Shear Stress based on Julian and Torres (2005)
!!    Units of critical shear stress (N/m^2)
	SC = 0.
	if  (ch_sed(ised)%tc_bnk <= 1.e-6) then 
	  SC = (ch(irch)%bnk_sil + ch(irch)%bnk_cla) * 100.
        ch_sed(ised)%tc_bnk = (0.1 + (0.1779*SC) + (0.0028*(SC)**2)       &
                         - ((2.34E-05)*(SC)**3)) * ch_sed(ised)%cov1
      end if

	if  (ch_sed(ised)%tc_bed <= 1.e-6) then
	  SC = (ch(irch)%bed_sil + ch(irch)%bed_cla) * 100.
        ch_sed(ised)%tc_bed = (0.1 + (0.1779*SC) + (0.0028*(SC)**2)       &
                         - ((2.34E-05)*(SC)**3)) * ch_sed(ised)%cov2
      end if

      return
      end subroutine ch_initial
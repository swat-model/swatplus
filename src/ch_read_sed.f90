      subroutine ch_read_sed
      
      use input_file_module
      use maximum_data_module
      use channel_data_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed. 

      implicit none

      integer :: eof                   !end of file
      integer :: imax                  !          |determine max number for array (imax) and total number in file
      character (len=80) :: titldum    !title of file
      character (len=80) :: header     !header of file
      logical :: i_exist               !          |check to determine if file exists
      real :: sumerod                  !units     |description
      integer :: ich                   !none      |counter
      integer :: mo                    !none      |counter

      eof = 0
      imax = 0

      inquire (file=in_cha%sed,exist=i_exist)
      if (.not. i_exist .or. in_cha%sed == "null") then
        allocate (ch_sed(0:0))
      else
      do
        open (105,file=in_cha%sed)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do   
          
        db_mx%ch_sed = imax
        
        allocate (ch_sed(0:imax))
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
             
        do ich = 1, db_mx%ch_sed
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) ch_sed(ich)
          if (eof < 0) exit
          
       if (ch_sed(ich)%tc_bnk <= 0.) ch_sed(ich)%tc_bnk=0. !! Critical shear stress (N.m^2)
        if (ch_sed(ich)%tc_bed <= 0.) ch_sed(ich)%tc_bed=0. !! Critical shear stress (N.m^2)

      if (ch_sed(ich)%eqn <= 0) then
        ch_sed(ich)%eqn=0 !! SWAT Default sediment routing routine
        if (ch_sed(ich)%cov1 <= 0.0) ch_sed(ich)%cov1 = 0.0
        if (ch_sed(ich)%cov2 <= 0.0) ch_sed(ich)%cov2 = 0.0
        if (ch_sed(ich)%cov1 >= 1.0) ch_sed(ich)%cov1 = 1.0
        if (ch_sed(ich)%cov2 >= 1.0) ch_sed(ich)%cov2 = 1.0
	else 
        if (ch_sed(ich)%cov1 <= 0.0) ch_sed(ich)%cov1 = 1.0
        if (ch_sed(ich)%cov2 <= 0.0) ch_sed(ich)%cov2 = 1.0
        if (ch_sed(ich)%cov1 >= 25.) ch_sed(ich)%cov1 = 25.
        if (ch_sed(ich)%cov2 >= 25.) ch_sed(ich)%cov2 = 25.
	end if
	  

!!    Bank material is assumed to be silt type partcile if not given.
      if (ch_sed(ich)%bnk_d50 <= 1.e-6) ch_sed(ich)%bnk_d50 = 50. !! Units are in Micrometer
      if (ch_sed(ich)%bnk_d50 > 10000) ch_sed(ich)%bnk_d50 = 10000.

!!    Bed material is assumed to be sand type partcile if not given.
      if (ch_sed(ich)%bed_d50 <= 1.e-6) ch_sed(ich)%bed_d50 = 500 !! Units are in Micrometer
      if (ch_sed(ich)%bed_d50 > 10000) ch_sed(ich)%bed_d50 = 10000. 

!!    Bulk density of channel bank sediment 
	if (ch_sed(ich)%bnk_bd <= 1.e-6) ch_sed(ich)%bnk_bd = 1.40 !! Silty loam bank

!!    Bulk density of channel bed sediment
	if (ch_sed(ich)%bed_bd <= 1.e-6) ch_sed(ich)%bed_bd = 1.50  !! Sandy loam bed

!!  An estimate of channel bank erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
      if (ch_sed(ich)%bnk_kd <= 1.e-6) then
	  if (ch_sed(ich)%tc_bnk <= 1.e-6) then
	    ch_sed(ich)%bnk_kd = 0.2
	  else 
          ch_sed(ich)%bnk_kd = 0.2 / sqrt(ch_sed(ich)%tc_bnk)
	  end if
	end if

!!  An estimate of channel bed erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
      if (ch_sed(ich)%bed_kd <= 1.e-6) then
	  if (ch_sed(ich)%tc_bed <= 1.e-6) then
	    ch_sed(ich)%bed_kd = 0.2
	  else 
          ch_sed(ich)%bed_kd = 0.2 / sqrt(ch_sed(ich)%tc_bed)
	  end if
      end if

      sumerod = 0.
      do mo = 1, 12
        sumerod = sumerod + ch_sed(ich)%erod(mo)
      end do

      if (sumerod < 1.e-6) then
        do mo = 1, 12
          ch_sed(ich)%erod(mo) = ch_sed(ich)%cov1
        end do
      end if
      
        end do
        exit
      end do
      end if
      
      close(105)
      
!!!!!!!!!!!!JEFF CODE!!!!!!!!!!!!!
! code from ch_readrte.f subroutine
!    initialize variables for channel degradation
!      ch(ich)%di = chdb(ich)%d 
!      ch(ich)%li = chdb(ich)%l
!      ch(ich)%si = chdb(ich)%s
!      ch(ich)%wi = chdb(ich)%w
      
!!    initialize flow routing variables
!     call ch_ttcoef (ich)
      
!     do ich = 1, RCHMAX
!        ichdb = ch_dat(ich)%hyd
!        ch(ich)%di = chdb(ichdb)%db
!        ...
!     end do
      
      return
      end subroutine ch_read_sed
      module ch_pesticide_module
    
      use constituent_mass_module, only : cs_db
      
      implicit none
              
      real :: frsol             !none          |fraction of pesticide in reach that is soluble
      real :: frsrb             !none          |fraction of pesticide in reach that is sorbed
      
      type ch_pesticide_processes
        real :: tot_in = 0.             ! kg        !total pesticide into reservoir
        real :: sol_out = 0.            ! kg        !soluble pesticide out of reservoir
        real :: sor_out = 0.            ! kg        !sorbed pesticide out of reservoir
        real :: react = 0.              ! kg        !pesticide lost through reactions in water layer
        real :: metab = 0.              ! kg        !pesticide metabolized from parent in water layer
        real :: volat = 0.              ! kg        !pesticide lost through volatilization
        real :: settle = 0.             ! kg        !pesticide settling to sediment layer
        real :: resus = 0.              ! kg        !pesticide resuspended into lake water
        real :: difus = 0.              ! kg        !pesticide diffusing from sediment to water
        real :: react_bot = 0.          ! kg        !pesticide lost from benthic sediment by reactions
        real :: metab_bot = 0.          ! kg        !pesticide metabolized from parent in water layer
        real :: bury = 0.               ! kg        !pesticide lost from benthic sediment by burial
        real :: water = 0.              ! kg        !pesticide in water at end of day
        real :: benthic = 0.            ! kg        !pesticide in benthic sediment at tend of day
      end type ch_pesticide_processes
      
      type ch_pesticide_output
        type (ch_pesticide_processes), dimension (:), allocatable :: pest         !pesticide hydrographs
      end type ch_pesticide_output
      type (ch_pesticide_processes) :: ch_pestbz
           
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_d
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_m
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_y
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_a
      type (ch_pesticide_output) :: bchpst_d
      type (ch_pesticide_output) :: bchpst_m
      type (ch_pesticide_output) :: bchpst_y
      type (ch_pesticide_output) :: bchpst_a
      type (ch_pesticide_output) :: chpst, chpstz
                 
      type ch_pesticide_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name              "  
          character (len=16) :: pest =      " pesticide"
          character(len=13) :: tot_in =     "tot_in_kg "            ! (kg)
          character(len=13) :: sol_out =    "sol_out_kg "           ! (kg)
          character(len=14) :: sor_out =    "sor_out_kg "           ! (kg)
          character(len=13) :: react =      "react_h2o_kg "         ! (kg)
          character(len=13) :: metab =      "metab_h2o_kg "         ! (kg)
          character(len=12) :: volat =      "volat_kg "             ! (kg)
          character(len=12) :: settle =     "settle_kg "            ! (kg)
          character(len=13) :: resus =      "resuspend_kg "         ! (kg)
          character(len=12) :: difus =      "diffuse_kg "           ! (kg)
          character(len=15) :: react_bot =  "react_benth_kg "       ! (kg)
          character(len=15) :: metab_bot =  "metab_benth_kg "       ! (kg)
          character(len=14) :: bury =       "bury_benth_kg "        ! (kg)
          character(len=14) :: water =      "water_stor_kg "        ! (kg)
          character(len=12) :: benthic =    "benthic_kg "           ! (kg)
      end type ch_pesticide_header
      type (ch_pesticide_header) :: chpest_hdr
     
      interface operator (+)
        module procedure chpest_add
      end interface
      
      interface operator (/)
        module procedure chpest_div
      end interface
        
      interface operator (//)
        module procedure chpest_ave
      end interface 
             
      contains

      function chpest_add(cho1, cho2) result (cho3)
        type (ch_pesticide_processes),  intent (in) :: cho1
        type (ch_pesticide_processes),  intent (in) :: cho2
        type (ch_pesticide_processes) :: cho3
        cho3%tot_in = cho1%tot_in + cho2%tot_in
        cho3%sol_out = cho1%sol_out + cho2%sol_out
        cho3%sor_out = cho1%sor_out + cho2%sor_out
        cho3%react = cho1%react + cho2%react
        cho3%metab = cho1%metab + cho2%metab
        cho3%volat = cho1%volat + cho2%volat
        cho3%settle = cho1%settle + cho2%settle
        cho3%resus = cho1%resus + cho2%resus
        cho3%difus = cho1%difus + cho2%difus
        cho3%react_bot = cho1%react_bot + cho2%react_bot
        cho3%metab_bot = cho1%metab_bot + cho2%metab_bot
        cho3%bury = cho1%bury + cho2%bury
        cho3%water = cho1%water + cho2%water
        cho3%benthic = cho1%benthic + cho2%benthic
      end function chpest_add
      
      function chpest_div (ch1, const) result (ch2)
        type (ch_pesticide_processes), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_pesticide_processes) :: ch2
          ch2%tot_in = ch1%tot_in / const
          ch2%sol_out = ch1%sol_out / const
          ch2%sor_out = ch1%sor_out / const
          ch2%react = ch1%react / const
          ch2%metab = ch1%metab / const
          ch2%volat = ch1%volat / const
          ch2%settle = ch1%settle / const
          ch2%resus = ch1%resus / const
          ch2%difus = ch1%difus / const
          ch2%react_bot = ch1%react_bot / const
          ch2%metab_bot = ch1%metab_bot / const
          ch2%bury = ch1%bury / const
          ch2%water = ch1%water / const
          ch2%benthic = ch1%benthic / const
      end function chpest_div
      
      function chpest_ave (ch1, const) result (ch2)
        type (ch_pesticide_processes), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_pesticide_processes) :: ch2
          ch2%tot_in = ch1%tot_in
          ch2%sol_out = ch1%sol_out
          ch2%sor_out = ch1%sor_out
          ch2%react = ch1%react
          ch2%metab = ch1%metab
          ch2%volat = ch1%volat
          ch2%settle = ch1%settle
          ch2%resus = ch1%resus
          ch2%difus = ch1%difus
          ch2%react_bot = ch1%react_bot
          ch2%metab_bot = ch1%metab_bot
          ch2%bury = ch1%bury
          ch2%water = ch1%water
          ch2%benthic = ch1%benthic
      end function chpest_ave
      
      end module ch_pesticide_module
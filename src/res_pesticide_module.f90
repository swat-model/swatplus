      module res_pesticide_module

      implicit none
              
      type res_pesticide_processes
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
        real :: benthic = 0.            ! kg        !pesticide in benthic sediment at end of day
      end type res_pesticide_processes
      
      type res_pesticide_output
        type (res_pesticide_processes), dimension (:), allocatable :: pest         !pesticide hydrographs
      end type res_pesticide_output
      type (res_pesticide_processes) :: res_pestbz
           
      type (res_pesticide_output), dimension(:), allocatable, save :: respst_d
      type (res_pesticide_output), dimension(:), allocatable, save :: respst_m
      type (res_pesticide_output), dimension(:), allocatable, save :: respst_y
      type (res_pesticide_output), dimension(:), allocatable, save :: respst_a
      type (res_pesticide_output) :: brespst_d
      type (res_pesticide_output) :: brespst_m
      type (res_pesticide_output) :: brespst_y
      type (res_pesticide_output) :: brespst_a
      type (res_pesticide_output) :: respst, respstz
                 
      type res_pesticide_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name"
          character (len=16) :: pest =      " pesticide"
          character(len=13) :: tot_in =     "tot_in_kg "            ! (kg)
          character(len=13) :: sol_out =    "sol_out_kg "           ! (kg)
          character(len=14) :: sor_out =    "sor_out_kg "           ! (kg)
          character(len=13) :: react =      "react_h2o_kg"        	! (kg)
          character(len=13) :: metab =      "metab_h2o_kg"        	! (kg)
          character(len=10) :: volat =      "volat_kg"        		! (kg)
          character(len=10) :: settle =     "settle_kg"        		! (kg)
          character(len=13) :: resus =      "resuspend_kg"        	! (kg)
          character(len=11) :: difus =      "diffuse_kg "        	! (kg)
          character(len=15) :: react_bot =  "react_benth_kg "       ! (kg)
          character(len=14) :: metab_bot =  "metab_benth_kg "       ! (kg)
          character(len=14) :: bury =       "bury_benth_kg "        ! (kg)
          character(len=14) :: water =      "water_stor_kg "        ! (kg)
          character(len=13) :: benthic =    "benthic_kg"        	! (kg)
      end type res_pesticide_header
      type (res_pesticide_header) :: respest_hdr
     
      interface operator (+)
        module procedure respest_add
      end interface
      
      interface operator (/)
        module procedure respest_div
      end interface
        
      interface operator (//)
        module procedure respest_ave
      end interface 
             
      contains
!! routines for swatdeg_hru module

      function respest_add(res1, res2) result (res3)
        type (res_pesticide_processes),  intent (in) :: res1
        type (res_pesticide_processes),  intent (in) :: res2
        type (res_pesticide_processes) :: res3
        res3%tot_in = res1%tot_in + res2%tot_in
        res3%sol_out = res1%sol_out + res2%sol_out
        res3%sor_out = res1%sor_out + res2%sor_out
        res3%react = res1%react + res2%react
        res3%metab = res1%metab + res2%metab
        res3%volat = res1%volat + res2%volat
        res3%settle = res1%settle + res2%settle
        res3%resus = res1%resus + res2%resus
        res3%difus = res1%difus + res2%difus
        res3%react_bot = res1%react_bot + res2%react_bot
        res3%metab_bot = res1%metab_bot + res2%metab_bot
        res3%bury = res1%bury + res2%bury
        res3%water = res1%water + res2%water
        res3%benthic = res1%benthic + res2%benthic
      end function respest_add
      
      function respest_div (res1, const) result (res2)
        type (res_pesticide_processes), intent (in) :: res1
        real, intent (in) :: const
        type (res_pesticide_processes) :: res2
          res2%tot_in = res1%tot_in / const
          res2%sol_out = res1%sol_out / const
          res2%sor_out = res1%sor_out / const
          res2%react = res1%react / const
          res2%metab = res1%metab / const
          res2%volat = res1%volat / const
          res2%settle = res1%settle / const
          res2%resus = res1%resus / const
          res2%difus = res1%difus / const
          res2%react_bot = res1%react_bot / const
          res2%metab_bot = res1%metab_bot / const
          res2%bury = res1%bury / const
          res2%water = res1%water / const
          res2%benthic = res1%benthic / const
      end function respest_div
      
      function respest_ave (res1, const) result (res2)
        type (res_pesticide_processes), intent (in) :: res1
        real, intent (in) :: const
        type (res_pesticide_processes) :: res2
          res2%tot_in = res1%tot_in
          res2%sol_out = res1%sol_out
          res2%sor_out = res1%sor_out
          res2%react = res1%react
          res2%metab = res1%metab
          res2%volat = res1%volat
          res2%settle = res1%settle
          res2%resus = res1%resus
          res2%difus = res1%difus
          res2%react_bot = res1%react_bot
          res2%metab_bot = res1%metab_bot
          res2%bury = res1%bury
          res2%water = res1%water
          res2%benthic = res1%benthic
      end function respest_ave
      
      end module res_pesticide_module
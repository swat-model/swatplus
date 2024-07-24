      !module for constituent mass balance in aquifers (rtb cs)
      module cs_aquifer
      
      implicit none

      type cs_balance_aqu
        real :: csgw = 0.            !! |kg       |mass loaded to streams from the aquifer
        real :: rchrg = 0.           !! |kg       |mass reaching the water table (recharge)
        real :: seep = 0.            !! |kg       |mass seepage out of aquifer
        real :: irr = 0.             !! |kg       |mass removed via irrigation (groundwater pumping)
        real :: div = 0.             !! |kg       |mass removed or added via diversion
        real :: sorb = 0.            !! |kg       |mass transferred from sorbed phase to dissolved phase
        real :: rctn = 0.            !! |kg       |mass transferred by chemical reaction
        real :: mass = 0.            !! |kg       !mass stored in aquifer
        real :: conc = 0.            !! |g/m3     |concentration in groundwater
        real :: srbd = 0.            !! |kg       |mass sorbed to aquifer material
      end type cs_balance_aqu
      
      type object_cs_balance_aqu
        type (cs_balance_aqu), dimension (:), allocatable :: cs
      end type object_cs_balance_aqu
      
      !aquifer system fluxes
      type (object_cs_balance_aqu), dimension (:), allocatable :: acsb_d
      type (object_cs_balance_aqu), dimension (:), allocatable :: acsb_m
      type (object_cs_balance_aqu), dimension (:), allocatable :: acsb_y
      type (object_cs_balance_aqu), dimension (:), allocatable :: acsb_a
      
      !basin-wide cs fluxes - aquifer
      type (object_cs_balance_aqu) :: bacs_d
      type (object_cs_balance_aqu) :: bacs_m
      type (object_cs_balance_aqu) :: bacs_y
      type (object_cs_balance_aqu) :: bacs_a
      type (object_cs_balance_aqu) :: csbz_aqu
      
      !header for cs output
      type output_cs_header
         character (len=6) :: day =        "  jday"
         character (len=6) :: mo =         "   mon"
         character (len=6) :: day_mo =     "   day"
         character (len=6) :: yrc =        "    yr"
         character (len=8) :: isd =        "   unit "
         character (len=12) :: id =        " gis_id "
         character(len=18) :: seo4 =       "   gw_seo4"
         character(len=18) :: seo3 =       "   gw_seo3"
         character(len=18) :: born =       "   gw_born"
         character(len=18) :: seo4r =      " rchg_seo4"
         character(len=18) :: seo3r =      " rchg_seo3"
         character(len=18) :: bornr =      " rchg_born"
         character(len=18) :: seo4s =      " seep_seo4"
         character(len=18) :: seo3s =      " seep_seo3"
         character(len=18) :: borns =      " seep_born"
         character(len=18) :: seo4i =      " irr_seo4"
         character(len=18) :: seo3i =      " irr_seo3"
         character(len=18) :: borni =      " irr_born"
         character(len=18) :: seo4v =      " div_seo4"
         character(len=18) :: seo3v =      " div_seo3"
         character(len=18) :: bornv =      " div_born"
         character(len=18) :: seo4b =      " sorb_seo4"
         character(len=18) :: seo3b =      " sorb_seo3"
         character(len=18) :: bornb =      " sorb_born"
         character(len=18) :: seo4t =      " rctn_seo4"
         character(len=18) :: seo3t =      " rctn_seo3"
         character(len=18) :: bornt =      " rctn_born"
         character(len=18) :: seo4m =      " mass_seo4"
         character(len=18) :: seo3m =      " mass_seo3"
         character(len=18) :: bornm =      " mass_born"
         character(len=18) :: seo4c =      " conc_seo4"
         character(len=18) :: seo3c =      " conc_seo3"
         character(len=18) :: bornc =      " conc_born"
         character(len=18) :: seo4d =      " srbd_seo4"
         character(len=18) :: seo3d =      " srbd_seo3"
         character(len=18) :: bornd =      " srbd_born"
      end type output_cs_header      
      type (output_cs_header) :: cs_hdr_aqu
      
      end module
      
      
		
      
      
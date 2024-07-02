      module salt_aquifer

      implicit none
    
      real :: testing_aquifer
      
      type salt_balance_aqu
        real :: diss = 0.            !! |kg       |salt ion mass transferred from sorbed phase to dissolved phase
        real :: rchrg = 0.           !! |kg       |salt ion mass reaching the water table (recharge)
        real :: seep = 0.            !! |kg       |salt ion mass seepage out of aquifer
        real :: saltgw = 0.          !! |kg       |salt ion mass loaded to streams from the aquifer
        real :: irr = 0.             !! |kg       |salt ion mass removed via irrigation (groundwater pumping)
        real :: div = 0.             !! |kg       |salt ion mass removed via diversion
        real :: mass = 0.            !! |kg       !salt ion mass in aquifer
        real :: conc = 0.            !! |g/m3     |salt ion mass concentration in groundwater
      end type salt_balance_aqu
      
      type object_salt_balance_aqu
        type (salt_balance_aqu), dimension (:), allocatable :: salt
      end type object_salt_balance_aqu
      
      !aquifer system fluxes
      type (object_salt_balance_aqu), dimension (:), allocatable :: asaltb_d
      type (object_salt_balance_aqu), dimension (:), allocatable :: asaltb_m
      type (object_salt_balance_aqu), dimension (:), allocatable :: asaltb_y
      type (object_salt_balance_aqu), dimension (:), allocatable :: asaltb_a
      
      !basin-wide salt fluxes - aquifer
      type (object_salt_balance_aqu) :: basalt_d
      type (object_salt_balance_aqu) :: basalt_m
      type (object_salt_balance_aqu) :: basalt_y
      type (object_salt_balance_aqu) :: basalt_a
      type (object_salt_balance_aqu) :: saltbz_aqu
      
      !header for salt output
      type output_salt_header
         character (len=6) :: day =        "  jday"
         character (len=6) :: mo =         "   mon"
         character (len=6) :: day_mo =     "   day"
         character (len=6) :: yrc =        "    yr"
         character (len=8) :: isd =        "   unit "
         character (len=12) :: id =        " gis_id "
         character(len=18) :: so4 =        "   gw_so4"
         character(len=18) :: ca =         "    gw_ca"
         character(len=18) :: mg =         "    gw_mg"
         character(len=18) :: na =         "    gw_na"
         character(len=18) :: k =          "     gw_k"
         character(len=18) :: cl =         "    gw_cl"
         character(len=18) :: co3 =        "   gw_co3"
         character(len=18) :: hco3 =       "  gw_hco3"
         character(len=18) :: so4r =       " rchg_so4"
         character(len=18) :: car =        "  rchg_ca"
         character(len=18) :: mgr =        "  rchg_mg"
         character(len=18) :: nar =        "  rchg_na"
         character(len=18) :: kr =         "   rchg_k"
         character(len=18) :: clr =        "  rchg_cl"
         character(len=18) :: co3r =       " rchg_co3"
         character(len=18) :: hco3r =      "rchg_hco3"
         character(len=18) :: so4s =       " seep_so4"
         character(len=18) :: cas =        "  seep_ca"
         character(len=18) :: mgs =        "  seep_mg"
         character(len=18) :: nas =        "  seep_na"
         character(len=18) :: ks =         "   seep_k"
         character(len=18) :: cls =        "  seep_cl"
         character(len=18) :: co3s =       " seep_co3"
         character(len=18) :: hco3s =      "seep_hco3"
         character(len=18) :: so4i =       "  irr_so4"
         character(len=18) :: cai =        "   irr_ca"
         character(len=18) :: mgi =        "   irr_mg"
         character(len=18) :: nai =        "   irr_na"
         character(len=18) :: ki =         "    irr_k"
         character(len=18) :: cli =        "   irr_cl"
         character(len=18) :: co3i =       "  irr_co3"
         character(len=18) :: hco3i =      " irr_hco3"
         character(len=18) :: so4d =       "  div_so4"
         character(len=18) :: cad =        "   div_ca"
         character(len=18) :: mgd =        "   div_mg"
         character(len=18) :: nad =        "   div_na"
         character(len=18) :: kd =         "    div_k"
         character(len=18) :: cld =        "   div_cl"
         character(len=18) :: co3d =       "  div_co3"
         character(len=18) :: hco3d =      " div_hco3"
         character(len=18) :: so4m =       " mass_so4"
         character(len=18) :: cam =        "  mass_ca"
         character(len=18) :: mgm =        "  mass_mg"
         character(len=18) :: nam =        "  mass_na"
         character(len=18) :: km =         "   mass_k"
         character(len=18) :: clm =        "  mass_cl"
         character(len=18) :: co3m =       " mass_co3"
         character(len=18) :: hco3m =      "mass_hco3"
         character(len=18) :: so4c =       " conc_so4"
         character(len=18) :: cac =        "  conc_ca"
         character(len=18) :: mgc =        "  conc_mg"
         character(len=18) :: nac =        "  conc_na"
         character(len=18) :: kc =         "   conc_k"
         character(len=18) :: clc =        "  conc_cl"
         character(len=18) :: co3c =       " conc_co3"
         character(len=18) :: hco3c =      "conc_hco3"
         character(len=18) :: dssl =      "dssl_total"
      end type output_salt_header      
      type (output_salt_header) :: salt_hdr_aqu
      
      end module     
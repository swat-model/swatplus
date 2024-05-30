      module res_cs_module !rtb cs

      implicit none
        
      !reservoir balance components
      type res_cs_balance
        real :: inflow = 0.             ! kg        !constituent entering the reservoir
        real :: outflow = 0.            ! kg        !constituent leaving the reservoir via streamflow
        real :: seep = 0.               ! kg        !constituent leaving the reservoir via seepage to aquifer 
        real :: settle = 0.             ! kg        !constituent settling to bottom of reservoir
        real :: rctn = 0.               ! kg        !constituent removal due to chemical reaction
        real :: prod = 0.               ! kg        !constituent produced due to chemical reaction
        real :: fert = 0.               ! kg        !constituent added in fertilizer (to wetland)
        real :: irrig = 0.              ! kg        !constituent removed from the reservoir via irrigation diversion
        real :: div = 0.                ! kg        !constituent removed or added via diversion
        real :: mass = 0.               ! kg        !constituent in reservoir water at end of day
        real :: conc = 0.               ! g/m3      !constituent concentration in reservoir at end of day
        real :: volm = 0.               ! m3        !volume of water in the reservoir
      end type res_cs_balance
      
      type res_cs_output
        type (res_cs_balance), dimension (:), allocatable :: cs         !constituents hydrographs
      end type res_cs_output
      type (res_cs_balance) :: res_csbz
      
      !arrays for reservoir mass balance output
      type (res_cs_output), dimension(:), allocatable, save :: rescs_d
      type (res_cs_output), dimension(:), allocatable, save :: rescs_m
      type (res_cs_output), dimension(:), allocatable, save :: rescs_y
      type (res_cs_output), dimension(:), allocatable, save :: rescs_a
			
      !arrays for wetland mass balance output
      type (res_cs_output), dimension(:), allocatable, save :: wetcs_d
      type (res_cs_output), dimension(:), allocatable, save :: wetcs_m
      type (res_cs_output), dimension(:), allocatable, save :: wetcs_y
      type (res_cs_output), dimension(:), allocatable, save :: wetcs_a
      
      !reservoir constituent parameters
      type reservoir_cs_data
        character(len=25) :: name
        real :: v_seo4 = 0.001            !m/day      |settling rate for selenate
        real :: v_seo3 = 0.001            !m/day      |settling rate for selinite
        real :: v_born = 0.001            !m/day      |settling rate for boron
        real :: k_seo4 = 0.05             !1/day      |first-order degradation constant for selenate
        real :: k_seo3 = 0.03             !1/day      |first-order degradation constant for selenite
        real :: k_born = 0.00             !1/day      |first-order degradation constant for boron
        real :: theta_seo4 = 1.08         !none       |temperature adjustment for selenate degradation
        real :: theta_seo3 = 1.08         !none       |temperature adjustment for selenite degradation
        real :: theta_born = 1.08         !none       |temperature adjustment for boron degradation
        real :: c_seo4 = 0.               !g/m3       |initial concentration of selenate
        real :: c_seo3 = 0.               !g/m3       |initial concentration of selenite
        real :: c_born = 0.               !g/m3       |initial concentration of boron
      end type reservoir_cs_data
      type (reservoir_cs_data), dimension(:), allocatable :: res_cs_data
      
      !output file headers
      type res_cs_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=12) :: id =        " gis_id "           
          character(len=15) :: seo4in =     "seo4_in "
          character(len=15) :: seo3in =     "seo3_in  "
          character(len=15) :: bornin =     "born_in  "
          character(len=15) :: seo4out =    "seo4_out "
          character(len=15) :: seo3out =    "seo3_out  "
          character(len=15) :: bornout =    "born_out  "
          character(len=15) :: seo4seep =   "seo4_seep "
          character(len=15) :: seo3seep =   "seo3_seep "
          character(len=15) :: bornseep =   "born_seep "
          character(len=15) :: seo4setl =   "seo4_setl "
          character(len=15) :: seo3setl =   "seo3_setl "
          character(len=15) :: bornsetl =   "born_setl "
          character(len=15) :: seo4rctn =   "seo4_rctn "
          character(len=15) :: seo3rctn =   "seo3_rctn "
          character(len=15) :: bornrctn =   "born_rctn "
          character(len=15) :: seo4prod =   "seo4_prod "
          character(len=15) :: seo3prod =   "seo3_prod "
          character(len=15) :: bornprod =   "born_prod "
          character(len=15) :: seo4fert =   "seo4_fert "
          character(len=15) :: seo3fert =   "seo3_fert "
          character(len=15) :: bornfert =   "born_fert "
          character(len=15) :: seo4irr =    "seo4_irrg "
          character(len=15) :: seo3irr =    "seo3_irrg  "
          character(len=15) :: bornirr =    "born_irrg  "
          character(len=15) :: seo4div =    "seo4_div "
          character(len=15) :: seo3div =    "seo3_div  "
          character(len=15) :: borndiv =    "born_div  "
          character(len=15) :: seo4 =       "seo4_mass "
          character(len=15) :: seo3 =       "seo3_mass  "
          character(len=15) :: born =       "born_mass  "
          character(len=15) :: seo4c =      "seo4_conc"
          character(len=15) :: seo3c =      "seo3_conc"
          character(len=15) :: bornc =      "born_conc"
          character(len=15) :: volm =       "vol_m3"
      end type res_cs_header
      type (res_cs_header) :: rescs_hdr

      
      end module res_cs_module
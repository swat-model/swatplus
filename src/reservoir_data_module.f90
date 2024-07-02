    module reservoir_data_module
    
    implicit none      

      type reservoir_data_char_input
        character (len=25) :: name = "default"
        character (len=25) :: init                  !initial data-points to initial.res
        character (len=25) :: hyd                   !points to hydrology.res for hydrology inputs
        character (len=25) :: release               !0=simulated; 1=measured outflow
        character (len=25) :: sed                   !sediment inputs-points to sediment.res
        character (len=25) :: nut                   !nutrient inputs-points to nutrient.res    
      end type reservoir_data_char_input
      type (reservoir_data_char_input), dimension(:), allocatable :: res_dat_c
      type (reservoir_data_char_input), dimension(:), allocatable :: wet_dat_c

      !rtb salt/cs
      type reservoir_data_char_input_cs  
        character (len=25) :: pst                   !pesticide inputs-points to pesticide.res    
        character (len=25) :: weir                  !weir inputs-points to weir.res    Jaehak 2022
        character (len=25) :: salt                  !salt inputs - points to salt_res rtb salt
        character (len=25) :: cs                    !constituent inputs - points to cs_res rtb cs
      end type reservoir_data_char_input_cs
      type (reservoir_data_char_input_cs), dimension(:), allocatable :: res_dat_c_cs
      type (reservoir_data_char_input_cs), dimension(:), allocatable :: wet_dat_c_cs
      
      type reservoir_data
        character(len=25) :: name = "default"
        integer :: init = 0                   !initial data-points to initial.res
        integer :: hyd = 0                    !points to hydrology.res for hydrology inputs
        integer :: release = 0                !0=simulated; 1=measured outflow
        integer :: sed = 0                    !sediment inputs-points to sediment.res
        integer :: nut = 0                    !nutrient inputs-points to nutrient.res
        integer :: pst = 0                    !pesticide inputs-points to pesticide.res
        integer :: salt = 0                   !salt input-points to salt.res
        integer :: cs = 0                     !constituent inputs-points to cs.res
        character (len=25) :: weir            !weir inputs-points to weir.res  Jaehak 2022  
      end type reservoir_data
      type (reservoir_data), dimension(:), allocatable :: res_dat
      type (reservoir_data), dimension(:), allocatable :: wet_dat
      type (reservoir_data) :: res_datz
            
      type reservoir_init_data_char
        character (len=25) :: init                 !initial data-points to initial.cha
        character (len=25) :: org_min              !points to initial organic-mineral input file
        character (len=25) :: pest                 !points to initial pesticide input file
        character (len=25) :: path                 !points to initial pathogen input file
        character (len=25) :: hmet                 !points to initial heavy metals input file
        character (len=25) :: salt                 !points to initial salt input file
      end type reservoir_init_data_char
      type (reservoir_init_data_char), dimension(:), allocatable :: res_init_dat_c
            
      type reservoir_init_data
        integer :: init = 1                 !initial data-points to initial.cha
        integer :: org_min = 1              !points to initial organic-mineral input file
        integer :: pest = 1                 !points to initial pesticide input file
        integer :: path = 1                 !points to initial pathogen input file
        integer :: hmet = 1                 !points to initial heavy metals input file
        integer :: salt = 1                 !points to initial salt input file
        integer :: cs = 1                   !points to initial constituent input file (rtb cs)
      end type reservoir_init_data
      type (reservoir_init_data), dimension(:), allocatable :: res_init
      type (reservoir_init_data), dimension(:), allocatable :: wet_init
            
      type reservoir_hyd_data
        character(len=25) :: name = "default"
        integer :: iyres = 0      !none          |year of the sim that the res becomes operational
        integer :: mores = 0      !none          |month the res becomes operational
        real :: psa = 0.          !ha            |res surface area when res is filled to princ spillway
        real :: pvol = 0.         !ha-m          |vol of water needed to fill the res to the princ spillway (read in as ha-m
                                  !                and converted to m^3)
        real :: esa = 0.          !ha            |res surface area when res is filled to emerg spillway 
        real :: evol = 0.         !ha-m          |vol of water needed to fill the res to the emerg spillway (read in as ha-m
                                  !                and converted to m^3)
        real :: k = .01           !mm/hr         |hydraulic conductivity of the res bottom
        real :: evrsv = .7        !none          |lake evap coeff
        real :: br1 = 0.          !none          |vol-surface area coefficient for reservoirs (model estimates if zero)
        real :: br2 = 0.          !none          |vol-surface area coefficient for reservoirs (model estimates if zero)
      end type reservoir_hyd_data
      type (reservoir_hyd_data), dimension(:), allocatable :: res_hyd
      type (reservoir_hyd_data), dimension(:), allocatable :: res_hyddb
      
      type wetland_hyd_data
        character(len=25) :: name = "default"
        real :: psa = 0.          !frac          |fraction of hru area at principal spillway (ie: when surface inlet riser flow starts)
        real :: pdep = 0.         !mm            |average depth of water at principal spillway
        real :: esa = 0.          !frac          |fraction of hru area at emergency spillway (ie: when starts to spill into ditch)
        real :: edep = 0.         !mm            |average depth of water at emergency spillway
        real :: k = .01           !mm/hr         |hydraulic conductivity of the wetland bottom
        real :: evrsv = .7        !none          |wetland evap coeff
        real :: acoef = 1.        !none          |vol-surface area coefficient for hru impoundment
        real :: bcoef = 1.        !none          |vol-depth coefficient for hru impoundment
        real :: ccoef = 1.        !none          |vol-depth coefficient for hru impoundment
        real :: frac = .5         !none          |fraction of hru that drains into impoundment
      end type wetland_hyd_data
      type (wetland_hyd_data), dimension(:), allocatable :: wet_hyd
      type (wetland_hyd_data), dimension(:), allocatable :: wet_hyddb
      
      type reservoir_sed_data
        character(len=25) :: name
        real :: nsed                !kg/L       |normal amt of sed in res (read in as mg/L and convert to kg/L)
        real :: d50                 !mm         |median particle size of suspended and benthic sediment
        real :: carbon              !%          |organic carbon in suspended and benthic sediment
        real :: bd                  !t/m^3      |bulk density of benthic sediment
        real :: sed_stlr            !none       |sediment settling rate
        real :: velsetlr            !m/d        |sediment settling velocity
      end type reservoir_sed_data
      type (reservoir_sed_data), dimension(:), allocatable :: res_sed
            
      type reservoir_nut_data
        character(len=25) :: name
        integer :: ires1            !none       |beg of mid-year nutrient settling "season"
        integer :: ires2            !none       |end of mid-year nutrient settling "season"
        real :: nsetlr1             !frac       |nit mass loss rate for mid-year period 
        real :: nsetlr2             !frac       |nit mass loss rate for remainder of year
        real :: psetlr1             !frac       |phos mass loss rate for mid-year period
        real :: psetlr2             !frac       |phos mass loss rate for remainder of year
        real :: chlar = 1.          !none       |chlorophyll-a production coeff for res
        real :: seccir = 1.0        !none       |water clarity coeff for res
        real :: theta_n = 1.        !none       |temperature adjustment for nitrogen loss (settling)
        real :: theta_p = 1.        !none       |temperature adjustment for phosphorus loss (settling)
        real :: conc_nmin = .1      !ppm        |minimum nitrogen concentration for settling
        real :: conc_pmin = .01     !ppm        |minimum phosphorus concentration for settling
      end type reservoir_nut_data
      type (reservoir_nut_data), dimension(:), allocatable :: res_nut
          
      type water_body_data_parameters
        type (reservoir_sed_data) :: sed
        type (reservoir_nut_data) :: nut
      end type water_body_data_parameters
      type (water_body_data_parameters), dimension(:), allocatable, target :: res_prm
      type (water_body_data_parameters), dimension(:), allocatable, target :: wet_prm
      type (water_body_data_parameters), pointer :: wbody_prm       !! used for reservoir and wetlands
      
      type reservoir_weir_outflow
        character(len=25) :: name
        real :: c = 1.84              !none          |weir discharge linear coefficient 
        real :: k = 2.6               !none          |weir discharge exponential coefficient
        real :: w = 2.5               !m             |width
        real :: h = 0.0               !m             |height of weir above bottoom of impoundment
      end type reservoir_weir_outflow
      type (reservoir_weir_outflow),dimension(:),allocatable :: res_weir   
    
      end module reservoir_data_module 
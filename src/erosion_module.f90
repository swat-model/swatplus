      module erosion_module
    
      implicit none 

      type erosion_output_variables
        real :: sedyld = 0.         !t/ha       |sediment yield
        real :: precip = 0.         !mm         |precipitation
        real :: surfq = 0.          !mm         |surface runoff
        real :: peak = 0.           !m3/s       |peak rate
        real :: k = 0.              !           |usle k factor
        real :: s = 0.              !m/m        |slope
        real :: l = 0.              !m          |slope length
        real :: ls = 0.             !           |usle ls factor
        real :: p = 0.              !           |usle p factor
        real :: c = 0.              !           |usle c factor
        real :: rsd_m = 0.          !kg/ha      |surface residue mass
        real :: grcov_frac = 0.     !frac       |ground cover fraction
        real :: rsd_covfact = 0.    !           |residue cover factor
        real :: bio_covfact = 0.    !           |growing biomass factor
      end type erosion_output_variables

      type erosion_output
        integer :: n_events = 0.                        !number of erosion events
        type (erosion_output_variables) :: ero_d        !ersion variables at each erosion event
        type (erosion_output_variables) :: ero_ave      !erosion variables averaged by number of events
      end type erosion_output
      type (erosion_output), dimension(:), allocatable  :: ero_output   !dimensioned by hru
      
      type erosion_output_header
        character (len=6) :: hru          =  "   hru"
        character (len=6) :: neve         =  "events"
        character (len=6) :: sedyld       =  "sedyld"
        character (len=6) :: precip       =  "precip"
        character (len=9) :: peak         =  "peak_rate"
        character (len=8) :: k            =  "k_factor"
        character (len=16) :: s           =  "      slope"
        character (len=12) :: l           =  "     length"
        character (len=12)  :: ls         =  "  ls_factor"
        character (len=12)  :: p          =  "   p_factor"
        character (len=12)  :: c          =  "   c_factor"
        character (len=12)  :: rsd_m      =  "      rsd_m"
        character (len=12)  :: grcov_frac =  " grcov_frac"
        character (len=12)  :: rsd_covfact   =  " rsd_covfact"
        character (len=12)  :: bio_covfact   =  " bio_covfact"
      end type erosion_output_header      
      type (erosion_output_header) :: ero_hdr
      
       type erosion_header_units
        character (len=6) :: hru          =  "      "
        character (len=6) :: neve         =  "      "
        character (len=6) :: sedyld       =  "  t/ha"
        character (len=6) :: precip       =  "    mm"
        character (len=8) :: peak         =  "    m3/s"
        character (len=8) :: k            =  "        "
        character (len=16) :: s           =  "        m/m"
        character (len=12) :: l           =  "          m"
        character (len=12)  :: ls         =  "           "
        character (len=12)  :: p          =  "           "
        character (len=12)  :: c          =  "           "
        character (len=12)  :: rsd_m      =  "      kg/ha"
        character (len=12)  :: grcov_frac =  "       frac"
        character (len=12)  :: rsd_covfact   =  "           "
        character (len=12)  :: bio_covfact   =  "           "
      end type erosion_header_units      
      type (erosion_header_units) :: ero_hdr_units
   
      !objects needed for operators
      type (erosion_output_variables) :: ero_1, ero_2, ero_3

      
      interface operator (+)
        module procedure ero_add
      end interface
                         
      interface operator (/)
        module procedure ero_divide
      end interface 
                   

contains
      
      !! add erosion outputs for each event
      function ero_add (ero_1, ero_2) result (ero_3)
        type (erosion_output_variables), intent (in) :: ero_1
        type (erosion_output_variables), intent (in) :: ero_2
        type (erosion_output_variables) :: ero_3
        ero_3%sedyld = ero_1%sedyld + ero_2%sedyld
        ero_3%precip = ero_1%precip + ero_2%precip
        ero_3%surfq = ero_1%surfq + ero_2%surfq
        ero_3%peak = ero_1%peak + ero_2%peak
        ero_3%k = ero_1%k + ero_2%k
        ero_3%s = ero_1%s + ero_2%s
        ero_3%l = ero_1%l + ero_2%l
        ero_3%ls = ero_1%ls + ero_2%ls
        ero_3%p = ero_1%p + ero_2%p
        ero_3%rsd_m = ero_1%rsd_m + ero_2%rsd_m
        ero_3%grcov_frac = ero_1%grcov_frac + ero_2%grcov_frac
        ero_3%rsd_covfact = ero_1%rsd_covfact + ero_2%rsd_covfact
        ero_3%bio_covfact = ero_1%bio_covfact + ero_2%bio_covfact
        ero_3%c = ero_1%c + ero_2%c
      end function ero_add
                          
      !! divide erosion outputs by number of events
      function ero_divide (ero_1, const) result (ero_2)
        type (erosion_output_variables), intent (in) :: ero_1
        real, intent (in) :: const 
        type (erosion_output_variables) :: ero_2
        ero_2%sedyld = ero_1%sedyld / const
        ero_2%precip = ero_1%precip / const
        ero_2%surfq = ero_1%surfq / const
        ero_2%peak = ero_1%peak / const
        ero_2%k = ero_1%k / const
        ero_2%s = ero_1%s / const
        ero_2%l = ero_1%l / const
        ero_2%ls = ero_1%ls / const
        ero_2%p = ero_1%p / const
        ero_2%rsd_m = ero_1%rsd_m / const
        ero_2%grcov_frac = ero_1%grcov_frac / const
        ero_2%rsd_covfact = ero_1%rsd_covfact / const
        ero_2%bio_covfact = ero_1%bio_covfact / const
        ero_2%c = ero_1%c / const
      end function ero_divide
      
      end module erosion_module 
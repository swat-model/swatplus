      module output_ls_pesticide_module
    
      implicit none 
      
      type pesticide_balance
        !character(len=10) :: name
        !integer :: num_db
        real :: plant = 0.          !! |kg/ha       |pesticide on plant foliage
        real :: soil = 0.           !! |kg/ha       |pesticide in soil
        real :: sed = 0.            !! |kg/ha       |pesticide loading from HRU sorbed onto sediment
        real :: surq = 0.           !! |kg/ha       |amount of pesticide type lost in surface runoff in HRU
        real :: latq = 0.           !! |kg/ha       |amount of pesticide in lateral flow in HRU
        real :: tileq = 0.          !! |kg/ha       |amount of pesticide in tile flow in HRU
        real :: perc = 0.           !! |kg/ha       |amount of pesticide leached past bottom of soil
        real :: apply_s = 0.        !! |kg/ha       |amount of pesticide applied on soil
        real :: apply_f = 0.        !! |kg/ha       |amount of pesticide applied on foliage
        real :: decay_s = 0.        !! |kg/ha       |amount of pesticide decayed on soil
        real :: decay_f = 0.        !! |kg/ha       |amount of pesticide decayed on foliage
        real :: wash = 0.           !! |kg/ha       |amount of pesticide washed off from plant to soil
        real :: metab_s = 0.        !! |kg/ha       |amount of pesticide metabolized from parent in soil
        real :: metab_f = 0.        !! |kg/ha       |amount of pesticide metabolized from parent on foilage
        real :: pl_uptake = 0.      !! |kg/ha       |amount of pesticide taken up by plants
        real :: in_plant = 0.       !! |kg/ha       |pesticide in plant foliage
      end type pesticide_balance
      type (pesticide_balance) :: pestbz

      type object_pesticide_balance
        type (pesticide_balance), dimension (:), allocatable :: pest
      end type object_pesticide_balance

      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_d
      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_m
      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_y
      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_a
      
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_d
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_m
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_y
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_a
      
      type (object_pesticide_balance) :: bpestb_d
      type (object_pesticide_balance) :: bpestb_m
      type (object_pesticide_balance) :: bpestb_y
      type (object_pesticide_balance) :: bpestb_a
      
      type output_pestbal_header
        character (len=5) :: day =      " jday"
        character (len=6) :: mo =       "   mon"
        character (len=6) :: day_mo =   "   day"
        character (len=6) :: yrc =      "    yr"
        character (len=8) :: isd =          "    unit"
        character (len=8) :: id =           "  gis_id"         
        character (len=16) :: name =        "  name          "
        character (len=16) :: pest =        "  pesticide     "       
        character (len=15) :: on_plant =    "  on_plant_kg/ha"       
        character (len=15) :: soil =        "    soil_kg/ha "
        character (len=15) :: sed =         "    sed_kg/ha  "        
        character (len=15) :: surq =        "   surq_kg/ha  "      
        character (len=15) :: latq =        "   latq_kg/ha  " 
        character (len=15) :: tileq =       "  tileq_kg/ha  "
        character (len=15) :: perc =        "   perc_kg/ha  "  
        character (len=15) :: apply_s =     " apply_s_kg/ha "
        character (len=15) :: apply_f =     " apply_f_kg/ha "
        character (len=15) :: decay_s =     " decay_s_kg/ha "
        character (len=15) :: decay_f =     " decay_f_kg/ha "
        character (len=15) :: wash =        "   wash_kg/ha  "
        character (len=15) :: metab_s =     " metab_s_kg/ha "
        character (len=15) :: metab_f =     " metab_f_kg/ha "
        character (len=15) :: uptake =      "  uptake_kg/ha "
        character (len=15) :: in_plant =    " in_plant_kg/ha "
      end type output_pestbal_header      
      type (output_pestbal_header) :: pestb_hdr
      
      interface operator (+)
        module procedure hruout_pestbal_add
      end interface

      interface operator (*)
        module procedure hruout_pestbal_mult
      end interface
      
      interface operator (/)
        module procedure hruout_pestbal_div
      end interface
      
      interface operator (//)
        module procedure hruout_pestbal_ave
      end interface
        
        
      contains

      function hruout_pestbal_add (hru1, hru2) result (hru3)
        type (pesticide_balance), intent (in) :: hru1
        type (pesticide_balance), intent (in) :: hru2
        type (pesticide_balance) :: hru3
        hru3%plant = hru1%plant + hru2%plant
        hru3%soil = hru1%soil + hru2%soil
        hru3%sed = hru1%sed + hru2%sed
        hru3%surq = hru1%surq + hru2%surq
        hru3%latq = hru1%latq + hru2%latq
        hru3%tileq = hru1%tileq + hru2%tileq
        hru3%perc = hru1%perc + hru2%perc
        hru3%apply_s = hru1%apply_s + hru2%apply_s
        hru3%apply_f = hru1%apply_f + hru2%apply_f
        hru3%decay_s = hru1%decay_s + hru2%decay_s
        hru3%decay_f = hru1%decay_f + hru2%decay_f
        hru3%wash = hru1%wash + hru2%wash
        hru3%metab_s = hru1%metab_s + hru2%metab_s
        hru3%metab_f = hru1%metab_f + hru2%metab_f
        hru3%pl_uptake = hru1%pl_uptake + hru2%pl_uptake
        hru3%in_plant = hru1%in_plant + hru2%in_plant
      end function hruout_pestbal_add

      function hruout_pestbal_mult (hru1,const) result (hru2)
        type (pesticide_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pesticide_balance) :: hru2
        hru2%plant = hru1%plant
        hru2%soil = hru1%soil
        hru2%sed= hru1%sed * const
        hru2%surq = hru1%surq * const
        hru2%latq = hru1%latq * const
        hru2%tileq = hru1%tileq * const
        hru2%perc = hru1%perc * const
        hru2%apply_s = hru1%apply_s * const
        hru2%apply_f = hru1%apply_f * const
        hru2%decay_s = hru1%decay_s * const
        hru2%decay_f = hru1%decay_f * const
        hru2%wash = hru1%wash * const
        hru2%metab_s = hru1%metab_s * const
        hru2%metab_f = hru1%metab_f * const
        hru2%pl_uptake = hru1%pl_uptake * const
        hru2%in_plant = hru1%in_plant
      end function hruout_pestbal_mult
      
      function hruout_pestbal_div (hru1,const) result (hru2)
        type (pesticide_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pesticide_balance) :: hru2
        hru2%plant = hru1%plant
        hru2%soil = hru1%soil
        hru2%sed= hru1%sed / const
        hru2%surq = hru1%surq / const
        hru2%latq = hru1%latq / const
        hru2%tileq = hru1%tileq / const
        hru2%perc = hru1%perc / const
        hru2%apply_s = hru1%apply_s / const
        hru2%apply_f = hru1%apply_f / const
        hru2%decay_s = hru1%decay_s / const
        hru2%decay_f = hru1%decay_f / const
        hru2%wash = hru1%wash / const
        hru2%metab_s = hru1%metab_s / const
        hru2%metab_f = hru1%metab_f / const
        hru2%pl_uptake = hru1%pl_uptake / const
        hru2%in_plant = hru1%in_plant
      end function hruout_pestbal_div
      
      function hruout_pestbal_ave (hru1,const) result (hru2)
        type (pesticide_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pesticide_balance) :: hru2   
        hru2%plant = hru1%plant / const 
        hru2%soil = hru1%soil / const
        hru2%sed= hru1%sed
        hru2%surq = hru1%surq
        hru2%latq = hru1%latq
        hru2%tileq = hru1%tileq
        hru2%perc = hru1%perc
        hru2%apply_s = hru1%apply_s
        hru2%apply_f = hru1%apply_f
        hru2%decay_s = hru1%decay_s
        hru2%decay_f = hru1%decay_f
        hru2%wash = hru1%wash
        hru2%metab_s = hru1%metab_s
        hru2%metab_f = hru1%metab_f
        hru2%pl_uptake = hru1%pl_uptake   
        hru2%in_plant = hru1%in_plant / const 
      end function hruout_pestbal_ave
                            
      end module output_ls_pesticide_module
      module output_ls_salt_module
    
      implicit none 
      
      type salt_balance
        real :: plant = 0.          !! |kg/ha       |salt in plant foliage
        real :: soil = 0.           !! |kg/ha       |salt in soil
        real :: surq = 0.           !! |kg/ha       |amount of pesticide type lost in surface runoff in HRU
        real :: latq = 0.           !! |kg/ha       |amount of pesticide in lateral flow in HRU
        real :: tileq = 0.          !! |kg/ha       |amount of pesticide in tile flow in HRU
        real :: perc = 0.           !! |kg/ha       |amount of pesticide leached past bottom of soil
        real :: irrig = 0.          !! |kg/ha       |amount of pesticide applied on soil
      end type salt_balance
      type (salt_balance) :: saltbz

      type object_salt_balance
        type (salt_balance), dimension (:), allocatable :: salt
      end type object_salt_balance

      type (object_salt_balance), dimension (:), allocatable :: hsaltb_d
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_m
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_y
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_a
      
      type (object_salt_balance), dimension (:), allocatable :: rusaltb_d
      type (object_salt_balance), dimension (:), allocatable :: rusaltb_m
      type (object_salt_balance), dimension (:), allocatable :: rusaltb_y
      type (object_salt_balance), dimension (:), allocatable :: rusaltb_a
      
      type (object_salt_balance) :: bsaltb_d
      type (object_salt_balance) :: bsaltb_m
      type (object_salt_balance) :: bsaltb_y
      type (object_salt_balance) :: bsaltb_a
      
      type output_saltbal_header
        character (len=5) :: day =      " jday"
        character (len=6) :: mo =       "   mon"
        character (len=6) :: day_mo =   "   day"
        character (len=6) :: yrc =      "    yr"
        character (len=8) :: isd =          "    unit"
        character (len=8) :: id =           "  gis_id"         
        character (len=16) :: name =        "  name          "
        character (len=16) :: salt =        "  salt_ion      "
        character (len=12) :: plant =       "  plant_kg/h "       
        character (len=12) :: soil =        "    soil_kg/h"       
        character (len=12) :: surq =        "   surq_kg/h "      
        character (len=12) :: latq =        "   latq_kg/h " 
        character (len=12) :: tileq =       "  tileq_kg/h "
        character (len=12) :: perc =        "   perc_kg/h "  
        character (len=16) :: irrig =       "   irrig_kg/h"
      end type output_saltbal_header      
      type (output_saltbal_header) :: saltb_hdr
      
      interface operator (+)
        module procedure hruout_saltbal_add
      end interface

      interface operator (/)
        module procedure hruout_saltbal_div
      end interface
      
      interface operator (//)
        module procedure hruout_saltbal_ave
      end interface
        
      contains

      function hruout_saltbal_add (hru1, hru2) result (hru3)
        type (salt_balance), intent (in) :: hru1
        type (salt_balance), intent (in) :: hru2
        type (salt_balance) :: hru3
        hru3%plant = hru1%plant + hru2%plant
        hru3%soil = hru1%soil + hru2%soil
        hru3%surq = hru1%surq + hru2%surq
        hru3%latq = hru1%latq + hru2%latq
        hru3%tileq = hru1%tileq + hru2%tileq
        hru3%perc = hru1%perc + hru2%perc
        hru3%irrig = hru1%irrig + hru2%irrig
      end function hruout_saltbal_add

      function hruout_saltbal_div (hru1,const) result (hru2)
        type (salt_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (salt_balance) :: hru2
        hru2%plant = hru1%plant
        hru2%soil = hru1%soil
        hru2%surq = hru1%surq / const
        hru2%latq = hru1%latq / const
        hru2%tileq = hru1%tileq / const
        hru2%perc = hru1%perc / const
        hru2%irrig = hru1%irrig / const
      end function hruout_saltbal_div
      
      function hruout_saltbal_ave (hru1,const) result (hru2)
        type (salt_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (salt_balance) :: hru2   
        hru2%plant = hru1%plant / const 
        hru2%soil = hru1%soil / const
        hru2%surq = hru1%surq
        hru2%latq = hru1%latq
        hru2%tileq = hru1%tileq
        hru2%perc = hru1%perc
        hru2%irrig = hru1%irrig
      end function hruout_saltbal_ave
                            
      end module output_ls_salt_module
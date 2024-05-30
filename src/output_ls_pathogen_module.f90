      module output_ls_pathogen_module
    
      implicit none 
      
      type pathogen_balance
        !character(len=10) :: name
        !integer :: num_db
        real :: plant = 0.          !! |kg/ha       |pathogen on plant foliage
        real :: soil = 0.           !! |kg/ha       |pathogen enrichment ratio
        real :: sed = 0.            !! |kg/ha       |pathogen loading from HRU sorbed onto sediment
        real :: surq = 0.           !! |kg/ha       |amount of pathogen type lost in surface runoff on current day in HRU
        real :: latq = 0.           !! |kg/ha       |amount of pathogen in lateral flow in HRU for the day
        real :: perc1 = 0.          !! |kg/ha       |amount of pathogen leached past first layer
        real :: apply_sol = 0.      !! |kg/ha       |amount of pathogen applied to soil
        real :: apply_plt = 0.      !! |kg/ha       |amount of pathogen applied to plant
        real :: regro = 0.          !! |kg/ha       |amount of pathogen regrowth
        real :: die_off = 0.        !! |kg/ha       |amount of pathogen die-off
        real :: wash = 0.           !! |kg/ha       |amount of pathogen washed off from plant to soil
      end type pathogen_balance
      type (pathogen_balance) :: pathbz

      type object_pathogen_balance
        type (pathogen_balance), dimension (:), allocatable :: path
      end type object_pathogen_balance

      type (object_pathogen_balance), dimension (:), allocatable :: hpath_bal
      type (object_pathogen_balance), dimension (:), allocatable :: hpathb_m
      type (object_pathogen_balance), dimension (:), allocatable :: hpathb_y
      type (object_pathogen_balance), dimension (:), allocatable :: hpathb_a
      
      type (object_pathogen_balance), dimension (:), allocatable :: rupathb_d
      type (object_pathogen_balance), dimension (:), allocatable :: rupathb_m
      type (object_pathogen_balance), dimension (:), allocatable :: rupathb_y
      type (object_pathogen_balance), dimension (:), allocatable :: rupathb_a
      
      type (object_pathogen_balance) :: bpathb_d
      type (object_pathogen_balance) :: bpathb_m
      type (object_pathogen_balance) :: bpathb_y
      type (object_pathogen_balance) :: bpathb_a
      
      type output_pathbal_header
        character (len=5) :: day =      " jday"
        character (len=6) :: mo =       "   mon"
        character (len=6) :: day_mo =   "   day"
        character (len=6) :: yrc =      "    yr"
        character (len=8) :: isd =          "    unit"
        character (len=8) :: id =           "  gis_id"        
        character (len=16) :: name =        "  name          "        
        character (len=14) :: plant =       "  plant_kg/h"
        character (len=12) :: soil =        "  soil_kg/h"
        character (len=12) :: sed =         "   sed_kg/h"        
        character (len=12) :: surq =        "  surq_kg/h"      
        character (len=12) :: latq =        "  latq_kg/h" 
        character (len=12) :: perc =        "  perc_kg/h"   
        character (len=12) :: apply =       " apply_kg/h"
        character (len=12) :: decay =       " decay_kg/h"
      end type output_pathbal_header      
      type (output_pathbal_header) :: pathb_hdr
      
      interface operator (+)
        module procedure hruout_pathbal_add
      end interface

      interface operator (/)
        module procedure hruout_pathbal_div
      end interface
      
      interface operator (//)
        module procedure hruout_pathbal_ave
      end interface
             
      contains

      function hruout_pathbal_add (hru1, hru2) result (hru3)
        type (pathogen_balance), intent (in) :: hru1
        type (pathogen_balance), intent (in) :: hru2
        type (pathogen_balance) :: hru3
        hru3%plant = hru1%plant + hru2%plant
        hru3%soil = hru1%soil + hru2%soil
        hru3%sed = hru1%sed + hru2%sed
        hru3%surq = hru1%surq + hru2%surq
        hru3%perc1 = hru1%perc1 + hru2%perc1
        hru3%apply_sol = hru1%apply_sol + hru2%apply_sol
        hru3%apply_plt = hru1%apply_plt + hru2%apply_plt
        hru3%regro = hru1%regro + hru2%regro
        hru3%die_off = hru1%die_off + hru2%die_off
      end function hruout_pathbal_add

      function hruout_pathbal_div (hru1,const) result (hru2)
        type (pathogen_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pathogen_balance) :: hru2
        hru2%plant = hru1%plant / const
        hru2%soil = hru1%soil / const
        hru2%sed= hru1%sed / const
        hru2%surq = hru1%surq / const
        hru2%perc1 = hru1%perc1 / const
        hru2%apply_sol = hru1%apply_sol / const
        hru2%apply_plt = hru1%apply_plt / const
        hru2%regro = hru1%regro / const
        hru2%die_off = hru1%die_off / const
      end function hruout_pathbal_div
      
      function hruout_pathbal_ave (hru1,const) result (hru2)
        type (pathogen_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pathogen_balance) :: hru2   
        hru2%plant = hru1%plant / const 
        hru2%soil = hru1%soil / const
      end function hruout_pathbal_ave
                            
      end module output_ls_pathogen_module
      module water_body_module
    
      implicit none
    
      !! water body (reservoir, wetland, and channel) output not uncluded in hyd_output object

      type water_body
        real :: area_ha = 0.            !ha         |water body surface area
        real :: precip = 0.             !ha-m       |precip on the water body
        real :: evap = 0.               !ha-m       |evaporation from the water surface
        real :: seep = 0.               !ha-m       |seepage from bottom of water body
        !real :: temp = 0.               !deg C      |ave temperature over time period
        !real :: chla = 0.               !ppm        |ave chlorophyll-a concentration during time period
        !real :: cbod = 0.               !mg O2/L    |ave carbonaceous biochemical oxygen concentration during time period
        !real :: dox = 0.                !mg O2/L    |ave dissolved oxygen concentration during time period
        !real :: secci = 0.              !m          !ave seci depth - water clarity indicator during time period
      end type water_body
      type (water_body) :: wbodz
      
      type (water_body), dimension(:), allocatable, target :: res_wat_d
      type (water_body), dimension(:), allocatable :: res_wat_m
      type (water_body), dimension(:), allocatable :: res_wat_y
      type (water_body), dimension(:), allocatable :: res_wat_a
      type (water_body), dimension(:), allocatable, target :: wet_wat_d
      type (water_body), dimension(:), allocatable :: wet_wat_m
      type (water_body), dimension(:), allocatable :: wet_wat_y
      type (water_body), dimension(:), allocatable :: wet_wat_a
      type (water_body), dimension(:), allocatable :: ch_wat_d
      type (water_body), dimension(:), allocatable :: ch_wat_m
      type (water_body), dimension(:), allocatable :: ch_wat_y
      type (water_body), dimension(:), allocatable :: ch_wat_a
      type (water_body) :: bch_wat_d
      type (water_body) :: bch_wat_m
      type (water_body) :: bch_wat_y
      type (water_body) :: bch_wat_a
      type (water_body) :: bres_wat_d
      type (water_body) :: bres_wat_m
      type (water_body) :: bres_wat_y
      type (water_body) :: bres_wat_a
      type (water_body), pointer :: wbody_wb       !! used for reservoir and wetlands

       interface operator (+)
        module procedure watbod_add
      end interface

      interface operator (/)
        module procedure watbod_div
      end interface   
      
      interface operator (//)
        module procedure watbod_ave
      end interface   
      
      contains
      
     !! routines for water body module
      function watbod_add (wbod1, wbod2) result (wbod3)
        type (water_body), intent (in) :: wbod1
        type (water_body), intent (in) :: wbod2
        type (water_body) :: wbod3
        wbod3%area_ha = wbod1%area_ha + wbod2%area_ha
        wbod3%precip = wbod1%precip + wbod2%precip
        wbod3%evap = wbod1%evap + wbod2%evap        
        wbod3%seep = wbod1%seep + wbod2%seep   
        !wbod3%temp = wbod1%temp + wbod2%temp
        !wbod3%chla = wbod1%chla + wbod2%chla
        !wbod3%cbod = wbod1%cbod + wbod2%cbod        
        !wbod3%dox = wbod1%dox + wbod2%dox  
        !wbod3%secci = wbod1%secci + wbod2%secci
      end function watbod_add
      
      function watbod_div (wbod1,const) result (wbod2)
        type (water_body), intent (in) :: wbod1
        real, intent (in) :: const
        type (water_body) :: wbod2
        wbod2%area_ha = wbod1%area_ha / const
        wbod2%precip = wbod1%precip / const
        wbod2%evap = wbod1%evap / const
        wbod2%seep = wbod1%seep / const
        !wbod2%temp = wbod1%temp
        !wbod2%chla = wbod1%chla
        !wbod2%cbod = wbod1%cbod
        !wbod2%dox = wbod1%dox
        !wbod2%secci = wbod1%secci
      end function watbod_div
            
      function watbod_ave (wbod1,const) result (wbod2)
        type (water_body), intent (in) :: wbod1
        real, intent (in) :: const
        type (water_body) :: wbod2
        wbod2%area_ha = wbod1%area_ha / const
        wbod2%precip = wbod1%precip
        wbod2%evap = wbod1%evap
        wbod2%seep = wbod1%seep
        !wbod2%temp = wbod1%area_ha / const
        !wbod2%chla = wbod1%chla / const
        !wbod2%cbod = wbod1%cbod / const
        !wbod2%dox = wbod1%dox / const
        !wbod2%secci = wbod1%secci / const
      end function watbod_ave
      
      end module water_body_module
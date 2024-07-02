      module landuse_data_module
    
      implicit none
     
      type land_use_management
        character (len=40) :: name          !! name of the land use and management (from hru-data.hru pointer) 
        character (len=40) :: cal_group     !! calibration group (not currently used)
        character (len=40) :: plant_cov     !! plant community initialization (pointer to plants.ini) 
        character (len=40) :: mgt_ops       !! management operations (pointer to management.sch)
        character (len=40) :: cn_lu         !! land use for curve number table (pointer to cntable.lum)
        character (len=40) :: cons_prac     !! conservation practice from table (cons_practice.lum)
        character (len=40) :: urb_lu        !! type of urban land use- ie. residential, industrial, etc (urban.urb)
        character (len=40) :: urb_ro        !! urban runoff model
                                            !! "usgs_reg", simulate using USGS regression eqs
                                            !! "buildup_washoff", simulate using build up/wash off alg       
        character (len=40) :: ovn           !! Manning"s "n" land use type for overland flow (ovn_table.lum)
        character (len=40) :: tiledrain     !! tile drainage (pointer to tiledrain.str
        character (len=40) :: septic        !! septic tanks (pointer to septic.str)
        character (len=40) :: fstrip        !! filter strips (pointer to filterstrip.str)
        character (len=40) :: grassww       !! grass waterways (pointer to grassedww.str)
        character (len=40) :: bmpuser       !! user specified removal efficiency (pointer to bmpuser.str)
      end type land_use_management
      type (land_use_management), dimension (:), allocatable :: lum
      
      type land_use_structures
        integer :: plant_cov = 0
        integer :: mgt_ops = 0
        integer :: cn_lu = 0
        integer :: cons_prac = 0
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: bmpuser = 0
      end type land_use_structures
      type (land_use_structures), dimension (:), allocatable :: lum_str
           
      type curvenumber_table
        character(len=40) :: name                      !name includes abbrev for lu/treatment/condition 
        real, dimension(4) :: cn = (/30.,55.,70.,77./) !curve number
      end type curvenumber_table
      type (curvenumber_table), dimension (:), allocatable :: cn
      
      type land_use_mgt_groups
        integer :: num
        character(len=40), dimension(:), allocatable :: name    !land use groups
      end type
      type (land_use_mgt_groups) :: lum_grp
      
      type conservation_practice_table
        character(len=40) :: name                   !name of conservation practice
        real :: pfac = 1.0                          !usle p factor
        real :: sl_len_mx = 1.0             !m      !maximum slope length
      end type conservation_practice_table
      type (conservation_practice_table), dimension (:), allocatable :: cons_prac
                       
      type overlandflow_n_table
        character(len=40) :: name                   !name of conservation practice
        real :: ovn = 0.5                           !overland flow mannings n - mean
        real :: ovn_min = 0.5                       !overland flow mannings n - min
        real :: ovn_max = 0.5                       !overland flow mannings n - max
      end type overlandflow_n_table
      type (overlandflow_n_table), dimension (:), allocatable :: overland_n
    
      end module landuse_data_module 
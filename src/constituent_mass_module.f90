      module constituent_mass_module

      implicit none

      character(len=16), dimension(:), allocatable :: pest_init_name
      character(len=16), dimension(:), allocatable :: path_init_name
      character(len=16), dimension(:), allocatable :: hmet_init_name
      character(len=16), dimension(:), allocatable :: salt_init_name

      
      type constituents
        integer :: num_tot = 0                                      !number of total constituents simulated
        integer :: num_pests = 0                                    !number of pesticides simulated
        character (len=16), dimension(:), allocatable :: pests      !name of the pesticides- points to pesticide database
        !!need to crosswalk pests to get pest_num for database - use sequential for object
        integer, dimension(:), allocatable :: pest_num              !number of the pesticides- points to pesticide database
        integer :: num_paths = 0                                    !number of pathogens simulated
        character (len=16), dimension(:), allocatable :: paths      !name of the pathogens- points to pathogens database
        integer, dimension(:), allocatable :: path_num              !number of the pathogens- points to pathogens database
        integer :: num_metals = 0                                   !number of heavy metals simulated
        character (len=16), dimension(:), allocatable :: metals     !name of the heavy metals- points to heavy metals database
        integer, dimension(:), allocatable :: metals_num            !number of the heavy metals- points to heavy metals database
        integer :: num_salts = 0                                    !number of salt ions simulated
        character (len=16), dimension(:), allocatable :: salts      !name of the salts - points to salts database
        integer, dimension(:), allocatable :: salts_num             !number of the salts - points to salts database
      end type constituents
      type (constituents) :: cs_db

      type exco_pesticide
        real, dimension (:), allocatable :: pest         !pesticide hydrographs
      end type exco_pesticide
      type (exco_pesticide), dimension (:), allocatable :: exco_pest        !export coefficients
      
      type dr_pesticide
        real, dimension (:), allocatable :: pest         !pesticide delivery
      end type dr_pesticide
      type (dr_pesticide), dimension (:), allocatable :: dr_pest            !delivery ratios
      
      type exco_pathogens
        real, dimension (:), allocatable :: path         !pesticide hydrographs
      end type exco_pathogens
      type (exco_pathogens), dimension (:), allocatable :: exco_path        !export coefficients
      
      type dr_pathogens
        real, dimension (:), allocatable :: path         !pathogen delivery
      end type dr_pathogens
      type (dr_pathogens), dimension (:), allocatable :: dr_path            !delivery ratios
      
      type exco_heavy_metals
        real, dimension (:), allocatable :: hmet                            !heavy metals hydrographs
      end type exco_heavy_metals
      type (exco_heavy_metals), dimension (:), allocatable :: exco_hmet     !export coefficients
      
      type dr_heavy_metals
        real, dimension (:), allocatable :: hmet                            !heavy metals delivery
      end type dr_heavy_metals
      type (dr_heavy_metals), dimension (:), allocatable :: dr_hmet         !delivery ratios
      
      type exco_salts
        real, dimension (:), allocatable :: salt                            !salts hydrographs
      end type exco_salts
      type (exco_salts), dimension (:), allocatable :: exco_salt            !export coefficients
      
      type dr_salts
        real, dimension (:), allocatable :: salt                            !salts delivery
      end type dr_salts
      type (dr_salts), dimension (:), allocatable :: dr_salt                !delivery ratios
      
      type salt_solids_soil
        real, dimension (:), allocatable :: solid                               !salt solid by soil layer
      end type salt_solids_soil
      type (salt_solids_soil), dimension (:), allocatable :: sol_salt_solid     !salt solid by hru

      ! constituent mass - soil, plant, aquifer, channel and reservoir
      type constituent_mass
        real, dimension (:), allocatable :: pest      !kg/ha          |pesticide in soil layer
        real, dimension (:), allocatable :: path      !pathogen hydrographs
        real, dimension (:), allocatable :: hmet      !heavy metal hydrographs
        real, dimension (:), allocatable :: salt      !salt ion hydrographs
        real, dimension (:), allocatable :: salt_min  !salt mineral hydrographs
      end type constituent_mass
      
      ! irrigation water constituent mass - dimensioned by hru
      type (constituent_mass), dimension (:), allocatable :: cs_irr
      
      ! soil constituent mass - dimensioned by hru
      type soil_constituent_mass
        type (constituent_mass), dimension (:), allocatable :: ly
      end type soil_constituent_mass
      type (soil_constituent_mass), dimension (:), allocatable :: cs_soil

      ! plant constituent mass
      type (constituent_mass), dimension (:), allocatable :: cs_pl

      ! aquifer constituent mass
      type (constituent_mass), dimension (:), allocatable :: cs_aqu
      type (constituent_mass), dimension (:), allocatable :: cs_aqu_init

      ! storing water and benthic constituents in channels and reservoirs
      type (constituent_mass), dimension (:), allocatable :: ch_water
      type (constituent_mass), dimension (:), allocatable :: ch_benthic
      type (constituent_mass), dimension (:), allocatable :: res_water
      type (constituent_mass), dimension (:), allocatable :: res_benthic
      type (constituent_mass), dimension (:), allocatable :: ch_water_init
      type (constituent_mass), dimension (:), allocatable :: ch_benthic_init
      type (constituent_mass), dimension (:), allocatable :: res_water_init
      type (constituent_mass), dimension (:), allocatable :: res_benthic_init
      
      ! hydrographs used in command for adding incoming hyds
      type (constituent_mass) :: hcs1, hcs2
      ! set zero constituent hydrograph
      type (constituent_mass) :: hin_csz
      
      ! hydrographs for all constituents - dimension to number of each constituent
      type all_constituent_hydrograph
        type (constituent_mass), dimension (:), allocatable :: hd
        type (constituent_mass) :: hin
        type (constituent_mass) :: hin_sur
        type (constituent_mass) :: hin_lat
        type (constituent_mass) :: hin_til
        type (constituent_mass) :: hin_aqu
        type (constituent_mass), dimension(:), allocatable :: hcsin_d
        type (constituent_mass), dimension(:), allocatable :: hcsin_m
        type (constituent_mass), dimension(:), allocatable :: hcsin_y
        type (constituent_mass), dimension(:), allocatable :: hcsin_a
        type (constituent_mass), dimension(:), allocatable :: hcsout_m
        type (constituent_mass), dimension(:), allocatable :: hcsout_y
        type (constituent_mass), dimension(:), allocatable :: hcsout_a
      end type all_constituent_hydrograph
      type (all_constituent_hydrograph), dimension (:), allocatable :: obcs
      
      ! set zero all constituent hydrograph
      type (all_constituent_hydrograph) :: hcsz
      
      !recall pesticide inputs
      type recall_pesticide_inputs
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (constituent_mass), dimension (:,:), allocatable :: hd_pest
      end type recall_pesticide_inputs
      type (recall_pesticide_inputs), dimension(:),allocatable:: rec_pest
      
      ! intial constituent soil-plant concentrations for hrus and aquifers
      type cs_soil_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: soil    !! ppm or #cfu/m^2      |amount of constituent in soil at start of simulation
        real, dimension (:), allocatable :: plt     !! ppm or #cfu/m^2      |amount of constituent on plant at start of simulation
      end type cs_soil_init_concentrations
      type (cs_soil_init_concentrations), dimension(:), allocatable:: pest_soil_ini
      type (cs_soil_init_concentrations), dimension(:), allocatable:: path_soil_ini
      type (cs_soil_init_concentrations), dimension(:), allocatable:: hmet_soil_ini
      !! first 8 values of soil and plt are salt ion concentrations and next 5 are salt mineral fractions
      type (cs_soil_init_concentrations), dimension(:), allocatable:: salt_soil_ini
      
      ! intial constituent water-benthic concentrations for reservoirs and channels
      type cs_water_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: water   !! ppm or #cfu/m^2      |amount of constituent in water at start of simulation
        real, dimension (:), allocatable :: benthic !! ppm or #cfu/m^2      |amount of constituent in benthic at start of simulation
      end type cs_water_init_concentrations
      type (cs_water_init_concentrations), dimension(:),allocatable:: pest_water_ini
      type (cs_water_init_concentrations), dimension(:),allocatable:: path_water_ini
      type (cs_water_init_concentrations), dimension(:),allocatable:: hmet_water_ini
      type (cs_water_init_concentrations), dimension(:),allocatable:: salt_water_ini

     type constituents_header_in          
        character (len=11) :: day      = "       jday "
        character (len=12) :: mo       = "         mon"
        character (len=12) :: day_mo   = "         day"
        character (len=12) :: yrc      = "          yr"
        character (len=12) :: name     = "         iob"
        character (len=12) :: otype    = "     gis_id "
        character (len=12) :: type     = "        type"
        character (len=12) :: num      = "         num"
        character (len=12) :: obout    = "     obtypin"
        character (len=12) :: obno_out = "  obtyp_noin"
        character (len=12) :: htyp_out = "     htyp_in"
        character (len=12) :: frac     = "     frac_in"
        character (len=12) :: sol      = "      sol_in"
        character (len=12) :: sor      = "      sor_in"
      end type constituents_header_in
      type (constituents_header_in) :: csin_hyd_hdr
          
      type constituents_header_out          
        character (len=11) :: day      = "       jday "
        character (len=12) :: mo       = "         mon"
        character (len=12) :: day_mo   = "         day"
        character (len=12) :: yrc      = "          yr"
        character (len=12) :: name     = "         iob"
        character (len=12) :: otype    = "     gis_id "
        character (len=12) :: type     = "        type"
        character (len=12) :: num      = "         num"
        character (len=12) :: obout    = "    obtypout"
        character (len=12) :: obno_out = " obtyp_noout"
        character (len=12) :: htyp_out = "    htyp_out"
        character (len=12) :: frac     = "    frac_out"
      end type constituents_header_out
      type (constituents_header_out) :: csout_hyd_hdr
      
      type sol_sor
        character (len=12) :: sol =      "     sol_out"
        character (len=12) :: sor =      "     sor_out"
      end type sol_sor
      type (sol_sor), dimension (:), allocatable :: cs_pest_solsor
      type (sol_sor), dimension (:), allocatable :: cs_path_solsor
      type (sol_sor), dimension (:), allocatable :: cs_hmet_solsor
      type (sol_sor), dimension (:), allocatable :: cs_salt_solsor
     
      interface operator (+)
        module procedure hydcsout_add
      end interface
      
      interface operator (*)
        module procedure hydcsout_mult_const
      end interface

      contains
      
      function hydcsout_add (hydcs1, hydcs2) result (hydcs3)
        type (constituent_mass), intent (in) :: hydcs1
        type (constituent_mass), intent (in) :: hydcs2
        type (constituent_mass) :: hydcs3
        integer :: ipest, ipath, ihmet, isalt
        allocate (hydcs3%pest(cs_db%num_pests))
        allocate (hydcs3%path(cs_db%num_paths))
        allocate (hydcs3%hmet(cs_db%num_metals))
        allocate (hydcs3%salt(cs_db%num_salts))

        do ipest = 1, cs_db%num_pests
          hydcs3%pest(ipest) =  hydcs2%pest(ipest) + hydcs1%pest(ipest)
        end do
        do ipath = 1, cs_db%num_paths
          hydcs3%path(ipath) =  hydcs2%path(ipath) + hydcs1%path(ipath)
        end do
        do ihmet = 1, cs_db%num_metals
          hydcs3%hmet(ihmet) =  hydcs2%hmet(ihmet) + hydcs1%hmet(ihmet)
        end do
        do isalt = 1, cs_db%num_salts
          hydcs3%salt(isalt) =  hydcs2%salt(isalt) + hydcs1%salt(isalt)
        end do
      return
      end function hydcsout_add
      
      function hydcsout_mult_const (const, hydcs1) result (hydcs2)
        type (constituent_mass), intent (in) :: hydcs1
        type (constituent_mass) :: hydcs2
        real, intent (in) :: const
        integer :: ipest, ipath, ihmet, isalt
        allocate (hydcs2%pest(cs_db%num_pests))
        allocate (hydcs2%path(cs_db%num_paths))
        allocate (hydcs2%hmet(cs_db%num_metals))
        allocate (hydcs2%salt(cs_db%num_salts))

        do ipest = 1, cs_db%num_pests
          hydcs2%pest(ipest) =  const * hydcs1%pest(ipest)
        end do
        do ipath = 1, cs_db%num_paths
          hydcs2%path(ipath) =  const * hydcs1%path(ipath)
        end do
        do ihmet = 1, cs_db%num_metals
          hydcs2%hmet(ihmet) =  const * hydcs1%hmet(ihmet)
        end do
        do isalt = 1, cs_db%num_salts
          hydcs2%salt(isalt) =  const * hydcs1%salt(isalt)
        end do
        return
      end function hydcsout_mult_const
      
      end module constituent_mass_module
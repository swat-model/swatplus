      subroutine soils_init
      
      use hru_module, only : hru, wfsh, ihru
      use soil_module
      use plant_module
      use maximum_data_module
      use soil_data_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob
      use time_module
      use basin_module
      
      implicit none  
      
      integer :: msoils           !none          !ending of loop
      integer :: isol             !none          |counter
      integer :: mlyr             !none          |max number of soil layers
      integer :: j                !none          |counter
      integer :: nly              !              |end of loop
      integer :: ly               !none          |counter
      
      !!Section 1
      !!this section sets, allocates, and initializes the original soil database
      msoils = Max(0,db_mx%soil)
      allocate (sol(0:msoils))
       
      do isol = 1, msoils
        sol(isol)%s%snam = soildb(isol)%s%snam
        if (soildb(isol)%ly(1)%z > 19.5) then
          sol(isol)%s%nly = soildb(isol)%s%nly + 1    !add 10 mm layer
        else
          sol(isol)%s%nly = soildb(isol)%s%nly
        end if
        sol(isol)%s%hydgrp = soildb(isol)%s%hydgrp
        sol(isol)%s%zmx = soildb(isol)%s%zmx                      
        sol(isol)%s%anion_excl = soildb(isol)%s%anion_excl
        sol(isol)%s%crk = soildb(isol)%s%crk                  
        sol(isol)%s%texture = soildb(isol)%s%texture
        mlyr = sol(isol)%s%nly
        allocate (sol(isol)%ly(mlyr))
        allocate (sol(isol)%phys(mlyr))
        
        !!set first 10 mm layer
        sol(isol)%phys(1)%d = 10.
        sol(isol)%phys(1)%bd = soildb(isol)%ly(1)%bd
        sol(isol)%phys(1)%awc = soildb(isol)%ly(1)%awc
        sol(isol)%phys(1)%k = soildb(isol)%ly(1)%k
        
        sol(isol)%phys(1)%clay = soildb(isol)%ly(1)%clay
        sol(isol)%phys(1)%silt = soildb(isol)%ly(1)%silt
        sol(isol)%phys(1)%sand = soildb(isol)%ly(1)%sand
        sol(isol)%phys(1)%rock = soildb(isol)%ly(1)%rock
        sol(isol)%ly(1)%alb = soildb(isol)%ly(1)%alb
        sol(isol)%ly(1)%usle_k = soildb(isol)%ly(1)%usle_k
        sol(isol)%ly(1)%ec = soildb(isol)%ly(1)%ec
        sol(isol)%ly(1)%cal = soildb(isol)%ly(1)%cal
        sol(isol)%ly(1)%ph = soildb(isol)%ly(1)%ph
        !!set remaining layers
        if (soildb(isol)%ly(1)%z > 19.5) then
          do j = 2, mlyr
            sol(isol)%phys(j)%d = soildb(isol)%ly(j-1)%z
            sol(isol)%phys(j)%bd = soildb(isol)%ly(j-1)%bd
            sol(isol)%phys(j)%awc = soildb(isol)%ly(j-1)%awc
            sol(isol)%phys(j)%k = soildb(isol)%ly(j-1)%k
            sol(isol)%phys(j)%clay = soildb(isol)%ly(j-1)%clay
            sol(isol)%phys(j)%silt = soildb(isol)%ly(j-1)%silt
            sol(isol)%phys(j)%sand = soildb(isol)%ly(j-1)%sand
            sol(isol)%phys(j)%rock = soildb(isol)%ly(j-1)%rock
            sol(isol)%ly(j)%alb = soildb(isol)%ly(j-1)%alb
            sol(isol)%ly(j)%usle_k = soildb(isol)%ly(j-1)%usle_k
            sol(isol)%ly(j)%ec = soildb(isol)%ly(j-1)%ec
            sol(isol)%ly(j)%cal = soildb(isol)%ly(j-1)%cal
            sol(isol)%ly(j)%ph = soildb(isol)%ly(j-1)%ph
          end do
        else
          !!1st layer < 20 mm so dont add 10 mm  layer
          do j = 2, mlyr
            sol(isol)%phys(j)%d = soildb(isol)%ly(j)%z
            sol(isol)%phys(j)%bd = soildb(isol)%ly(j)%bd
            sol(isol)%phys(j)%awc = soildb(isol)%ly(j)%awc
            sol(isol)%phys(j)%k = soildb(isol)%ly(j)%k
            sol(isol)%phys(j)%clay = soildb(isol)%ly(j)%clay
            sol(isol)%phys(j)%silt = soildb(isol)%ly(j)%silt
            sol(isol)%phys(j)%sand = soildb(isol)%ly(j)%sand
            sol(isol)%phys(j)%rock = soildb(isol)%ly(j)%rock
            sol(isol)%ly(1)%alb = soildb(isol)%ly(j)%alb
            sol(isol)%ly(1)%usle_k = soildb(isol)%ly(j)%usle_k
            sol(isol)%ly(j)%ec = soildb(isol)%ly(j)%ec
            sol(isol)%ly(j)%cal = soildb(isol)%ly(j)%cal
            sol(isol)%ly(j)%ph = soildb(isol)%ly(j)%ph
          end do
        end if
      end do
           
      do isol = 1, msoils
        call soil_phys_init(isol)          !! initialize soil physical parameters
        !call soil_nutcarb_init(isol)       !! initialize soil chemical parameters
      end do
      
      !!Section 2
      !!this section sets hru soils to appropriate soil database
      
      do ihru = 1, sp_ob%hru
        !! allocate soil layers
        isol = hru(ihru)%dbs%soil
        wfsh(ihru) = 10. * Exp(6.5309 - 7.32561* sol(isol)%phys(1)%por +    &
      3.809479 * sol(isol)%phys(1)%por**2+0.001583 *                        &
      sol(isol)%phys(1)%clay **2 + 0.000344 * sol(isol)%phys(1)%sand*       &
      sol(isol)%phys(1)%clay - 0.049837 * sol(isol)%phys(1)%por *           &
      sol(isol)%phys(1)%sand + 0.001608*sol(isol)%phys(1)%por ** 2 *        &
      sol(isol)%phys(1)%sand ** 2+0.001602*sol(isol)%phys(1)%por ** 2 *     &
      sol(isol)%phys(1)%clay**2-0.0000136*sol(isol)%phys(1)%sand ** 2       &
      * sol(isol)%phys(1)%clay-0.003479*sol(isol)%phys(1)%clay ** 2 *       &
      sol(isol)%phys(1)%por - 0.000799 * sol(isol)%phys(1)%sand ** 2 *      & 
      sol(isol)%phys(1)%por)
        soil(ihru) = sol(isol)%s
        nly = soil(ihru)%nly
        allocate (soil(ihru)%ly(nly))
        allocate (cs_soil(ihru)%ly(nly))
        allocate (soil(ihru)%phys(nly))
        allocate (soil1(ihru)%sw(nly))
        allocate (soil1(ihru)%cbn(nly))
        allocate (soil1(ihru)%sed(nly))
        allocate (soil1(ihru)%mn(nly))
        allocate (soil1(ihru)%mp(nly))
        allocate (soil1(ihru)%tot(nly))
        allocate (soil1(ihru)%hact(nly))
        allocate (soil1(ihru)%hsta(nly))
        allocate (soil1(ihru)%rsd(nly))
        allocate (soil1(ihru)%str(nly))
        allocate (soil1(ihru)%lig(nly))
        allocate (soil1(ihru)%meta(nly))
        allocate (soil1(ihru)%hs(nly))
        allocate (soil1(ihru)%hp(nly))
        allocate (soil1(ihru)%microb(nly))
        allocate (soil1(ihru)%man(nly))
        allocate (soil1(ihru)%water(nly))

        allocate (soil1_init(ihru)%sw(nly))
        allocate (soil1_init(ihru)%cbn(nly))
        allocate (soil1_init(ihru)%sed(nly))
        allocate (soil1_init(ihru)%mn(nly))
        allocate (soil1_init(ihru)%mp(nly))
        allocate (soil1_init(ihru)%tot(nly))
        allocate (soil1_init(ihru)%hact(nly))
        allocate (soil1_init(ihru)%hsta(nly))
        allocate (soil1_init(ihru)%rsd(nly))
        allocate (soil1_init(ihru)%str(nly))
        allocate (soil1_init(ihru)%lig(nly))
        allocate (soil1_init(ihru)%meta(nly))
        allocate (soil1_init(ihru)%hs(nly))
        allocate (soil1_init(ihru)%hp(nly))
        allocate (soil1_init(ihru)%microb(nly))
        allocate (soil1_init(ihru)%man(nly))
        allocate (soil1_init(ihru)%water(nly))
        
        !! set hru soils to appropriate database soil layers
        do ly = 1, nly
          soil(ihru)%phys(ly) = sol(isol)%phys(ly)
          soil(ihru)%ly(ly) = sol(isol)%ly(ly)
          !! set arrays that are layer and plant dependent - residue and roots
          !allocate (soil(ihru)%ly(ly)%rs(pcom(ihru)%npl))
        end do
        
        !! initialize carbon and nutrient contents for each hru
        call soil_nutcarb_init(isol)
        
      end do
      
      do ihru = 1, sp_ob%hru
        if (pco%snutc == "d" .or. pco%snutc == "m" .or. pco%snutc == "y" .or. pco%snutc == "a") then
          call soil_nutcarb_write
        end if
      end do

      return
      end subroutine soils_init
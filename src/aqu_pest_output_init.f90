      subroutine aqu_pest_output_init
      
      use aqu_pesticide_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob
      
      implicit none      

      integer :: ipest                  !none         |pesticide counter
      integer :: iaq                    !none         |aquifer counter
      
      !! set initial aquifer pesticides at beggining of output for monthly, annual and average annual
      do iaq = 1, sp_ob%aqu
          
        !! zero initial basin pesticides (for printing)
        do ipest = 1, cs_db%num_pests
          baqupst_d%pest(ipest)%stor_init = 0.
          baqupst_m%pest(ipest)%stor_init = 0.
          baqupst_y%pest(ipest)%stor_init = 0.
          baqupst_a%pest(ipest)%stor_init = 0.
        end do
          
        do ipest = 1, cs_db%num_pests
          !! set inital aquifer pesticides (for printing)
          aqupst_d(iaq)%pest(ipest)%stor_init = cs_aqu(iaq)%pest(ipest)
          aqupst_m(iaq)%pest(ipest)%stor_init = cs_aqu(iaq)%pest(ipest)
          aqupst_y(iaq)%pest(ipest)%stor_init = cs_aqu(iaq)%pest(ipest)
          aqupst_a(iaq)%pest(ipest)%stor_init = cs_aqu(iaq)%pest(ipest)
          !! sum initial basin pesticides (for printing)
          baqupst_d%pest(ipest)%stor_init = baqupst_d%pest(ipest)%stor_init + cs_aqu(iaq)%pest(ipest)
          baqupst_m%pest(ipest)%stor_init = baqupst_m%pest(ipest)%stor_init + cs_aqu(iaq)%pest(ipest)
          baqupst_y%pest(ipest)%stor_init = baqupst_y%pest(ipest)%stor_init + cs_aqu(iaq)%pest(ipest)
          baqupst_a%pest(ipest)%stor_init = baqupst_a%pest(ipest)%stor_init + cs_aqu(iaq)%pest(ipest)
        end do

      end do

      return
      end subroutine aqu_pest_output_init
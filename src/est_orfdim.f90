	   subroutine est_orfdim(desdis,dia,cd)

   !!	This program estimates orifice dimensions based on 
   !!	design discharge at different stages
   
       implicit none

	   real, intent(in) :: cd            !          |
       real, intent(in) :: desdis        !          | 
	   real, intent(out) :: dia          !          | 
	   real :: tempvar                   !          |
       real :: pi                        !          |

	   pi = 3.14159
	   tempvar = 0.6 * cd * pi * sqrt(9.81)
	   dia = (4.0 * desdis / tempvar) ** 0.4

	   return
	   end subroutine est_orfdim
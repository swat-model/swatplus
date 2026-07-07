      subroutine caps(file_name)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the input and output names
!!    and converts all capital letters to lowercase letters.

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Len, Index, AdjustL

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      implicit none


      intrinsic Len, Index, AdjustL

      character (len=*) ::  file_name
      character (len=Len(file_name)) ::  temp_name
      character (len=26) :: low_case = "abcdefghijklmnopqrstuvwxyz",      &
                            up_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      integer :: ii, j

      temp_name = ""
      j = 0

      temp_name = file_name

      do ii = 1, Len(file_name)
       j = Index (up_case,file_name(ii:ii))
       if (j /= 0) temp_name(ii:ii) = low_case(j:j)
      end do

      temp_name = AdjustL (temp_name)     !moves leading blanks to right end
      
      file_name = temp_name

      return
      end
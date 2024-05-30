      subroutine define_unit_elements (num_elem, ielem)

      use hydrograph_module, only : elem_cnt, defunit_num
      
      implicit none

      integer, intent (in)  :: num_elem
      integer, intent (out)  :: ielem
      integer :: ii                   !none       |counter
      integer :: ie1                  !none       |beginning of loop
      integer :: ie2                  !none       |ending of loop  
      integer :: ie                   !none       |counter

      !!save the object number of each defining unit
          ielem = 0
          ii = 1
          do while (ii <= num_elem)
            if (ii == num_elem) then
              if (ii == 1) then
                ielem = 1
                exit
              end if
              ie1 = elem_cnt(ii-1)
              ie2 = elem_cnt(ii)
              if (ie2 > 0) then
                ielem = ielem + 1
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                end do
              end if
              ii = ii + 1
            else
              ie1 = elem_cnt(ii)
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                ii = ii + 1   
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                end do
                ii = ii + 2
              end if
            end if
          end do
          allocate (defunit_num(ielem))

          ielem = 0
          ii = 1
          do while (ii <= num_elem)
            if (ii == num_elem) then
              if (ii == 1) then
                defunit_num(1) = elem_cnt(1)
                ielem = 1
                exit
              end if
              ie1 = elem_cnt(ii-1)
              ie2 = elem_cnt(ii)
              if (ie2 > 0) then
                ielem = ielem + 1
                defunit_num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  defunit_num(ielem) = ie
                end do
              end if
              ii = ii + 1
            else
              ie1 = elem_cnt(ii)
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                defunit_num(ielem) = ie1
                ii = ii + 1   
                !ielem = ielem + 1
                !defunit_num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  defunit_num(ielem) = ie
                end do
                ii = ii + 2
              end if
              
            end if
          end do
          deallocate (elem_cnt)

      return
      end subroutine define_unit_elements
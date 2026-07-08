      module name_index_module
      !! Small open-addressing hash map: database name (character) -> integer index.
      !! Built once per database so per-HRU lookups in hru_read become O(1) instead of
      !! an O(N_hru x N_db) linear scan (for_cpstr_eq was a top startup CPU hotspot on
      !! large models, dominated by the per-HRU topography/hydrology name matching).
      !! Preserves first-match semantics: if a name occurs more than once, the lowest
      !! index wins, exactly like the original `do ... if (name==db) exit` scans.
      implicit none
      private
      public :: name_index, nix_build, nix_get

      integer, parameter :: NIX_KEYLEN = 40    !! >= longest db/target name (dbsc fields are len=40)

      type name_index
        integer :: tsize = 0
        character(len=NIX_KEYLEN), allocatable :: keys(:)
        integer, allocatable :: vals(:)        !! 0 = empty slot
      end type name_index

      contains

      pure integer function nix_hash(key, tsize) result(h)
        character(len=*), intent(in) :: key
        integer, intent(in) :: tsize
        integer :: i, n
        integer(kind=8) :: acc
        acc = 1469598103934665603_8            !! FNV-1a offset basis
        n = len_trim(key)
        do i = 1, n
          acc = ieor(acc, int(iachar(key(i:i)), 8))
          acc = acc * 1099511628211_8          !! FNV-1a prime (wraps; fine for hashing)
        end do
        h = int(iand(acc, int(huge(0), 8)), kind=4)
        h = modulo(h, tsize) + 1
      end function nix_hash

      subroutine nix_build(idx, names, n)
        type(name_index), intent(out) :: idx
        integer, intent(in) :: n
        character(len=*), intent(in) :: names(:)
        integer :: i, slot
        character(len=NIX_KEYLEN) :: key
        logical :: dup
        idx%tsize = max(16, 2 * max(n, 1))     !! load factor <= 0.5
        allocate (idx%keys(idx%tsize)); idx%keys = ''
        allocate (idx%vals(idx%tsize)); idx%vals = 0
        do i = 1, n
          key = names(i)
          slot = nix_hash(key, idx%tsize)
          dup = .false.
          do
            if (idx%vals(slot) == 0) exit
            if (idx%keys(slot) == key) then    !! keep first occurrence (first-match)
              dup = .true.; exit
            end if
            slot = slot + 1
            if (slot > idx%tsize) slot = 1
          end do
          if (.not. dup) then
            idx%keys(slot) = key
            idx%vals(slot) = i
          end if
        end do
      end subroutine nix_build

      integer function nix_get(idx, target) result(res)
        type(name_index), intent(in) :: idx
        character(len=*), intent(in) :: target
        character(len=NIX_KEYLEN) :: key
        integer :: slot, probes
        res = 0
        if (idx%tsize == 0) return
        key = target
        slot = nix_hash(key, idx%tsize)
        probes = 0
        do
          if (idx%vals(slot) == 0) return      !! empty slot -> not present
          if (idx%keys(slot) == key) then
            res = idx%vals(slot); return
          end if
          slot = slot + 1
          if (slot > idx%tsize) slot = 1
          probes = probes + 1
          if (probes > idx%tsize) return       !! safety (full table)
        end do
      end function nix_get

      end module name_index_module

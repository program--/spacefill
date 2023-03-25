module spacefill_hilbert
   implicit none
   private

   public :: hc, ihc
contains
   elemental integer function hc(n, x, y)
      integer, intent(in) :: n
      integer, value :: x, y
      integer :: rx, ry, s
      hc = 0
      s = n / 2
      do while (s > 0)
         rx = transfer(iand(x, s) > 0, rx)
         ry = transfer(iand(y, s) > 0, ry)
         hc  = hc + (s * s * ieor(3 * rx, ry))
         call rotate(n, x, y, rx, ry)
         s  = s / 2
      end do
   end function

   elemental subroutine ihc(n, h, x, y)
      integer, intent(in) :: n
      integer, value :: h
      integer, intent(out) :: x, y
      integer :: rx, ry, s, t
      x = 0
      y = 0
      s = 1
      t = h
      do while (s < n)
         rx = iand(1, t/2)
         ry = iand(1, ieor(t, rx))
         call rotate(s, x, y, rx, ry)
         x = x + s * rx
         y = y + s * ry
         t = t / 4
         s = s * 2
      end do
   end subroutine

   elemental subroutine rotate(n, x, y, rx, ry)
      integer, intent(in) :: n, rx, ry
      integer, intent(inout) :: x, y
      integer :: temp
      if (ry == 0) then
         if (rx == 1) then
            x = n - 1 - x
            y = n - 1 - y
         end if

         temp = x
         x    = y
         y    = temp
      end if
   end subroutine
end module

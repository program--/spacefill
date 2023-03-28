module spacefill_morton
   implicit none
   private

   public :: mc, imc
contains
   pure elemental integer function mc(x, y)
      integer, value :: x, y
      integer :: i
      mc = 0
      do i = 0, bit_size(x)
         block
            integer :: mi, mx, my
            mi = lshift(1, i)
            mx = iand(x, mi)
            my = iand(y, mi)
            mc = ior(mc, lshift(mx, i))
            mc = ior(mc, lshift(my, i + 1))
         end block
      end do
   end function

   pure elemental subroutine imc(m, x, y)
      integer, value :: m
      integer, intent(out) :: x, y
      x = mc_extract(m)
      y = mc_extract(rshift(m, 1))
   end subroutine

   pure elemental integer function mc_extract(m)
      integer, value :: m
      m = iand(m, z'55555555')
      m = iand(ior(m, rshift(m, 1)), z'33333333')
      m = iand(ior(m, rshift(m, 2)), z'0F0F0F0F')
      m = iand(ior(m, rshift(m, 4)), z'00FF00FF')
      m = iand(ior(m, rshift(m, 8)), z'0000FFFF')
      mc_extract = m
   end function
end module

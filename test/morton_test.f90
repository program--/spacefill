module morton_test
   use spacefill_morton, only: mc, imc
   use veggies, only: result_t, test_item_t, assert_equals, describe, it
   implicit none
   private
   public :: test_morton
contains
   type(test_item_t)function test_morton() result(tests)
      tests = describe("Morton Curve", [&
         it("returns z-order indices for given coordinates", check_mc), &
         it("returns coordinates given morton indices", check_imc) &
         ])
   end function

   type(result_t) pure function check_mc()
      associate(&
         x => [0, 0, 0,  0, 1, 1, 1,  1, 2, 2,  2,  2, 3, 3,  3,  3, 19], &
         y => [0, 1, 2,  3, 0, 1, 2,  3, 0, 1,  2,  3, 0, 1,  2,  3, 47], &
         e => [0, 2, 8, 10, 1, 3, 9, 11, 4, 6, 12, 14, 5, 7, 13, 15, 2479]  &
         )
         check_mc = assert_equals(e, mc(x, y))
      end associate
   end function

   type(result_t) pure function check_imc()
      integer :: x, y
      associate(m => 2479, ex => 19, ey => 47)
         call imc(m, x, y)
         check_imc = assert_equals([ex, ey], [x, y])
      end associate
   end function
end module

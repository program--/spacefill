module hilbert_test
   use spacefill_hilbert, only: hc, ihc
   use veggies, only: result_t, test_item_t, assert_equals, describe, it
   implicit none
   private
   public :: test_hilbert
contains
   function test_hilbert() result(tests)
      type(test_item_t) :: tests
      tests = describe("hc", [&
         it( "returns hilbert indices for given coordinates", check_hc), &
         it( "returns coordinates given hilbert indices", check_ihc) &
         ])
   end function

   type(result_t) pure function check_hc()
      associate(n => 1, x => [0, 0, 1, 1], y => [0, 1, 1, 0], e => [0, 1, 2, 3])
         check_hc = assert_equals(e, hc(lshift(1, n), x, y))
      end associate

      associate(&
         n => 2, &
         x => [0, 0, 0, 0, 1, 1, 1, 1,  2,  2, 2, 2,  3,  3,  3,  3], &
         y => [0, 1, 2, 3, 0, 1, 2, 3,  0,  1, 2, 3,  0,  1,  2,  3], &
         e => [0, 3, 4, 5, 1, 2, 7, 6, 14, 13, 8, 9, 15, 12, 11, 10]  &
         )

         check_hc = check_hc .and. assert_equals(e, hc(lshift(1, n), x, y))

      end associate
   end function

   type(result_t) pure function check_ihc()
      integer, dimension(:), allocatable :: x, y
      associate(n => 1, h => [0, 1, 2, 3], ex => [0, 0, 1, 1], ey => [0, 1, 1, 0])
         allocate(x(4), y(4))
         call ihc(lshift(1, n), h, x, y)
         check_ihc = assert_equals(ex, x) .and. assert_equals(ey, y)
      end associate


      associate(&
         n => 2, &
         h  => [0, 3, 4, 5, 1, 2, 7, 6, 14, 13, 8, 9, 15, 12, 11, 10], &
         ex => [0, 0, 0, 0, 1, 1, 1, 1,  2,  2, 2, 2,  3,  3,  3,  3], &
         ey => [0, 1, 2, 3, 0, 1, 2, 3,  0,  1, 2, 3,  0,  1,  2,  3]  &
         )

         if (allocated(x)) deallocate(x)
         if (allocated(y)) deallocate(y)
         allocate(x(16), y(16))
         call ihc(lshift(1, n), h, x, y)
         check_ihc = check_ihc .and. assert_equals(ex, x) .and. assert_equals(ey, y)

      end associate
   end function
end module

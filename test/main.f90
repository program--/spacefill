program main
   implicit none

   if (.not.run()) stop 1
contains
   function run() result(passed)
      use hilbert_test, only: test_hilbert
      use morton_test, only: test_morton
      use veggies, only: test_item_t, test_that, run_tests

      logical :: passed

      type(test_item_t) :: tests
      type(test_item_t) :: individual_tests(2)

      individual_tests(1) = test_hilbert()
      individual_tests(2) = test_morton()
      tests = test_that(individual_tests)

      passed = run_tests(tests)
   end function
end program

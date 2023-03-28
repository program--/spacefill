# spacefill

A [space-filling curve](https://en.wikipedia.org/wiki/Space-filling_curve) indexing library. Maps coordinates to pseudo-SFCs indices.

## Tooling

spacefill is implemented using features found in **Fortran 2018** with the **GNU Fortran** (gfortran) compiler.

Dependencies and program management require [fpm](https://fpm.fortran-lang.org/).

Unit testing is managed through [veggies](https://gitlab.com/everythingfunctional/veggies). Drivers and specific tests are found in the `test/` directory.

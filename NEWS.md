# swmmr 0.8.1.9000

## Bug fixes

* `read_rpt` result contains a tibble containing analysis info (also for errors)

* `subcatchments_to_sf` returned an error if a geometry was insufficient defined (3 points are now at least required)

* LID performance summary from `*.rpt` file is now correctly read (@maltehenrichs)

* `write_inp` and `read_inp` are more robust.

## Internal

* Cleaned code basis (@hsonne)

# swmmr 0.8.1

## Bug fixes

* `read_out` correctly reads and assigns multiple objects instead of shuffling (thanks to Benjamin and Christian for spotting this bug)

# swmmr 0.8.0

## Breaking changes

* `read_inp` sections are now all in lower letters.

## New features

* vignettes added

* LID report file reader `read_lid_rpt`

* `*.inp` and `*.rpt` files can be read with different locales (e.g. encodings).

* Read `*.rpt` files with `read_rpt` and get direct access to simulation summaries.

* Convert swmm objects to simple features (sf) with `*_to_sf`.

* `read_inp` returns now an object of class `inp`.

## Bug fixes

# swmmr 0.7.0

* Added a `NEWS.md` file to track changes to the package.




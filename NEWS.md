# swmmr 0.9.0

## Breaking changes

* Mac OS and Linux users only: The default path for swmm exec is expected to be "/usr/local/bin" or "/usr/bin"

## New features

* `autoplot` and `summary`generics added

* `swmmr` detects and assigns the latest swmm5 version if multiple swmm versions are installed (Windows only) (#18)

* `inp_to_sf` gets new paramter `remove_invalid` to increase conversion performance in case all geometries have already been checked beforehand.

* `read_rpt` result contains a tibble containing analysis info (also for errors) (#26)

## Bug fixes

* `read_rpt` now respects all report sections (#21)

* `subcatchments_to_sf` returned an error if a geometry was insufficient defined (3 points are now at least required)

* LID performance summary from `*.rpt` file is now correctly read (@maltehenrichs)

* `write_inp` and `read_inp` are more robust.

## Internal

* The path to swmm5 exec is now stored as package option `swmmr.exec` when the package gets loaded

* Substantially cleaned code basis (@hsonne)

* `inp_to_sf` to convert swmm sections to sf geometries is significantly faster

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




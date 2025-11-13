# Wrapper biomass function for packages medfate and medfateland

Wrapper function to be used to calculate tree biomass for packages
medfate and medfateland.

## Usage

``` r
IFNbiomass_medfate(
  x,
  SpParams,
  area = NA,
  fraction = "total",
  level = "cohort"
)
```

## Arguments

- x:

  Data frame of tree data or `forest` object from package medfate.

- SpParams:

  Data frame of species parameters suitable for medfate package (not
  used).

- area:

  Either 'Atlantic' or 'Mediterranean' to specify allometric equations
  specific to the area (for Pinus pinaster)

- fraction:

  A string, either "total" (for total biomass), "stem" (for stem
  biomass), "branches" (for branch biomass), "aboveground" (for
  aboveground biomass) or "belowground" (for belowground biomass).

- level:

  A string, either "cohort" (for tree cohort-level biomass) or "stand"
  (for stand-level biomass).

## Value

A vector of biomass of each tree cohort (in Mg/ha of dry weight) to be
used in medfate or medfateland packages.

## Details

Values for parameter `x` will be supplied by the calling function (e.g.
`modify_forest_structure`). Values for parameters `area`, `fraction` and
`level` (if they defaults need to be changed) should be supplied using
as a list to parameter `biomass_arguments`.

## See also

[`IFNbiomass`](https://emf-creaf.github.io/IFNallometry/reference/IFNbiomass.md)

## Examples

``` r
if(require("medfate")) {
  IFNbiomass_medfate(exampleforest, SpParamsMED,
                     fraction = "aboveground",
                     level = "stand")
}
#> [1] 142.1382
```

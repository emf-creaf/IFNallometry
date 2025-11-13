# Wrapper biomass function for package forestindicators

Wrapper biomass function for package forestindicators

## Usage

``` r
IFNbiomass_forestindicators(x, area = NA, fraction = "total", ...)
```

## Arguments

- x:

  A data frame corresponding to plant_dynamic_input in package
  forestindicators

- area:

  Either 'Atlantic' or 'Mediterranean' to specify allometric equations
  specific to the area (for Pinus pinaster)

- fraction:

  A string, either "total" (for total biomass), "stem" (for stem
  biomass), "branches" (for branch biomass), "aboveground" (for
  aboveground biomass) or "belowground" (for belowground biomass).

- ...:

  Parameters not used

## Value

A vector of biomass of each tree cohort (in Mg/ha of dry weight)

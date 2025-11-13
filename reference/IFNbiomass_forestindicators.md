# Wrapper biomass function for package forestindicators

Wrapper biomass function for package forestindicators

## Usage

``` r
IFNbiomass_forestindicators(
  x,
  area = NA,
  fraction = "total",
  as.CO2 = FALSE,
  ...
)
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

- as.CO2:

  Flag to indicate output as Mg of CO2 / ha instead of Mg of dry weight
  / ha. Percentage of carbon per dry weight biomass by species are taken
  from Montero et al. (2005) (in turn, from Ibáñez et al. 2002).

- ...:

  Parameters not used

## Value

A vector of biomass of each tree cohort (in Mg/ha of CO2 or dry weight)


<!-- README.md is generated from README.Rmd. Please edit that file -->

# IFNallometry

<!-- badges: start -->

[![R-CMD-check](https://github.com/emf-creaf/IFNallometry/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emf-creaf/IFNallometry/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of IFNallometry is to provide routines for calculating volume
and biomass for forest stands using allometries developed from Spanish
Forest Inventory (IFN) plot data.

## Installation

You can install the development version of IFNallometry from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emf-creaf/IFNallometry")
```

## Example

Here is an example data set of forest plot input to the package:

``` r
library(IFNallometry)
data("exampleTreeData")
head(exampleTreeData)
#>        ID Species        N  DBH   H
#> 344 80001      22 31.83099 18.9 7.5
#> 345 80001      22 14.14711 29.2 7.5
#> 346 80001      22 14.14711 23.9 9.5
#> 347 80001      22 14.14711 23.7 7.0
#> 348 80001      22 14.14711 32.7 8.0
#> 349 80001      22 31.83099 13.7 8.0
```

And here is the result of calculating timber volumes, using plot ID to
extract the Spanish province:

``` r
example_volumes <- IFNvolume(exampleTreeData, provinceFromID = TRUE)
head(example_volumes)
#>      ID Species           Name FC      VCC      VSC          VLE       IAVC
#> 1 80001      22 Pinus uncinata  1 3.602559 2.860209 -0.035419361 0.12176755
#> 2 80001      22 Pinus uncinata  1 3.229656 2.690636  0.033717806 0.07381033
#> 3 80001      22 Pinus uncinata  1 2.805269 2.321029  0.018885424 0.06887026
#> 4 80001      22 Pinus uncinata  1 2.150192 1.750104 -0.001320525 0.06097990
#> 5 80001      22 Pinus uncinata  1 4.176124 3.514192  0.071731231 0.08434162
#> 6 80001      22 Pinus uncinata  1 2.441344 1.846656 -0.061216067 0.10679360
```

## Documentation

So far, the only documentation provided is the one included for package
functions.

## Companion R packages

The development of **IFNallometries** is intended to complement packages
[**medfate**](https://emf-creaf.github.io/medfate) and
[**medfateland**](https://emf-creaf.github.io/medfateland) but it can be
used independently.

## Authorship

This R package is developed and maintained by the [*Ecosystem Modelling
Facility*](https://emf.creaf.cat) unit at
[*CREAF*](https://www.creaf.cat/) (in Spain), in close collaboration
with researchers from [*CTFC*](https://www.ctfc.cat/) (in Spain).

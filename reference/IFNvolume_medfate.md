# Wrapper tree volume function for packages medfate and medfateland

Wrapper function to be used to calculate timber volumes with packages
medfate and medfateland.

## Usage

``` r
IFNvolume_medfate(x, SpParams, province, min_dbh = 7.5, level = "cohort")
```

## Arguments

- x:

  Data frame of tree data or `forest` object from package medfate.

- SpParams:

  Data frame of species parameters suitable for medfate package (not
  used).

- province:

  Spanish province code.

- min_dbh:

  Minimum diameter at breast height (in cm) for timber estimation.

- level:

  A string, either "cohort" (for tree cohort-level volume) or "stand"
  (for stand-level volume).

## Value

A vector of timber volumes per tree cohort (in m3/ha) to be used in
medfateland simulations.

## Details

Values for parameters `x` and `SpParams` will be supplied by the
simulation function (e.g. `fordyn_scenario`) during calculations. Values
for parameters `province`, `min_dbh` and `level` should be supplied
using as a list to parameter `volume_arguments`. See documentation of
package medfateland.

## See also

[`IFNvolume`](https://emf-creaf.github.io/IFNallometry/reference/IFNvolume.md)

## Examples

``` r
if(require("medfate")) {
  IFNvolume_medfate(exampleforest, SpParamsMED,
                    province = "8")
}
#> [1] 55.08258 24.08554
```
